#lang racket/base

(require ffi/cvector
         racket/class
         racket/contract
         (only-in racket/draw dc<%>)
         (only-in racket/draw/private/local
                  flush-cr
                  in-cairo-context
                  install-color)
         racket/flonum
         racket/string
         "font.rkt"
         "unsafe/cairo.rkt"
         "unsafe/pango.rkt")

(provide (contract-out
          (struct line-extents
            ([em-ascent (>=/c 0)]
             [em-descent (>=/c 0)]
             [advance-width real?]
             [bearing-left real?]
             [bearing-bottom real?]
             [ink-width (>=/c 0)]
             [ink-height (>=/c 0)]
             [italic-correction real?]
             [math-script-kerns math-script-kerns?]))
          [line-extents-em-height (-> line-extents? (>=/c 0))]
          [line-extents-bearing-right (-> line-extents? real?)]
          [line-extents-bearing-top (-> line-extents? real?)]
          [line-extents-ink-ascent (-> line-extents? real?)]
          [line-extents-ink-descent (-> line-extents? real?)]

          [font-feature-settings/c flat-contract?]
          [measure-text-line (->* [(is-a?/c dc<%>) font? string?]
                                  [#:features font-feature-settings/c
                                   #:fallback? any/c]
                                  line-extents?)]
          [draw-text-line (->* [(is-a?/c dc<%>) font? string?]
                               [#:x real? #:y real?
                                #:features font-feature-settings/c
                                #:fallback? any/c]
                               void?)]))

;; -----------------------------------------------------------------------------
;; line-extents

(struct line-extents
  (em-ascent      ; logical ascent above the baseline, depends only on the font
   em-descent     ; logical descent below the baseline, depends only on the font
   advance-width  ; logical distance to advance to the right after drawing these glyphs

   bearing-left   ; distance from the origin to the leftmost part of the glyphs as drawn
   bearing-bottom ; distance from the baseline to the bottommost part of the glyphs as drawn
                  ;   (negative if the glyph has a descender)
   ink-width      ; width of the glyphs as drawn
   ink-height     ; height of the glyphs as drawn

   italic-correction
   math-script-kerns)
  #:transparent)

(define (line-extents-em-height le)
  (+ (line-extents-em-ascent le)
     (line-extents-em-descent le)))

(define (line-extents-bearing-right le)
  (- (line-extents-advance-width le)
     (line-extents-bearing-left le)
     (line-extents-ink-width le)))

(define (line-extents-bearing-top le)
  (- (line-extents-em-ascent le)
     (line-extents-bearing-bottom le)
     (line-extents-ink-height le)))

(define (line-extents-ink-ascent le)
  (+ (line-extents-bearing-bottom le)
     (line-extents-ink-height le)))

(define (line-extents-ink-descent le)
  (- (line-extents-bearing-bottom le)))

;; -----------------------------------------------------------------------------
;; rendering

(define font-feature-settings/c
  (and/c hash-equal?
         hash-strong?
         (hash/c (and/c string? #px"^[ !#-~]{4}$")
                 exact-nonnegative-integer?
                 #:immutable #t)))

;; FIXME: cache
(define (make-pango-attrs #:fallback? fallback?
                          #:features features)
  (define attrs (pango_attr_list_new))
  (pango_attr_list_insert attrs (pango_attr_fallback_new fallback?))
  (unless (hash-empty? features)
    (define css-features
      (for/list ([(tag val) (in-immutable-hash features)])
        (format "\"~a\" ~a" tag val)))
    (pango_attr_list_insert attrs (pango_attr_font_features_new
                                   (string-join css-features ","))))
  attrs)

(define (with-pango-layout-line dc font str
          #:fallback? fallback?
          #:features features
          #:before [before-proc void]
          body-proc
          #:after [after-proc void])
  (define ctx (make-pango-context (font-map-pango-font-map (font-font-map font))))
  (define desc (font-pango-description font))
  (define attrs (make-pango-attrs #:fallback? fallback? #:features features))
  (send dc in-cairo-context
        (λ (cr)
          (before-proc cr)

          (pango_cairo_update_context cr ctx)
          (define layout (pango_layout_new ctx))
          (pango_layout_set_single_paragraph_mode layout #t)
          (pango_layout_set_font_description layout desc)
          (pango_layout_set_attributes layout attrs)
          (pango_layout_set_text layout str)

          (begin0
            (body-proc cr (pango_layout_get_line_readonly layout 0))

            (g_object_unref layout)
            (after-proc cr)))))

(define (measure-text-line dc font str
                           #:fallback? [fallback? #f]
                           #:features [features (hash)])
  (define-values [ink logical first-glyph last-glyph]
    (with-pango-layout-line dc font str #:fallback? fallback? #:features features
      (λ (cr line)
        (define-values [ink logical] (pango_layout_line_get_extents line))
        (define-values [first-glyph last-glyph]
          (cond
            [(PangoLayoutLine-runs line)
             (define first-glyphs (PangoGlyphString-glyphs (PangoGlyphItem-glyphs (PangoLayoutLine-first-run line))))
             (define last-glyphs (PangoGlyphString-glyphs (PangoGlyphItem-glyphs (PangoLayoutLine-last-run line))))
             (values (PangoGlyphInfo-glyph (cvector-ref first-glyphs 0))
                     (PangoGlyphInfo-glyph (cvector-ref last-glyphs (sub1 (cvector-length last-glyphs)))))]
            [else
             (values #f #f)]))
        (values ink logical first-glyph last-glyph))))

  (line-extents
   (fl- (pango->px (PangoRectangle-y logical)))
   (fl+ (pango->px (PangoRectangle-y logical))
        (pango->px (PangoRectangle-height logical)))
   (pango->px (PangoRectangle-width logical))
   (pango->px (PangoRectangle-x ink))
   (fl- (fl+ (pango->px (PangoRectangle-y ink))
             (pango->px (PangoRectangle-height ink))))
   (pango->px (PangoRectangle-width ink))
   (pango->px (PangoRectangle-height ink))
   (if last-glyph (font-glyph-italic-correction font last-glyph) 0)
   (cond
     [(not first-glyph)
      no-math-script-kerns]
     [(eqv? first-glyph last-glyph)
      (font-glyph-math-script-kerns font first-glyph)]
     [else
      (h-append-math-script-kerns (font-glyph-math-script-kerns font first-glyph)
                                  (font-glyph-math-script-kerns font last-glyph))])))

(define (draw-text-line dc font str #:x [x 0] #:y [y 0]
                        #:fallback? [fallback? #f]
                        #:features [features (hash)])
  (define text-color (send dc get-text-foreground))
  (define alpha (send dc get-alpha))
  (with-pango-layout-line dc font str #:fallback? fallback? #:features features
    #:before (λ (cr)
               (send dc install-color cr text-color alpha #f)
               (cairo_new_path cr)
               (cairo_move_to cr x y))
    pango_cairo_show_layout_line
    #:after (λ (cr) (send dc flush-cr))))
