#lang racket/base

(require ffi/cvector
         ffi/unsafe/atomic
         racket/contract
         racket/flonum
         racket/format
         racket/match
         (only-in racket/unsafe/ops unsafe-vector*->immutable-vector!)
         threading
         "../util/print.rkt"
         "unsafe/cairo.rkt"
         "unsafe/harfbuzz.rkt"
         "unsafe/pango.rkt")

(provide (contract-out
          [font-family/c flat-contract?]
          [font-style/c flat-contract?]
          [font-weight/c flat-contract?]
          [font-size/c flat-contract?]

          [normalized-font-weight/c flat-contract?]
          [normalize-font-weight (-> font-weight/c normalized-font-weight/c)]
          [prettify-font-weight (-> normalized-font-weight/c font-weight/c)]

          [font-description? predicate/c]
          [font-description-family (-> font-description? (or/c font-family/c #f))]
          [font-description-style (-> font-description? (or/c font-style/c #f))]
          [font-description-weight (-> font-description? (or/c normalized-font-weight/c #f))]
          [font-description-size (-> font-description? (or/c font-size/c #f))]
          [make-font-description (->* [] [#:family (or/c font-family/c #f)
                                          #:style (or/c font-style/c #f)
                                          #:weight (or/c font-weight/c #f)
                                          #:size (or/c font-size/c #f)
                                          #:cache (or/c font-description-cache? #f)]
                                      font-description?)]
          [make-partial-font-description (->* [] [#:family (or/c font-family/c #f)
                                                  #:style (or/c font-style/c #f)
                                                  #:weight (or/c font-weight/c #f)
                                                  #:size (or/c font-size/c #f)
                                                  #:cache (or/c font-description-cache? #f)]
                                              font-description?)]
          [copy-font-description (->* [font-description?]
                                      [#:family (or/c font-family/c #f)
                                       #:style (or/c font-style/c #f)
                                       #:weight (or/c font-weight/c #f)
                                       #:size (or/c font-size/c #f)
                                       #:cache (or/c font-description-cache? #f)]
                                      font-description?)]
          [combine-font-descriptions (-> font-description? ... font-description?)]

          [font-description-cache? predicate/c]
          [make-font-description-cache (-> font-description-cache?)]
          [current-font-description-cache (parameter/c (or/c font-description-cache? #f))]

          [font-map? predicate/c]
          [make-font-map (-> font-map?)]
          [current-font-map (parameter/c font-map?)]
          [resolve-font-description (->* [font-description?]
                                         [#:font-map font-map?
                                          #:fail failure-result/c]
                                         any)]

          [font? predicate/c]
          [font-font-map (-> font? font-map?)]
          [font-describe (-> font? font-description?)]
          [math-font? predicate/c]
          [font-math-script-metrics (-> font? math-script-metrics?)]

          [missing-glyph-error (-> symbol? font? char? none/c)]
          [glyph-id? predicate/c]
          [font-nominal-glyph-id (->* [font? char?] [#:missing failure-result/c] glyph-id?)]
          [font-glyph-italic-correction (-> font? glyph-id? real?)]
          [font-glyph-math-script-kerns (-> font? glyph-id? math-script-kerns?)]

          (struct math-script-metrics
            ([depth-scales (vectorof (real-in 0 1) #:immutable #t)]
             [space-after real?]
             [sub-shift-down real?]
             [sub-top-max real?]
             [sub-baseline-drop-min real?]
             [sup-shift-up real?]
             [sup-shift-up-cramped real?]
             [sup-bottom-min real?]
             [sup-baseline-drop-max real?]
             [sub-sup-gap-min real?]
             [sup-bottom-max-with-sub real?]))
          [math-script-depth? predicate/c]
          [math-script-metrics-depth-scale (-> math-script-metrics?
                                               math-script-depth?
                                               (real-in 0 1))]

          (struct math-script-kerns
            ([top-left (or/c math-script-kern? #f)]
             [top-right (or/c math-script-kern? #f)]
             [bottom-left (or/c math-script-kern? #f)]
             [bottom-right (or/c math-script-kern? #f)]))
          [no-math-script-kerns math-script-kerns?]
          [any-math-script-kerns? (-> math-script-kerns? boolean?)]
          [make-math-script-kerns (->* [] [#:top-left (or/c math-script-kern? #f)
                                           #:top-right (or/c math-script-kern? #f)
                                           #:bottom-left (or/c math-script-kern? #f)
                                           #:bottom-right (or/c math-script-kern? #f)]
                                       math-script-kerns?)]
          [copy-math-script-kerns (->* [math-script-kerns?]
                                       [#:top-left (or/c math-script-kern? #f)
                                        #:top-right (or/c math-script-kern? #f)
                                        #:bottom-left (or/c math-script-kern? #f)
                                        #:bottom-right (or/c math-script-kern? #f)]
                                       math-script-kerns?)]
          [scale-math-script-kerns (-> math-script-kerns? real? real? math-script-kerns?)]
          [h-append-math-script-kerns (-> math-script-kerns? math-script-kerns? math-script-kerns?)]

          [math-script-kern? predicate/c]
          [math-script-kern-correction-heights (-> math-script-kern? (vectorof real? #:immutable #t))]
          [math-script-kern-kern-values (-> math-script-kern? (and/c (vectorof real? #:immutable #t)
                                                                     (property/c vector-length (>=/c 1))))]
          [math-script-kern-value (-> (or/c math-script-kern? #f) real? real?)]
          [scale-math-script-kern (-> math-script-kern? real? real? math-script-kern?)])

         (protect-out
          make-pango-context
          font-map-pango-font-map

          font-pango-description
          font-hb-font))

;; -----------------------------------------------------------------------------
;; font maps and font descriptions

(struct font-description-cache (hash))

;; TODO: Cache eviction?
(define (make-font-description-cache)
  (font-description-cache (make-hash)))

(define current-font-description-cache (make-parameter (make-font-description-cache)))

(define font-weight-sym-mapping
  (hasheq 'thin        100
          'ultralight  200
          'light       300
          'semilight   350
          'book        380
          'normal      400
          'medium      500
          'semibold    600
          'bold        700
          'ultrabold   800
          'heavy       900
          'ultraheavy 1000))
(define font-weight-num-mapping
  (for/hasheq ([(sym num) (in-immutable-hash font-weight-sym-mapping)])
    (values num sym)))

(define font-family/c string?)
(define font-style/c (or/c 'normal 'italic 'oblique))
(define normalized-font-weight/c (integer-in 100 1000))
(define font-weight/c (apply or/c normalized-font-weight/c (hash-keys font-weight-sym-mapping)))
(define font-size/c (>=/c 0))

(struct font-description (family style weight size)
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (self out mode)
    (match-define (font-description family style weight size) self)
    (cond
      [(eq? mode 0)
       (~> (quasiexpr (make-font-description
                       {~seq #:family ,family}
                       {~if (eq? style 'normal) {~seq #:style ,style}}
                       {~if (eq? weight 400) {~seq #:weight ,(prettify-font-weight weight)}}
                       {~if size {~seq #:size ,size}}))
           (print out))]
      [else
       (write-string "#<font-description:" out)
       (define pretty-weight (prettify-font-weight weight))
       (display (printing-sequence
                 #:space-after 1
                 (~>> (list (~v family)
                            (and (not (eq? style 'normal)) style)
                            (and (not (eq? pretty-weight 'normal))
                                 (if (symbol? pretty-weight)
                                     pretty-weight
                                     (~a "weight=" pretty-weight)))
                            (and size
                                 (~a size "px")))
                      (filter values)))
                out)
       (write-string ">" out)])))

(define (make-font-description #:family [family #f]
                               #:style [style 'normal]
                               #:weight [weight 400]
                               #:size [size #f]
                               #:cache [cache (current-font-description-cache)])
  (define new-desc (font-description family style (and~> weight normalize-font-weight) size))
  (cond
    [cache
     (cond
       [(hash-ref-key (font-description-cache-hash cache) new-desc #f)]
       [else
        (hash-set! (font-description-cache-hash cache) new-desc #t)
        new-desc])]
    [else
     new-desc]))

(define (make-partial-font-description #:family [family #f]
                                       #:style [style #f]
                                       #:weight [weight #f]
                                       #:size [size #f]
                                       #:cache [cache (current-font-description-cache)])
  (make-font-description #:family family
                         #:style style
                         #:weight weight
                         #:size size
                         #:cache cache))

(define (copy-font-description desc
                               #:family [family (font-description-family desc)]
                               #:style [style (font-description-style desc)]
                               #:weight [weight (font-description-weight desc)]
                               #:size [size (font-description-size desc)]
                               #:cache [cache (current-font-description-cache)])
  (make-font-description #:family family
                         #:style style
                         #:weight weight
                         #:size size
                         #:cache cache))

(define combine-font-descriptions
  (case-lambda
    [() (make-partial-font-description)]
    [(desc) desc]
    [(desc . descs)
     (for/fold ([desc-a desc])
               ([desc-b (in-list descs)])
       (match-define (font-description family-a style-a weight-a size-a) desc-a)
       (match-define (font-description family-b style-b weight-b size-b) desc-b)
       (make-font-description #:family (or family-b family-a)
                              #:style (or style-b style-b)
                              #:weight (or weight-b weight-a)
                              #:size (or size-b size-a)))]))

(define (normalize-font-weight v)
  (if (symbol? v)
      (hash-ref font-weight-sym-mapping v)
      v))

(define (prettify-font-weight v)
  (hash-ref font-weight-num-mapping v v))

(define (font-description->PangoFontDescription desc)
  (match-define (font-description family style weight size) desc)

  (define pango-desc (pango_font_description_new))
  (when family (pango_font_description_set_family pango-desc family))
  (when style (pango_font_description_set_style pango-desc style))
  (when weight (pango_font_description_set_weight pango-desc weight))
  (when size (pango_font_description_set_absolute_size pango-desc (px->pango size)))

  pango-desc)

(define (PangoFontDescription->font-description pango-desc)
  (define set-fields (pango_font_description_get_set_fields pango-desc))
  (make-font-description
   #:family (and (memq 'family set-fields)
                 (pango_font_description_get_family pango-desc))
   #:style (and (memq 'style set-fields)
                (pango_font_description_get_style pango-desc))
   #:weight (and (memq 'weight set-fields)
                 (pango_font_description_get_weight pango-desc))
   #:size (and (memq 'size set-fields)
               (pango->px (pango_font_description_get_size pango-desc)))))

(struct font-map
  (pango-font-map
   pango-context
   font-description-mapping ; (ephemeron-hash/c font-description? font?)
   pango-font-mapping))     ; (ephemeron-hash/c PangoFont? font?)

;; FIXME: cache pango contexts
(define (make-pango-context pango-fm)
  (define context (pango_font_map_create_context pango-fm))

  (define options (cairo_font_options_create))
  (cairo_font_options_set_antialias options 'gray)
  (cairo_font_options_set_hint_metrics options 'off)
  (pango_cairo_context_set_font_options context options)
  (cairo_font_options_destroy options)

  context)

(define (make-font-map)
  (define pango-fm (pango_cairo_font_map_new))
  (pango_cairo_font_map_set_resolution pango-fm 72.0)
  (font-map pango-fm
            (make-pango-context pango-fm)
            (make-ephemeron-hash)
            (make-ephemeron-hash)))

(define current-font-map (make-parameter (make-font-map)))

(define (resolve-font-description
         desc
         #:font-map [font-map (current-font-map)]
         #:fail [fail (λ () (raise-arguments-error 'resolve-font-description
                                                   "no font matching description"
                                                   "description" desc))])
  (define desc-map (font-map-font-description-mapping font-map))
  (cond
    [(hash-ref desc-map desc #f)]
    [else
     (define pango-desc (font-description->PangoFontDescription desc))
     (cond
       [(begin0
          (pango_font_map_load_font (font-map-pango-font-map font-map)
                                    (font-map-pango-context font-map)
                                    pango-desc)
          (pango_font_description_free pango-desc))
        => (λ (pango-font)
             (define font (PangoFont->font font-map pango-font))
             (hash-set! desc-map desc font)
             font)]
       [else
        (if (procedure? fail)
            (fail)
            fail)])]))

(define (resolve-font-description* desc
                                   #:font-map [font-map (current-font-map)]
                                   #:language [language #f])
  (define pango-desc (font-description->PangoFontDescription desc))
  (cond
    [(begin0
       (pango_font_map_load_fontset (font-map-pango-font-map font-map)
                                    (font-map-pango-context font-map)
                                    pango-desc
                                    (if language
                                        (pango_language_from_string language)
                                        (pango_language_get_default)))
       (pango_font_description_free pango-desc))
     => (λ (pango-fontset)
          (define pango-fonts '())
          (pango_fontset_foreach
           pango-fontset
           (λ (font)
             (set! pango-fonts (cons (g_object_ref font) pango-fonts))
             #f))
          (map (λ~> (PangoFont->font font-map _)) (reverse pango-fonts)))]
    [else
     '()]))

(define (PangoFont->font font-map pango-font)
  (call-as-atomic
   (λ ()
     (hash-ref! (font-map-pango-font-mapping font-map)
                pango-font
                (λ () (make-font font-map pango-font))))))

;; -----------------------------------------------------------------------------
;; fonts

(struct font (font-map
              describe
              pango-font
              pango-description
              hb-font
              math-font?
              math-script-metrics)
  #:property prop:custom-write
  (λ (self out mode)
    (match-define (font-description family style weight size) (font-describe self))
    (write-string
     (~a "#<font:" (~v family)
         (if (eq? style 'normal) "" (~a " " style))
         (if (eq? weight 400) "" (~a " " (prettify-font-weight weight)))
         " " (if (zero? size) "0" size) "px>")
     out)))

(define (make-font font-map pango-font)
  (define pango-desc (pango_font_describe_with_absolute_size pango-font))
  (define desc (PangoFontDescription->font-description pango-desc))
  (define hb-font (pango_font_get_hb_font pango-font))
  (font font-map
        desc
        pango-font
        pango-desc
        hb-font
        (hb_ot_math_has_data (hb_font_get_face hb-font))
        (load-math-script-metrics hb-font #:size (font-description-size desc))))

(define (missing-glyph-error who f c)
  (raise-arguments-error who "no glyph in font for char"
                         "font" f
                         "char" c))

(define (glyph-id? v) (exact-nonnegative-integer? v))

(define (font-nominal-glyph-id f c #:missing [missing (λ () (missing-glyph-error 'font-get-nominal-glyph f c))])
  (or (hb_font_get_nominal_glyph (font-hb-font f) c)
      (if (procedure? missing) (missing) missing)))

(define (font-glyph-italic-correction font glyph-id)
  (define hb-font (font-hb-font font))
  (pango->px (hb_ot_math_get_glyph_italics_correction hb-font glyph-id)))

(define (math-font? v)
  (and (font? v) (font-math-font? v)))

;; -----------------------------------------------------------------------------
;; math metrics

(struct math-script-metrics
  (depth-scales
   space-after

   sub-shift-down
   sub-top-max
   sub-baseline-drop-min

   sup-shift-up
   sup-shift-up-cramped
   sup-bottom-min
   sup-baseline-drop-max

   sub-sup-gap-min
   sup-bottom-max-with-sub)
  #:transparent)

(define (load-math-script-metrics hb-font #:size em-size)
  (define (get which #:convert [convert values] #:fallback [fallback 0])
    (define val (hb_ot_math_get_constant hb-font which))
    (if (zero? val)
        (if (procedure? fallback)
            (fallback)
            fallback)
        (convert val)))

  (define (get/px which #:fallback [fallback 0])
    (get which #:fallback fallback #:convert pango->px))

  (define (get-mvar which)
    (pango->px (or (hb_ot_metrics_get_position hb-font which) 0)))

  (define (convert-percent v)
    (fl/ (->fl v) 100.0))

  ;; Fallback values are computed as recommended by the MathML Core 2021-08-16 Working Draft:
  ;;   <https://www.w3.org/TR/mathml-core/#layout-constants-mathconstants>
  (math-script-metrics
   (vector-immutable
    (get 'script-percent-scale-down #:convert convert-percent #:fallback 0.71)
    (get 'script-script-percent-scale-down #:convert convert-percent #:fallback 0.5041))

   (get/px 'space-after-script #:fallback (λ () (fl* em-size (real->double-flonum 1/24))))

   (get/px 'subscript-shift-down #:fallback (λ () (get-mvar 'subscript-em-y-offset)))
   (get/px 'subscript-top-max #:fallback (λ () (fl* (get-mvar 'x-height) 0.8)))
   (get/px 'subscript-baseline-drop-min)

   (get/px 'superscript-shift-up #:fallback (λ () (get-mvar 'superscript-em-y-offset)))
   (get/px 'superscript-shift-up-cramped #:fallback (λ () (get-mvar 'superscript-em-y-offset)))
   (get/px 'superscript-bottom-min #:fallback (λ () (fl* (get-mvar 'x-height) 0.25)))
   (get/px 'superscript-baseline-drop-max)

   (get/px 'sub-superscript-gap-min #:fallback (λ () (fl* (get-mvar 'underline-size) 4.0)))
   (get/px 'superscript-bottom-max-with-subscript #:fallback (λ () (fl* (get-mvar 'x-height) 0.8)))))

(define (math-script-depth? v)
  (exact-nonnegative-integer? v))

(define (math-script-metrics-depth-scale sm depth)
  (define scales (math-script-metrics-depth-scales sm))
  (if (or (zero? depth)
          (zero? (vector-length scales)))
      1
      (vector-ref scales (sub1 (min depth (vector-length scales))))))

;; <https://docs.microsoft.com/en-us/typography/opentype/spec/math#mathkerninfo-table>
(struct math-script-kerns
  (top-left
   top-right
   bottom-left
   bottom-right)
  #:transparent)

(define no-math-script-kerns (math-script-kerns #f #f #f #f))
(define (any-math-script-kerns? cis)
  (and (or (math-script-kerns-top-left cis)
           (math-script-kerns-top-right cis)
           (math-script-kerns-bottom-left cis)
           (math-script-kerns-bottom-right cis))
       #t))

(define (make-math-script-kerns #:top-left [tl #f]
                                #:top-right [tr #f]
                                #:bottom-left [bl #f]
                                #:bottom-right [br #f])
  (if (or tl tr bl br)
      (math-script-kerns tl tr bl br)
      no-math-script-kerns))

(define (copy-math-script-kerns cis
                                #:top-left [tl (math-script-kerns-top-left cis)]
                                #:top-right [tr (math-script-kerns-top-right cis)]
                                #:bottom-left [bl (math-script-kerns-bottom-left cis)]
                                #:bottom-right [br (math-script-kerns-bottom-right cis)])
  (make-math-script-kerns #:top-left tl
                          #:top-right tr
                          #:bottom-left bl
                          #:bottom-right br))

(define (scale-math-script-kerns ci x y)
  (match-define (math-script-kerns tl tr bl br) ci)
  (make-math-script-kerns #:top-left (and~> tl (scale-math-script-kern x y))
                          #:top-right (and~> tr (scale-math-script-kern x y))
                          #:bottom-left (and~> bl (scale-math-script-kern x y))
                          #:bottom-right (and~> br (scale-math-script-kern x y))))

(define (h-append-math-script-kerns a b)
  (make-math-script-kerns #:top-left (math-script-kerns-top-left a)
                          #:bottom-left (math-script-kerns-bottom-left a)
                          #:top-right (math-script-kerns-top-right b)
                          #:bottom-right (math-script-kerns-bottom-right b)))

;; https://docs.microsoft.com/en-us/typography/opentype/spec/math#mathkern-table
(struct math-script-kern (correction-heights kern-values)
  #:property prop:custom-write
  (λ (self out mode)
    (match-define (math-script-kern correction-heights kern-values) self)
    (write-string "#<math-script-kern:" out)
    (write-string (~r (vector-ref kern-values 0) #:sign '+ #:precision 2) out)
    (for ([kern-value (in-vector kern-values 1)]
          [correction-height (in-vector correction-heights)])
      (write-string (~a " ⦉" (~r correction-height #:precision 2)
                        "⦊ " (~r kern-value #:sign '+ #:precision 2))
                    out))
    (write-string ">" out)))

(define (math-script-kern-value ci height)
  (cond
    [ci
     (match-define (math-script-kern correction-heights kern-values) ci)
     (let loop ([i 0]
                [count (vector-length correction-heights)])
       (cond
         [(> count 0)
          (define half (quotient count 2))
          (define height* (vector-ref correction-heights (+ i half)))
          (if (< height* height)
              (loop (+ i half 1) (- count (+ half 1)))
              (loop i half))]
         [else
          (vector-ref kern-values i)]))]
    [else 0]))

(define (unsafe-make-math-script-kern num-heights proc)
  (define correction-heights (make-vector num-heights))
  (define kern-values (make-vector (add1 num-heights)))
  (proc correction-heights kern-values)
  (math-script-kern (unsafe-vector*->immutable-vector! correction-heights)
               (unsafe-vector*->immutable-vector! kern-values)))

(define (scale-math-script-kern ci x y)
  (match-define (math-script-kern correction-heights kern-values) ci)
  (define num-heights (vector-length correction-heights))
  (unsafe-make-math-script-kern
   num-heights
   (λ (correction-heights* kern-values*)
     (for ([i (in-range num-heights)])
       (vector-set! correction-heights* i (* (vector-ref correction-heights i) y))
       (vector-set! kern-values* i (* (vector-ref kern-values i) x)))
     (vector-set! kern-values* num-heights (* (vector-ref kern-values num-heights) x)))))

;; FIXME: cache?
(define (font-glyph-math-script-kerns font glyph-id)
  (unless hb_ot_math_get_glyph_kernings
    (raise-arguments-error
     'font-glyph-math-script-kerns
     "libharfbuzz is too old to load math script kerns"
     "current version" (unquoted-printing-string (hb_version_string))
     "needed version" (unquoted-printing-string ">=3.4.0")))

  (define hb-font (font-hb-font font))

  (define (get-one which)
    (define kern-entries (hb_ot_math_get_glyph_kernings hb-font glyph-id which))
    (cond
      [kern-entries
       (define num-heights (sub1 (cvector-length kern-entries)))
       (unsafe-make-math-script-kern
        num-heights
        (λ (correction-heights kern-values)
          (for ([i (in-range num-heights)])
            (define entry (cvector-ref kern-entries i))
            (vector-set! correction-heights i (pango->px (hb_ot_math_kern_entry_t-max-correction-height entry)))
            (vector-set! kern-values i (pango->px (hb_ot_math_kern_entry_t-kern-value entry))))
          (define last-entry (cvector-ref kern-entries num-heights))
          (vector-set! kern-values num-heights (pango->px (hb_ot_math_kern_entry_t-kern-value last-entry)))))]
      [else #f]))

  (make-math-script-kerns #:top-left (get-one 'top-left)
                          #:top-right (get-one 'top-right)
                          #:bottom-left (get-one 'bottom-left)
                          #:bottom-right (get-one 'bottom-right)))
