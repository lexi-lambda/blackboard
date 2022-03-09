#lang racket/base

(require racket/class
         racket/contract
         racket/hash
         racket/list
         racket/match
         threading

         "draw/font.rkt"
         "util/print.rkt"
         "mpict.rkt"
         "size.rkt"
         "style.rkt"
         (prefix-in s: "style-props.rkt"))

;; -----------------------------------------------------------------------------

(struct m:element (style) #:transparent)

(struct m:text m:element (content) #:transparent)

(struct m:space m:element (width ascent descent) #:transparent)
(struct m:row m:element (content) #:transparent)
(struct m:scripts m:element (nucleus-content sup-content sub-content) #:transparent)

(struct m:table m:element (rows) #:transparent)
(struct m:table-row (styles cells) #:transparent)
(struct m:table-cell (styles row-span column-span content) #:transparent)

;; -----------------------------------------------------------------------------

(define s:explain? (make-style-property 'explain?))
(define s:explain-tokens? (make-inherited-style-property 'explain-tokens?))

(define (get-script-style which inherited)
  (combine-styles
   (style s:math-depth (add1 (computed-style-value inherited s:math-depth))
          s:math-style 'compact)
   (match which
     ['sub (style s:math-shift 'compact)]
     ['sup (style)])))

(define (element->mpict e)
  (define (loop e #:style inherited-style)
    (match e
      [(? list?)
       (apply hbl-append (map (λ~> (loop #:style inherited-style)) e))]

      [(m:element element-style)
       (define style (compute-style inherited-style element-style))

       (define (recur e #:style [extra-style plain])
         (loop e #:style (compute-style style extra-style)))

       (define (resolve-math-font)
         (define base-font (~> (combine-font-descriptions (computed-style-value style s:font)
                                                          (computed-style-value style s:math-font))
                               resolve-font-description))

         (define depth (computed-style-value style s:math-depth))
         (define depth-scale (~> (font-math-script-metrics base-font)
                                 (math-script-metrics-depth-scale depth)))

         (define base-desc (font-describe base-font))
         (~> (copy-font-description base-desc
                                    #:size (* (font-description-size base-desc) depth-scale))
             resolve-font-description))

       (define (maybe-explain p #:token? [token? #f])
         (if (or (computed-style-value style s:explain?)
                 (and token? (computed-style-value style s:explain-tokens?)))
             (explain p)
             p))

       (match e
         [(m:space _ width ascent descent)
          (define ppem (font-description-size (font-describe (resolve-math-font))))
          (~> (blank #:w (make-size-absolute #:ppem ppem width)
                     #:a (make-size-absolute #:ppem ppem ascent)
                     #:d (make-size-absolute #:ppem ppem descent))
              (maybe-explain #:token? #t))]

         [(m:row _ content)
          (maybe-explain (recur content))]

         [(m:text _ str)
          (~> (text str
                    #:font (resolve-math-font)
                    #:math-depth (computed-style-value style s:math-depth))
              (maybe-explain #:token? #t))]

         [(m:scripts _ nucleus sup sub)
          (~> (scripts (recur nucleus)
                       #:sup (and sup (recur sup #:style (get-script-style 'sup style)))
                       #:sub (and sub (recur sub #:style (get-script-style 'sub style)))
                       #:metrics (font-math-script-metrics (resolve-math-font))
                       #:cramped? (match (computed-style-value style s:math-shift)
                                    ['normal  #f]
                                    ['compact #t]))
              maybe-explain)])]))

  (loop e #:style (compute-style #f plain)))

;; simple demo
(module+ main
  (require racket/draw
           (prefix-in p: pict)
           "unicode.rkt")

  (define (add-white-bg p)
    (p:pin-over (p:filled-rectangle (p:pict-width p) (p:pict-height p) #:draw-border? #f #:color "white") 0 0 p))

  (define (save-pict p where)
    (~> (add-white-bg p)
        p:pict->bitmap
        (send save-file where 'png)))

  (define (save-pict-svg p where)
    (define dc (new svg-dc% [width (p:pict-width p)]
                            [height (p:pict-height p)]
                            [output where]
                            [exists 'truncate/replace]))
    (send dc start-doc "")
    (send dc start-page)
    (p:draw-pict (add-white-bg p) dc 0 0)
    (send dc end-page)
    (send dc end-doc))

  (define thin-space  (m:space plain (ems 3/18) 0 0))
  (define med-space   (m:space plain (ems 4/18) 0 0))
  (define thick-space (m:space plain (ems 5/18) 0 0))
  (define quad        (m:space plain (ems 1)    0 0))

  (define (italic-glyph c)
    (m:text plain (string (math-char c #:italic? #t))))

  (define (demo-specimen #:family math-family)
    (m:scripts (style s:math-font (make-font-description #:family math-family))
               (italic-glyph #\f)
               (m:text plain "1")
               (m:text plain "2")))

  (define (demo-specimens #:explain? explain?)
    (~> (for/list ([family (in-list '("Blackboard Modern Math"
                                      "Blackboard Pagella Math"
                                      "Blackboard Cambria Math"))])
          (demo-specimen #:family family))
        (add-between (m:space (style s:explain-tokens? #f) (ems 1/2) 0 0))
        (m:row (style s:font (make-font-description #:size 48)
                      s:explain-tokens? explain?)
               _)
        element->mpict
        (inset 5)))

  (~> (p:vl-append (demo-specimens #:explain? #f)
                   (demo-specimens #:explain? #t))
      (~> (p:scale 3)
          (p:freeze #:scale 2))
      #;(~> (p:scale 10)
          (save-pict-svg "/tmp/example.svg")))

  #;(~> (m:group (style s:font (make-font-description #:size 72)
                      s:math-font (make-partial-font-description #:family "Blackboard Modern Math"))
               (list (m:scripts plain
                                (m:token plain (string (math-char #\x #:italic? #t)))
                                (m:token plain "2")
                                #f)
                     med-space
                     (m:token plain "+")
                     med-space
                     (m:scripts plain
                                (m:token plain (string (math-char #\y #:italic? #t)))
                                (m:token plain "2")
                                #f)
                     thick-space
                     (m:token plain "=")
                     thick-space
                     (m:token plain "0")))
      element->mpict
      (inset 5)
      (p:freeze #:scale 2))

  #;(define (reduction-rule #:math math-family
                          #:mathsf mathsf-family
                          #:explain? explain?)
    (define (italic-glyph c)
      #;(m:token plain (string (math-char c #:italic? #t)))
      (m:token (style s:math-font (make-font-description #:family math-family #:style 'italic)) (string c)))
    #;(~> (m:group (style s:font (make-font-description #:size 48)
                        s:math-font (make-font-description #:family math-family)
                        s:explain-tokens? explain?)
                 (list (italic-glyph #\E)
                       (m:token plain "[")
                       (m:token (style s:math-font (make-font-description #:family mathsf-family)) "fst")
                       (m:space plain (ems 1/3) 0 0)
                       (m:token plain "(")
                       (m:scripts plain (italic-glyph #\v) #f (m:token plain "1"))
                       (m:token plain ",")
                       thin-space
                       (m:scripts plain (italic-glyph #\v) #f (m:token plain "2"))
                       (m:token plain ")")
                       (m:token plain "]")
                       (m:space plain (ems 2/3) 0 0)
                       (m:token (style s:math-font (make-font-description #:family "Cambria")) "⟶")
                       (m:space plain (ems 2/3) 0 0)
                       (italic-glyph #\E)
                       (m:token plain "[")
                       (m:scripts plain (italic-glyph #\v) #f (m:token plain "1"))
                       (m:token plain "]")))
        element->mpict
        (inset 5)))

  #;(~> (p:vl-append (reduction-rule #:math "Blackboard Modern Math" #:mathsf "Latin Modern Sans" #:explain? #f)
                   (reduction-rule #:math "Blackboard Cambria Math" #:mathsf "Calibri" #:explain? #f)
                   (reduction-rule #:math "Blackboard Modern Math" #:mathsf "Latin Modern Sans" #:explain? #t)
                   (reduction-rule #:math "Blackboard Cambria Math" #:mathsf "Calibri" #:explain? #t))
      (p:scale 2)
      #;(p:freeze #:scale 2)
      (p:scale 4)
      (save-pict "/tmp/reduction-relation.png"))

  #;(~> (p:vl-append (reduction-rule #:math "Comic Sans MS" #:mathsf "Comic Sans MS" #:explain? #f)
                   (reduction-rule #:math "Comic Sans MS" #:mathsf "Comic Sans MS" #:explain? #t))
      (p:scale 2)
      #;(p:freeze #:scale 2)
      (p:scale 4)
      (save-pict "/tmp/reduction-relation.png")))
