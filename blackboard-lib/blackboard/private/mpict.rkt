#lang racket/base

(require (for-syntax racket/base)
         (only-in pict
                  current-expected-text-scale
                  dc-for-text-size
                  make-pict-drawer
                  pict?
                  pict-width
                  pict-height
                  pict-ascent
                  pict-descent)
         (prefix-in p: pict)
         pict/convert
         racket/contract
         racket/class
         (only-in racket/draw
                  dc-path%
                  make-brush
                  make-color
                  make-pen)
         racket/match
         syntax/parse/define
         threading

         "draw/font.rkt"
         "draw/text.rkt"
         "util/case-kw.rkt")

;; -----------------------------------------------------------------------------

(provide (contract-out
          [mpict? predicate/c]
          [mpict-width (-> mpict? real?)]
          [mpict-height (-> mpict? real?)]
          [mpict-ascent (-> mpict? real?)]
          [mpict-descent (-> mpict? real?)]
          [mpict-italic-correction (-> mpict? real?)]
          [mpict-script-kerns (-> mpict? math-script-kerns?)]

          [pict->mpict (-> pict? mpict?)]

          [blank (case-kw->
                  (->* [] [real? real? real?] mpict?)
                  (->* [#:h real?] [#:w real?] mpict?)
                  (->* [] [#:w real? #:a real? #:d real?] mpict?))]
          [inset (case-kw->
                  (->* [mpict? real?] [real?] mpict?)
                  (->* [mpict?] [#:h real? #:v real?] mpict?)
                  (->* [mpict?] [#:l real? #:r real? #:t real? #:b real?] mpict?))]
          [apply-italic-correction (-> mpict? mpict?)]

          [scale (->* [mpict? real?] [real?] mpict?)]

          [hbl-append (-> mpict? ... mpict?)]
          [lbl-superimpose (-> mpict? ... mpict?)]
          [pin-over (->* [mpict? mpict?] [#:x real? #:y real?] mpict?)]
          [pin-under (->* [mpict? mpict?] [#:x real? #:y real?] mpict?)]

          [text (->* [string?
                      #:font font?]
                     [#:math-depth math-script-depth?]
                     mpict?)]
          [scripts (->* [mpict?
                         #:metrics math-script-metrics?]
                        [#:sup (or/c mpict? #f)
                         #:sub (or/c mpict? #f)
                         #:cramped? any/c
                         #:use-baseline-drop? any/c]
                        mpict?)]
          [explain (-> mpict? mpict?)]))

;; -----------------------------------------------------------------------------

(struct mpict
  (draw
   width
   ascent
   descent
   italic-correction
   script-kerns)
  #:property prop:pict-convertible
  (λ (self)
    (p:dc (make-mpict-drawer self)
          (mpict-width self)
          (mpict-height self)
          (mpict-ascent self)
          (mpict-descent self))))

(define (mpict-height p)
  (+ (mpict-ascent p)
     (mpict-descent p)))

(define (make-mpict #:draw draw
                    #:width width
                    #:ascent ascent
                    #:descent [descent 0]
                    #:italic-correction [italic-correction 0]
                    #:script-kerns [script-kerns no-math-script-kerns])
  (mpict draw width ascent descent italic-correction script-kerns))

(define (copy-mpict p
                    #:draw [draw (mpict-draw p)]
                    #:width [width (mpict-width p)]
                    #:ascent [ascent (mpict-ascent p)]
                    #:descent [descent (mpict-descent p)]
                    #:italic-correction [italic-correction (mpict-italic-correction p)]
                    #:script-kerns [script-kerns (mpict-script-kerns p)])
  (mpict draw width ascent descent italic-correction script-kerns))

;; -----------------------------------------------------------------------------

(define (pict->mpict p)
  (make-mpict #:draw (make-pict-drawer p)
              #:width (pict-width p)
              #:ascent (- (pict-height p)
                          (pict-descent p))
              #:descent (pict-descent p)))

(define (draw-mpict p dc [x 0] [y 0])
  ((mpict-draw p) dc x y))

(define (make-mpict-drawer p)
  (λ (dc [x 0] [y 0])
    (draw-mpict p dc x y)))

(define blank
  (case-kw-lambda
    [([w 0] [a w] [d 0]) (blank #:w w #:a a #:d d)]
    [(#:w [w 0] #:h h)   (blank #:w w #:a h)]
    [(#:w [w 0] #:a [a 0] #:d [d 0])
     (make-mpict #:width w
                 #:ascent a
                 #:descent d
                 #:draw void)]))

(define inset
  (case-kw-lambda
    [(p h [v h]) (inset p #:h h #:v v)]
    [(p #:h [h 0] #:v [v 0]) (inset p #:l h #:r h #:t v #:b v)]
    [(p #:l [l 0] #:r [r 0] #:t [t 0] #:b [b 0])
     (define draw (make-mpict-drawer p))
     (copy-mpict
      p
      #:width (+ (mpict-width p) l r)
      #:ascent (+ (mpict-ascent p) t)
      #:descent (+ (mpict-descent p) b)
      ;; TODO: is discarding the italic correction and script-kerns right?
      #:italic-correction 0
      #:script-kerns no-math-script-kerns
      #:draw (λ (dc x y) (draw dc (+ x l) (+ y t))))]))

(define (translate p #:x [dx 0] #:y [dy 0] #:extend-bb? [extend-bb? #f])
  (cond
    [(and (zero? dx) (zero? dy)) p]
    [else
     (define draw (make-mpict-drawer p))
     (if extend-bb?
         (copy-mpict
          p
          #:width (+ (mpict-width p) (abs dx))
          #:ascent (+ (mpict-ascent p) (if (negative? dy) (- dy) 0))
          #:descent (+ (mpict-descent p) (if (positive? dy) dy 0))
          ;; TODO: is discarding the italic correction and script-kerns right?
          #:italic-correction 0
          #:script-kerns no-math-script-kerns
          #:draw
          (let ([dx* (max 0 dx)]
                [dy* (max 0 dy)])
            (λ (dc x y) (draw dc (+ x dx*) (+ y dy*)))))
         (copy-mpict
          p
          #:script-kerns (and (mpict-script-kerns p)
                         (error 'translate "TODO script-kerns"))
          #:draw (λ (dc x y) (draw dc (+ x dx) (+ y dy)))))]))

(define (set-italic-correction p ic)
  (copy-mpict p #:italic-correction ic))

(define (apply-italic-correction p)
  (copy-mpict p
              #:width (+ (mpict-width p) (mpict-italic-correction p))
              #:italic-correction 0
              #:script-kerns (copy-math-script-kerns (mpict-script-kerns p)
                                                     #:top-right #f
                                                     #:bottom-right #f)))

(define (scale p x-fac [y-fac x-fac])
  (define draw (make-mpict-drawer p))
  (make-mpict
   #:width (* (mpict-width p) x-fac)
   #:ascent (* (mpict-ascent p) y-fac)
   #:descent (* (mpict-descent p) y-fac)
   #:italic-correction (* (mpict-italic-correction p) x-fac)
   #:script-kerns (scale-math-script-kerns (mpict-script-kerns p) x-fac y-fac)
   #:draw
   (λ (dc x y)
     (define t (send dc get-transformation))
     (send dc scale x-fac y-fac)
     (draw dc (/ x x-fac) (/ y y-fac))
     (send dc set-transformation t))))

(define-simple-macro (scale/improve-new-text p {~or* fac-e {~seq x-fac-e y-fac-e}})
  #:declare p (expr/c #'mpict? #:name "mpict argument")
  #:declare fac-e (expr/c #'real? #:name "scale argument")
  #:declare x-fac-e (expr/c #'real? #:name "x scale argument")
  #:declare y-fac-e (expr/c #'real? #:name "y scale argument")
  (let* ([x-fac {~? x-fac-e.c fac-e.c}]
         [y-fac {~? y-fac-e.c x-fac}]
         [old-scale (current-expected-text-scale)])
    (parameterize ([current-expected-text-scale
                    (list (* x-fac (car old-scale))
                          (* y-fac (cadr old-scale)))])
      (scale p.c x-fac y-fac))))

(define (hbl-append . ps)
  (match ps
    ['() (blank)]
    [(cons p ps)
     (for/fold ([p1 p])
               ([p2 (in-list ps)])
       (define p1.w (mpict-width p1))
       (define p1.a (mpict-ascent p1))
       (define p1.d (mpict-descent p1))
       (define p1.draw (make-mpict-drawer p1))

       (define p2.w (mpict-width p2))
       (define p2.a (mpict-ascent p2))
       (define p2.d (mpict-descent p2))
       (define p2.draw (make-mpict-drawer p2))

       (define-values [p1.dy p2.dy] (if (< p1.a p2.a)
                                        (values (- p2.a p1.a) 0)
                                        (values 0 (- p1.a p2.a))))
       (make-mpict
        #:width (+ p1.w p2.w)
        #:ascent (max p1.a p2.a)
        #:descent (max p1.d p2.d)
        #:italic-correction (mpict-italic-correction p2)
        #:script-kerns (h-append-math-script-kerns (mpict-script-kerns p1) (mpict-script-kerns p2))
        #:draw
        (λ (dc x y)
          (p1.draw dc x (+ y p1.dy))
          (p2.draw dc (+ x p1.w) (+ y p2.dy)))))]))

(define (lbl-superimpose . ps)
  (match ps
    ['() (blank)]
    [(cons p ps)
     (for/fold ([p1 p])
               ([p2 (in-list ps)])
       (define p1.w (mpict-width p1))
       (define p1.a (mpict-ascent p1))
       (define p1.draw (make-mpict-drawer p1))

       (define p2.w (mpict-width p2))
       (define p2.a (mpict-ascent p2))
       (define p2.draw (make-mpict-drawer p2))

       (define w (max p1.w p2.w))
       (define-values [p1.dy p2.dy] (if (< p1.a p2.a)
                                        (values (- p2.a p1.a) 0)
                                        (values 0 (- p1.a p2.a))))
       (make-mpict
        #:width w
        #:ascent (max p1.a p2.a)
        #:descent (max (mpict-descent p1) (mpict-descent p2))
        #:italic-correction (- (max (+ p1.w (mpict-italic-correction p1))
                                    (+ p2.w (mpict-italic-correction p2)))
                               w)
        ;; FIXME: Should use lub of script-kern values.
        #:script-kerns no-math-script-kerns
        #:draw
        (λ (dc x y)
          (p1.draw dc x (+ y p1.dy))
          (p2.draw dc x (+ y p2.dy)))))]))

(define (pin-over base p #:x [dx 0] #:y [dy 0])
  (define base.draw (make-mpict-drawer base))
  (define p.draw (make-mpict-drawer p))
  (copy-mpict base
              #:draw (λ (dc x y)
                       (base.draw dc x y)
                       (p.draw dc x y))))

(define (pin-under base p #:x [dx 0] #:y [dy 0])
  (define base.draw (make-mpict-drawer base))
  (define p.draw (make-mpict-drawer p))
  (copy-mpict base
              #:draw (λ (dc x y)
                       (p.draw dc x y)
                       (base.draw dc x y))))

(define (scripts nucleus #:sup [sup #f] #:sub [sub #f]
                 #:metrics metrics
                 #:cramped? [cramped? #f]
                 #:use-baseline-drop? [use-baseline-drop? #t])
  (cond
    [(and (not sup) (not sub)) nucleus]
    [else
     (define space-after (math-script-metrics-space-after metrics))

     ; Computes a tentative amount to shift a superscript up, before resolving
     ; any superscript/subscript collisions.
     (define (get-base-shift-up)
       (max
        ; We start with a minimum shift specified by the font.
        (if cramped?
            (math-script-metrics-sup-shift-up-cramped metrics)
            (math-script-metrics-sup-shift-up metrics))

        ; Next, we ensure that the bottom of the superscript’s descent is lifted
        ; above a font-specified minimum. That is, if a superscript descends far
        ; enough below its baseline, we may want to shift it up further.
        (+ (mpict-descent sup)
           (math-script-metrics-sup-bottom-min metrics))

        ; Finally, we ensure the superscript’s baseline is not dropped further
        ; from the top of the nucleus than a font-specified maximum. This makes
        ; sure that if the nucleus is sufficiently tall, we shift the
        ; superscript up even more than usual so it’s attached to its upper
        ; right corner rather than somewhere in the middle.
        ;
        ; This rule should only be applied conditionally: in TeX, it only
        ; applies when the nucleus is a box rather than an individual character.
        ; We don’t distinguish between boxes and characters in the same way, so
        ; the choice of whether or not to apply this rule is deferred to higher-
        ; level logic.
        (if use-baseline-drop?
            (- (mpict-ascent nucleus)
               (math-script-metrics-sup-baseline-drop-max metrics))
            0)))

     ; Computes a tentative amount to shift a subscript down, before resolving
     ; any superscript/subscript collisions.
     (define (get-base-shift-down)
       (max
        ; As with a superscript, we start with a minimum drop specified by the
        ; font. However, we don’t make any distinction based on whether the
        ; context is “cramped” or not, because in TeX, subscripts are *always*
        ; considered cramped, and OpenType Math copied that decision.
        (math-script-metrics-sub-shift-down metrics)

        ; Next, we ensure that the top of the subscript’s ascent is dropped
        ; below a font-specified maximum, dual to the superscript case.
        (- (mpict-ascent sub)
           (math-script-metrics-sub-top-max metrics))

        ; Finally, we ensure the subscript’s baseline is dropped sufficiently
        ; close to the bottom of the nucleus according to a font-specified
        ; minimum, also dual to the superscript case.
        (if use-baseline-drop?
            (- (mpict-descent nucleus)
               (math-script-metrics-sub-baseline-drop-min metrics))
            0)))

     (define (shifted-sup shift-up)
       (define nucleus-script-kern (math-script-kerns-top-right (mpict-script-kerns nucleus)))
       (define sup-script-kern (math-script-kerns-bottom-left (mpict-script-kerns sup)))
       (define kern (min (+ (math-script-kern-value nucleus-script-kern (mpict-ascent nucleus))
                            (math-script-kern-value sup-script-kern (- (mpict-ascent nucleus) shift-up)))
                         (+ (math-script-kern-value nucleus-script-kern (- shift-up (mpict-descent sup)))
                            (math-script-kern-value sup-script-kern (mpict-descent sup)))))
       (~> (set-italic-correction sup 0)
           (inset #:l (+ (mpict-italic-correction nucleus) kern)
                  #:r space-after)
           (translate #:y (- shift-up) #:extend-bb? #t)))

     (define (shifted-sub shift-down)
       (define nucleus-script-kern (math-script-kerns-bottom-right (mpict-script-kerns nucleus)))
       (define sub-script-kern (math-script-kerns-top-left (mpict-script-kerns sub)))
       (define kern (min (+ (math-script-kern-value nucleus-script-kern (mpict-descent nucleus))
                            (math-script-kern-value sub-script-kern (- (mpict-descent nucleus) shift-down)))
                         (+ (math-script-kern-value nucleus-script-kern (- shift-down (mpict-ascent sub)))
                            (math-script-kern-value sub-script-kern (mpict-ascent sub)))))
       (~> (set-italic-correction sub 0)
           (inset #:l kern #:r space-after)
           (translate #:y shift-down #:extend-bb? #t)))

     (cond
       ; In the simplest case of a superscript but no subscript, just shift it
       ; up the needed amount and attach it.
       [(not sub)
        (hbl-append nucleus (shifted-sup (get-base-shift-up)))]

       ; If we have a subscript but no superscript, the situation is not much
       ; more complex, but we want to preserve any portion of the italic
       ; correction that extends beyond the placed subscript (unlikely as it is
       ; that there will ever be any at all).
       [(not sup)
        (~> (hbl-append nucleus (shifted-sub (get-base-shift-down)))
            (set-italic-correction (max 0 (- (mpict-italic-correction nucleus)
                                             (mpict-width sub)))))]

       ; If we have both a superscript and a subscript, we have significantly
       ; more work to do, as we must ensure they do not visually collide.
       [else
        (define gap-min (math-script-metrics-sub-sup-gap-min metrics))
        (define base-shift-up (get-base-shift-up))
        (define base-shift-down (get-base-shift-down))

        ; When we have both a superscript and a subscript, we need to ensure
        ; they are placed sufficiently far apart so they don’t visually collide.
        ; To start, we compute the current gap between the bottom the
        ; superscript’s descent and top of the subscript’s ascent given our
        ; tentative placement locations.
        (define base-sup-bottom (- base-shift-up (mpict-descent sup)))
        (define base-sub-top (- (mpict-ascent sub) base-shift-down))
        (define base-gap (- base-sup-bottom base-sub-top))
        (define-values [shift-up shift-down]
          (cond
            ; Now we check whether the gap between them is larger than a
            ; font-specified minimum.
            [(>= base-gap gap-min)
             ; If it is, we’re fine, and we can just use the tentative locations.
             (values base-shift-up base-shift-down)]
            [else
             ; Otherwise, we have a collision. To resolve it, we start by trying
             ; to shift the superscript up. In the case of a collision, we’re
             ; allowed to move the bottom of the superscript up to a
             ; font-specified height.
             (define needed-extra-gap (- gap-min base-gap))
             (define allowed-extra-shift-up (max 0 (- (math-script-metrics-sup-bottom-max-with-sub metrics)
                                                      base-sup-bottom)))
             (cond
               ; If this extra allowed shift is enough to reach the minimum gap
               ; size, we just shift the superscript up the minimum amount we
               ; need.
               [(>= allowed-extra-shift-up needed-extra-gap)
                (values (+ base-shift-up needed-extra-gap) base-shift-down)]
               [else
                ; Otherwise, we shift the superscript up by the maximum allowed
                ; amount (if any) and shift the subscript down the rest of the
                ; needed amount.
                (values (+ base-shift-up allowed-extra-shift-up)
                        (+ base-shift-down (- needed-extra-gap allowed-extra-shift-up)))])]))

        (hbl-append nucleus (lbl-superimpose (shifted-sup shift-up)
                                             (shifted-sub shift-down)))])]))

(define (text str
              #:font font
              #:math-depth [math-depth 0])
  (define dc (dc-for-text-size))
  (unless dc
    (raise-arguments-error 'glyph "no dc<%> object installed for sizing"))

  (define features (if (zero? math-depth)
                       (hash)
                       (hash "ssty" (min math-depth 2))))
  (define le (measure-text-line dc font str #:features features))
  (define a (max 0 (line-extents-ink-ascent le)))
  (make-mpict
   #:width (line-extents-advance-width le)
   #:ascent a
   #:descent (max 0 (line-extents-ink-descent le))
   #:italic-correction (line-extents-italic-correction le)
   #:script-kerns (line-extents-math-script-kerns le)
   #:draw
   (λ (dc x y)
     (draw-text-line dc font str #:x x #:y (+ y a) #:features features))))

(define (glyph c #:font font #:math-depth [math-depth 0])
  (text (string c) #:font font #:math-depth math-depth))

(define (explain p)
  (define draw (make-mpict-drawer p))
  (define w (mpict-width p))
  (define a (mpict-ascent p))
  (define d (mpict-descent p))
  (define ic (mpict-italic-correction p))

  (define (script-kern-path ci tb lr)
    (define correction-heights (math-script-kern-correction-heights ci))
    (define kern-values (math-script-kern-kern-values ci))

    (define num-heights (vector-length correction-heights))
    (define first-kern (vector-ref kern-values 0))
    (define last-kern (vector-ref kern-values num-heights))

    (define-values [max-y min-y]
      (if (zero? num-heights)
          (values (+ a d) 0)
          (values (max (+ a d) (vector-ref correction-heights 0))
                  (min 0 (vector-ref correction-heights (sub1 num-heights))))))

    (define (kern->x k)
      (match* {lr tb}
        [{'left   _}      (- k)]
        [{'right 'top}    (+ w ic k)]
        [{'right 'bottom} (+ w k)]))
    (define (height->y h)
      (- a h))

    (define path (new dc-path%))
    (send path move-to (kern->x 0) max-y)
    (send path line-to (kern->x first-kern) max-y)

    (for ([h (in-vector correction-heights)]
          [k1 (in-vector kern-values)]
          [k2 (in-vector kern-values 1)])
      (send path line-to (kern->x k1) (height->y h))
      (send path line-to (kern->x k2) (height->y h)))

    (send path line-to (kern->x last-kern) min-y)
    (send path line-to (kern->x 0) min-y)
    (send path close)
    path)

  (define cis (mpict-script-kerns p))
  (define-values [tl tr bl br]
    (values (and~> (math-script-kerns-top-left cis) (script-kern-path 'top 'left))
            (and~> (math-script-kerns-top-right cis) (script-kern-path 'top 'right))
            (and~> (math-script-kerns-bottom-left cis) (script-kern-path 'bottom 'left))
            (and~> (math-script-kerns-bottom-right cis) (script-kern-path 'bottom 'right))))

  (copy-mpict
   p
   #:draw
   (λ (dc x y)
     (draw dc x y)

     (define old-pen (send dc get-pen))
     (define old-brush (send dc get-brush))
     (define old-alpha (send dc get-alpha))

     (send dc set-alpha 0.25)

     (when (any-math-script-kerns? cis)
       (send dc set-pen (make-pen #:style 'transparent))
       (define (draw-script-kern path color)
         (send dc set-brush (make-brush #:color color))
         (send dc draw-path path x y))
       (when tl (draw-script-kern tl (make-color 255 255   0)))
       (when tr (draw-script-kern tr (make-color 255   0   0)))
       (when bl (draw-script-kern bl (make-color 0   255   0)))
       (when br (draw-script-kern br (make-color   0   0 255))))

     (send dc set-alpha 0.7)

     (send dc set-brush (make-brush #:style 'transparent))

     (send dc set-pen (make-pen #:color "light blue"
                                #:width 1))
     (send dc draw-rectangle x y w (+ a d))

     (send dc set-pen (make-pen #:color "light blue"
                                #:width 1
                                #:style 'long-dash))
     (send dc draw-line x (+ y a) (+ x w) (+ y a))
     (send dc draw-line (+ x w ic) y (+ x w ic) (+ y a d))

     (send dc set-pen old-pen)
     (send dc set-brush old-brush)
     (send dc set-alpha old-alpha))))
