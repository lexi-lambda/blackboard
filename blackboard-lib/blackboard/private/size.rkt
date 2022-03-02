#lang racket/base

(require racket/match
         racket/contract
         threading
         "util/print.rkt"
         (only-in "unicode.rkt" integer->superscript-string))

(provide (contract-out
          [size? predicate/c]
          [size-zero? (-> size? boolean?)]

          [fixed-size? predicate/c]
          [flexible-size? predicate/c]
          [absolute-fixed-size? predicate/c]
          (rename font-relative-size? font-relative-fixed-size? predicate/c)

          [ems (-> rational? finite-fixed-size?)]
          [size-ems (-> finite-fixed-size? rational?)]
          [size-pxs (-> finite-fixed-size? absolute-fixed-size?)]
          [absolutize (-> finite-fixed-size? #:ppem absolute-fixed-size? absolute-fixed-size?)]

          [flexibility? predicate/c]
          [infinite-flexibility? predicate/c]
          [fil (->* [] [(and/c rational? (not/c negative?))] flexibility?)]
          [fill (->* [] [(and/c rational? (not/c negative?)) exact-nonnegative-integer?] flexibility?)]
          [infinite-flexibility-weight (-> infinite-flexibility? rational?)]
          [infinite-flexibility-cardinality (-> infinite-flexibility? exact-nonnegative-integer?)]

          [flex (->* [size?]
                     [#:grow fixed-size?
                      #:shrink fixed-size?]
                     size?)]
          [size-basis (-> size? fixed-size?)]
          [size-grow (-> size? fixed-size?)]
          [size-shrink (-> size? fixed-size?)]

          [size+ (-> size? ... size?)]
          [size- (-> size? size? ... size?)]
          [size* (-> size? rational? ... size?)]
          [size/ (-> size? (and/c rational? (not/c zero?)) (and/c rational? (not/c zero?)) ... size?)]))

;; -----------------------------------------------------------------------------
;; fixed sizes

(define (absolute-fixed-size? v)
  (rational? v))

(struct font-relative-size (ems pxs)
  #:property prop:equal+hash
  (let ()
    (define (=? a b recur)
      (and (= (font-relative-size-ems a) (font-relative-size-ems b))
           (= (font-relative-size-pxs a) (font-relative-size-pxs b))))
    (define (hash self recur)
      (recur (cons (real->double-flonum (font-relative-size-ems self))
                   (real->double-flonum (font-relative-size-pxs self)))))
    (list =? hash hash))
  #:property prop:custom-write
  (λ (self out mode)
    (match self
      [(font-relative-size ems (? zero?))
       (if (eq? mode 0)
           (print (quasiexpr (em ,ems)) out)
           (fprintf out "#<size: ~aem>" ems))]
      [(font-relative-size ems pxs)
       (fprintf out "#<size: ~aem ~a ~apx>"
                ems
                (if (positive? pxs) "+" "-")
                (abs pxs))])))

(define (make-size ems pxs)
  (if (zero? ems)
      pxs
      (font-relative-size ems pxs)))

(define (finite-fixed-size? v)
  (or (absolute-fixed-size? v)
      (font-relative-size? v)))

(define (fixed-size? v)
  (or (finite-fixed-size? v)
      (infinite-flexibility? v)))

(define (size-pxs v)
  (if (font-relative-size? v)
      (font-relative-size-pxs v)
      v))

(define (size-ems v)
  (if (font-relative-size? v)
      (font-relative-size-ems v)
      0))

(define (absolutize s #:ppem ppem)
  (match s
    [(font-relative-size ems pxs)
     (+ pxs (* ems ppem))]
    [pxs pxs]))

(define (size-zero? s)
  (and (absolute-fixed-size? s) (zero? s)))

(define (ems n) (make-size n 0))

;; -----------------------------------------------------------------------------
;; flexible sizes

(struct infinite-flexibility (weight cardinality)
  #:property prop:equal+hash
  (let ()
    (define (=? a b recur)
      (and (= (infinite-flexibility-weight a) (infinite-flexibility-weight b))
           (= (infinite-flexibility-cardinality a) (infinite-flexibility-cardinality b))))
    (define (hash self recur)
      (recur (cons (real->double-flonum (infinite-flexibility-weight self))
                   (real->double-flonum (infinite-flexibility-cardinality self)))))
    (list =? hash hash))
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (self out mode)
    (match-define (infinite-flexibility weight cardinality) self)
    (if (eq? mode 0)
        (~> (match cardinality
              [0 (quasiexpr (fil ,weight))]
              [1 (quasiexpr (fill ,weight))]
              [_ (quasiexpr (fill ,weight ,cardinality))])
            (print out))
        (fprintf out "#<flexibility: ~a~a>"
                 weight
                 (match cardinality
                   [0 "fil"]
                   [1 "fill"]
                   [_ (string-append "fill" (integer->superscript-string cardinality))])))))

(define (fil [weight 1])
  (fill weight 0))

(define (fill [weight 1] [cardinality 1])
  (if (zero? weight)
      weight
      (infinite-flexibility weight cardinality)))

(define (flexibility? v)
  (or (fixed-size? v)
      (infinite-flexibility? v)))

(define flexibility+
  (case-lambda
    [() 0]
    [(s) s]
    [(a b)
     (match* {a b}
       [{(infinite-flexibility weight-a cardinality-a)
         (infinite-flexibility weight-b cardinality-b)}
        (cond
          [(> cardinality-a cardinality-b) a]
          [(< cardinality-a cardinality-b) b]
          [else (fill (+ weight-a weight-b) cardinality-a)])]
       [{(? infinite-flexibility?) _} a]
       [{_ (? infinite-flexibility?)} b]
       [{_ _} (size+ a b)])]
    [(s . ss)
     (for/fold ([a s])
               ([b (in-list ss)])
       (flexibility+ a b))]))

(define flexibility*
  (case-lambda
    [(s) s]
    [(s n)
     (match s
       [(infinite-flexibility weight cardinality)
        (fill (* weight n) cardinality)]
       [_ (size* s n)])]
    [(s . ns)
     (match s
       [(infinite-flexibility weight cardinality)
        (fill (apply * weight ns) cardinality)]
       [_ (apply size* s ns)])]))

(struct flexible-size (basis grow shrink))

(define (flex basis #:grow [grow 0] #:shrink [shrink 0])
  (if (and (size-zero? grow)
           (size-zero? shrink))
      basis
      (match basis
        [(flexible-size old-basis old-grow old-shrink)
         (flex old-basis
               #:grow (size+ old-grow grow)
               #:shrink (size+ old-shrink shrink))]
        [_ (flexible-size basis grow shrink)])))

;; -----------------------------------------------------------------------------
;; size operations

(define (size? v)
  (or (fixed-size? v)
      (flexible-size? v)))

(define (finite-size? v)
  (or (finite-fixed-size? v)
      (and (flexible-size? v)
           (finite-fixed-size?))))

(define (size-basis v)
  (if (flexible-size? v)
      (flexible-size-basis v)
      v))

(define (size-grow v)
  (if (flexible-size? v)
      (flexible-size-grow v)
      0))

(define (size-shrink v)
  (if (flexible-size? v)
      (flexible-size-shrink v)
      v))

(define size+
  (case-lambda
    [() 0]
    [(s) s]
    [(a b)
     (match* {a b}
       [{(infinite-flexibility weight-a cardinality-a)
         (infinite-flexibility weight-b cardinality-b)}
        (cond
          [(> cardinality-a cardinality-b) a]
          [(< cardinality-a cardinality-b) b]
          [else (fill (+ weight-a weight-b) cardinality-a)])]
       [{(? infinite-flexibility?) _} a]
       [{_ (? infinite-flexibility?)} b]
       [{(font-relative-size ems-a pxs-a)
         (font-relative-size ems-b pxs-b)}
        (make-size (+ ems-a ems-b)
                   (+ pxs-a pxs-b))]
       [{(font-relative-size ems pxs-a) pxs-b}
        (font-relative-size ems (+ pxs-a pxs-b))]
       [{pxs-a (font-relative-size ems pxs-b)}
        (font-relative-size ems (+ pxs-a pxs-b))]
       [{pxs-a pxs-b}
        (+ pxs-a pxs-b)])]
    [(s . ss)
     (for/fold ([a s])
               ([b (in-list ss)])
       (size+ a b))]))

(define size-
  (case-lambda
    [(s)
     (match s
       [(infinite-flexibility weight cardinality)
        (infinite-flexibility (- weight) cardinality)]
       [(font-relative-size ems pxs)
        (font-relative-size (- ems) (- pxs))]
       [pxs (- pxs)])]
    [(a b)
     (size+ a (size- b))]
    [(s . ss)
     (for/fold ([a s])
               ([b (in-list ss)])
       (size- a b))]))

(define size*
  (case-lambda
    [(s) s]
    [(s . ns)
     (match s
       [(infinite-flexibility weight cardinality)
        (fill (apply * weight ns) cardinality)]
       [(font-relative-size ems pxs)
        (define n (apply * ns))
        (make-size (* ems n) (* pxs n))]
       [pxs (apply * pxs ns)])]))

(define (size/ s . ns)
  (match s
    [(infinite-flexibility weight cardinality)
     (fill (apply / weight ns) cardinality)]
    [(font-relative-size ems pxs)
     (define n (apply * ns))
     (make-size (/ ems n) (/ pxs n))]
    [pxs (apply / pxs ns)]))
