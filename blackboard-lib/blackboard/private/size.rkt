#lang racket/base

(require racket/match
         racket/contract
         racket/fixnum
         threading
         "util/print.rkt")

(provide (contract-out
          ;; size
          [size? predicate/c]
          [absolute-size? predicate/c]
          [font-relative-size? predicate/c]
          [size-ems (-> size? rational?)]
          [size-pxs (-> size? absolute-size?)]
          [ems (-> rational? size?)]

          [size-zero? (-> size? boolean?)]
          [size= (-> size? size? boolean?)]
          [size+ (-> size? ... size?)]
          [size- (-> size? size? ... size?)]
          [size* (-> size? rational? ... size?)]
          [make-size-absolute (-> size?
                                  #:ppem absolute-size?
                                  absolute-size?)]

          ;; flexibility
          [flexibility? flat-contract?]
          [infinite-flexibility? predicate/c]
          [infinite-flexibility-cardinality? predicate/c]
          [infinite-flexibility-weight (-> infinite-flexibility? (and/c rational? positive?))]
          [infinite-flexibility-cardinality (-> infinite-flexibility? infinite-flexibility-cardinality?)]

          [fi (->* [] [(and/c rational? (not/c negative?))] flexibility?)]
          [fil (->* [] [(and/c rational? (not/c negative?))] flexibility?)]
          [fill (->* [] [(and/c rational? (not/c negative?)) infinite-flexibility-cardinality?] flexibility?)]
          [filll (->* [] [(and/c rational? (not/c negative?))] flexibility?)]

          [flexibility-zero? (-> flexibility? boolean?)]
          [flexibility= (-> flexibility? flexibility? boolean?)]
          [flexibility+ (-> flexibility? ... flexibility?)]
          [flexibility* (-> flexibility? (and/c rational? (not/c negative?)) ... flexibility?)]

          ;; flexible size
          [flexible-size? predicate/c]
          [flexible-size-basis (-> flexible-size? size?)]
          [flexible-size-grow (-> flexible-size? flexibility?)]
          [flexible-size-shrink (-> flexible-size? flexibility?)]
          [flex (->* [flexible-size?]
                     [#:grow flexibility?
                      #:shrink flexibility?]
                     flexible-size?)]

          [flexible-size-zero? (-> flexible-size? boolean?)]
          [flexible-size= (-> flexible-size? flexible-size? boolean?)]
          [flexible-size+ (-> flexible-size? ... flexible-size?)]
          [flexible-size* (-> flexible-size? (and/c rational? (not/c negative?)) ... flexible-size?)]
          [make-flexible-size-absolute (-> flexible-size?
                                           #:ppem (and/c absolute-size? (not/c negative?))
                                           absolute-size?)]))

;; -----------------------------------------------------------------------------
;; rational arithmetic

(define (check-overflow who n)
  (cond
    [(eqv? n +inf.0) (raise-arguments-error who "flonum positive overflow")]
    [(eqv? n -inf.0) (raise-arguments-error who "flonum negative overflow")]
    [else            n]))

(define (rational+ who a b) (check-overflow who (+ a b)))
(define (rational* who a b) (check-overflow who (* a b)))

;; -----------------------------------------------------------------------------
;; fixed size

(define (size? v)
  (or (absolute-size? v)
      (font-relative-size? v)))

(define (absolute-size? v)
  (rational? v))

(struct font-relative-size (ems pxs)
  #:property prop:equal+hash
  (let ()
    (define (=? a b recur)
      (and (recur (font-relative-size-ems a) (font-relative-size-ems b))
           (recur (font-relative-size-pxs a) (font-relative-size-pxs b))))
    (define (hash self recur)
      (recur (cons (font-relative-size-ems self)
                   (font-relative-size-pxs self))))
    (list =? hash hash))
  #:property prop:custom-write
  (λ (self out mode)
    (match self
      [(font-relative-size ems (? zero?))
       #:when (eq? mode 0)
       (print (quasiexpr (ems ,ems)) out)]
      [_
       (write-string "#<size: " out)
       (write-size-expression self out)
       (write-string ">" out)])))

(define (write-size-expression s out)
  (match s
    [(font-relative-size ems pxs)
     (if (zero? pxs)
         (fprintf out "~aem" ems)
         (fprintf out
                  "~aem ~a ~apx"
                  ems
                  (if (positive? pxs) "+" "-")
                  (abs pxs)))]
    [0
     (write-string "0" out)]
    [pxs
     (fprintf out "~apx" pxs)]))

(define (make-size ems pxs)
  (if (zero? ems)
      pxs
      (font-relative-size ems pxs)))

(define (size-pxs v)
  (if (font-relative-size? v)
      (font-relative-size-pxs v)
      v))

(define (size-ems v)
  (if (font-relative-size? v)
      (font-relative-size-ems v)
      0))

(define (ems n) (make-size n 0))

(define (size-zero? s)
  (and (absolute-size? s) (zero? s)))

(define (size= a b)
  (match* {a b}
    [{(? absolute-size?) (? absolute-size?)}
     (= a b)]
    [{(font-relative-size ems-a pxs-a) (font-relative-size ems-b pxs-b)}
     (and (= ems-a ems-b) (= pxs-a pxs-b))]
    [{_ _}
     #f]))

(define (size+/who who a b)
  (match* {a b}
    [{(font-relative-size ems-a pxs-a)
      (font-relative-size ems-b pxs-b)}
     (make-size (rational+ who ems-a ems-b) (rational+ who pxs-a pxs-b))]
    [{(font-relative-size ems pxs-a) pxs-b}
     (font-relative-size ems (rational+ who pxs-a pxs-b))]
    [{pxs-a (font-relative-size ems pxs-b)}
     (font-relative-size ems (rational+ who pxs-a pxs-b))]
    [{pxs-a pxs-b}
     (rational+ who pxs-a pxs-b)]))

(define size+
  (case-lambda
    [() 0]
    [(s) s]
    [(a b)
     (size+/who 'size+ a b)]
    [(s . ss)
     (for/fold ([a s])
               ([b (in-list ss)])
       (size+ a b))]))

(define size-
  (case-lambda
    [(s)
     (match s
       [(font-relative-size ems pxs)
        (font-relative-size (- ems) (- pxs))]
       [pxs (- pxs)])]
    [(a b)
     (size+/who 'size- a (size- b))]
    [(s . ss)
     (for/fold ([a s])
               ([b (in-list ss)])
       (size- a b))]))

(define (size*/who who s n)
  (match s
    [(font-relative-size ems pxs)
     (make-size (rational* who ems n) (rational* who pxs n))]
    [pxs (rational* who pxs n)]))

(define size*
  (case-lambda
    [(s) s]
    [(s n)
     (size*/who 'size* s n)]
    [(s . ns)
     (size* s (apply * ns))]))

(define (make-size-absolute s #:ppem ppem #:who [who 'make-size-absolute])
  (match s
    [(font-relative-size ems pxs)
     (check-overflow who (+ pxs (* ems ppem)))]
    [pxs pxs]))

;; -----------------------------------------------------------------------------
;; flexibility

(define (infinite-flexibility-cardinality? v)
  (and (fixnum? v)
       (fx<= 0 v 3)))

(struct infinite-flexibility (weight cardinality)
  #:property prop:equal+hash
  (let ()
    (define (=? a b recur)
      (and (recur (infinite-flexibility-weight a) (infinite-flexibility-weight b))
           (fx= (infinite-flexibility-cardinality a) (infinite-flexibility-cardinality b))))
    (define (hash self recur)
      (recur (cons (infinite-flexibility-weight self)
                   (infinite-flexibility-cardinality self))))
    (list =? hash hash))
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (self out mode)
    (cond
      [(eq? mode 0)
       (match-define (infinite-flexibility weight cardinality) self)
       (~> (match cardinality
             [0 (quasiexpr (fi    ,weight))]
             [1 (quasiexpr (fil   ,weight))]
             [2 (quasiexpr (fill  ,weight))]
             [3 (quasiexpr (filll ,weight))])
           (print out))]
      [else
       (write-string "#<flexibility: " out)
       (write-flexibility-expression self out)
       (write-string ">" out)])))

(define (write-flexibility-expression s out)
  (match s
    [(infinite-flexibility weight cardinality)
     (display weight out)
     (write-string (match cardinality
                     [0 "fi"]
                     [1 "fil"]
                     [2 "fill"]
                     [3 "filll"])
                   out)]
    [_
     (write-size-expression s out)]))

(define (fill [weight 1] [cardinality 2])
  (if (zero? weight)
      weight
      (infinite-flexibility weight cardinality)))

(define (fi [weight 1])
  (fill weight 0))
(define (fil [weight 1])
  (fill weight 1))
(define (filll [weight 1])
  (fill weight 3))

(define flexibility?
  (flat-contract-with-explanation
   #:name 'flexibility?
   (λ (v)
     (match v
       [(? absolute-size?)
        (or (>= v 0)
            (λ (blame)
              (raise-blame-error
               blame v
               '(expected "a flexibility, which must not be negative"
                          given: "~e")
               v)))]

       [(font-relative-size ems pxs)
        (or (and (>= ems 0) (>= pxs 0))
            (λ (blame)
              (define depends-on-ppem? (or (positive? ems) (positive? pxs)))
              (raise-blame-error
               blame v
               '(expected "a flexibility, which must not be negative"
                 given: "~e~a")
               v (if depends-on-ppem? ",\n    which could be negative (depending on the font size)" ""))))]

       [(? infinite-flexibility?) #t]
       [_ #f]))))

(define (absolute-flexibility? v)
  (or (absolute-size? v)
      (infinite-flexibility? v)))

(define (font-relative-flexibility? v)
  (font-relative-size? v))

(define (flexibility-zero? v)
  (and (absolute-size? v) (size-zero? v)))

(define (flexibility= a b)
  (match* {a b}
    [{(? absolute-size?) (? absolute-size?)}
     (flexible-size= a b)]
    [{(infinite-flexibility weight-a cardinality-a) (infinite-flexibility weight-b cardinality-b)}
     (and (= weight-a weight-b)
          (fx= cardinality-a cardinality-b))]))

(define (flexibility+/who who a b)
  (match* {a b}
    [{(infinite-flexibility weight-a cardinality-a)
      (infinite-flexibility weight-b cardinality-b)}
     (cond
       [(fx> cardinality-a cardinality-b) a]
       [(fx< cardinality-a cardinality-b) b]
       [else (fill (rational+ who weight-a weight-b) cardinality-a)])]
    [{(? infinite-flexibility?) _} a]
    [{_ (? infinite-flexibility?)} b]
    [{_ _} (size+/who who a b)]))

(define flexibility+
  (case-lambda
    [() 0]
    [(s) s]
    [(a b)
     (flexibility+/who 'flexibility+ a b)]
    [(s . ss)
     (for/fold ([a s])
               ([b (in-list ss)])
       (flexibility+ a b))]))

(define (flexibility*/who who s n)
  (match s
    [(infinite-flexibility weight cardinality)
     (fill (rational* who weight n) cardinality)]
    [_ (size*/who who s n)]))

(define flexibility*
  (case-lambda
    [(s) s]
    [(s n)
     (flexibility*/who 'flexibility* s n)]
    [(s . ns)
     (flexibility* s (apply * ns))]))

(define (make-flexibility-absolute s #:ppem ppem #:who [who 'make-size-absolute])
  (if (infinite-flexibility? s)
      s
      (make-size-absolute s #:ppem ppem #:who who)))

;; -----------------------------------------------------------------------------
;; flexible size

(struct flexible-size-struct (basis grow shrink)
  #:name flexible-size
  #:constructor-name flexible-size
  #:reflection-name 'flexible-size
  #:property prop:equal+hash
  (let ()
    (define (=? a b recur)
      (and (recur (flexible-size-struct-basis a) (flexible-size-struct-basis b))
           (recur (flexible-size-struct-grow a) (flexible-size-struct-grow b))
           (recur (flexible-size-struct-shrink a) (flexible-size-struct-shrink b))))
    (define (hash self recur)
      (recur (vector-immutable
              (flexible-size-struct-basis self)
              (flexible-size-struct-grow self)
              (flexible-size-struct-shrink self))))
    (list =? hash hash))
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (self out mode)
    (match-define (flexible-size basis grow shrink) self)
    (cond
      [(eq? mode 0)
       (print (quasiexpr
               (flex ,basis
                     {~if (not (flexibility-zero? grow)) {~seq #:grow ,grow}}
                     {~if (not (flexibility-zero? shrink)) {~seq #:shrink ,shrink}}))
              out)]
      [else
       (write-string "#<flexible-size: " out)
       (write-size-expression basis out)

       (unless (flexibility-zero? grow)
         (write-string " grow: " out)
         (write-flexibility-expression grow out))

       (unless (flexibility-zero? shrink)
         (write-string " shrink: " out)
         (write-flexibility-expression shrink out))

       (write-string ">" out)])))

(define (flex basis #:grow [grow 0] #:shrink [shrink 0])
  (if (and (flexibility-zero? grow)
           (flexibility-zero? shrink))
      basis
      (match basis
        [(flexible-size old-basis old-grow old-shrink)
         (flex old-basis
               #:grow (flexibility+ old-grow grow)
               #:shrink (flexibility+ old-shrink shrink))]
        [_ (flexible-size basis grow shrink)])))

(define (flexible-size? v)
  (or (size? v)
      (flexible-size-struct? v)))

(define (flexible-size-basis v)
  (if (flexible-size-struct? v)
      (flexible-size-struct-basis v)
      v))

(define (flexible-size-grow v)
  (if (flexible-size-struct? v)
      (flexible-size-struct-grow v)
      0))

(define (flexible-size-shrink v)
  (if (flexible-size-struct? v)
      (flexible-size-struct-shrink v)
      0))

(define (flexible-size-zero? v)
  (and (size? v) (size-zero? v)))

(define (flexible-size= a b)
  (match* {a b}
    [{(? size?) (? size?)}
     (size= a b)]
    [{(flexible-size basis-a grow-a shrink-a) (flexible-size basis-b grow-b shrink-b)}
     (and (flexible-size= basis-a basis-b)
          (flexibility= grow-a grow-b)
          (flexibility= shrink-a shrink-b))]
    [{_ _}
     #f]))

(define flexible-size+
  (case-lambda
    [() 0]
    [(s) s]
    [(a b)
     (match* {a b}
       [{(flexible-size basis-a grow-a shrink-a)
         (flexible-size basis-b grow-b shrink-b)}
        (flexible-size (size+/who 'flexible-size+ basis-a basis-b)
                       (flexibility+/who 'flexible-size+ grow-a grow-b)
                       (flexibility+/who 'flexible-size+ shrink-a shrink-b))]
       [{(flexible-size basis-a grow shrink) basis-b}
        (flexible-size (size+/who 'flexible-size+ basis-a basis-b) grow shrink)]
       [{basis-a (flexible-size basis-b grow shrink)}
        (flexible-size (size+/who 'flexible-size+ basis-a basis-b) grow shrink)]
       [{_ _}
        (size+/who 'flexible-size+ a b)])]
    [(s . ss)
     (for/fold ([a s])
               ([b (in-list ss)])
       (flexible-size+ a b))]))

(define flexible-size*
  (case-lambda
    [(s) s]
    [(s n)
     (match s
       [(flexible-size basis grow shrink)
        (flex (size*/who 'flexible-size* basis n)
              #:grow (flexibility*/who 'flexible-size* grow n)
              #:shrink (flexibility*/who 'flexible-size* shrink n))]
       [_ (size*/who 'flexible-size* s n)])]
    [(s . ns)
     (flexible-size* s (apply * ns))]))

(define (make-flexible-size-absolute s #:ppem ppem)
  (match s
    [(flexible-size basis grow shrink)
     (flex (make-size-absolute basis #:ppem ppem #:who 'make-flexible-size-absolute)
           #:grow (make-flexibility-absolute grow #:ppem ppem #:who 'make-flexible-size-absolute)
           #:shrink (make-flexibility-absolute shrink #:ppem ppem #:who 'make-flexible-size-absolute))]
    [_
     (make-size-absolute s #:ppem ppem #:who 'make-flexible-size-absolute)]))
