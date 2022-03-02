#lang racket/base

(require racket/contract
         racket/hash
         racket/list
         racket/match
         racket/string
         threading
         "util/print.rkt")

(provide style-property?
         style-property/c
         (contract-out
          [make-style-property
           (->* [symbol?]
                [#:default any/c
                 #:combine (or/c (-> any/c any/c any/c) #f)]
                style-property?)]
          [make-inherited-style-property
           (->* [symbol?]
                [#:default any/c
                 #:combine (-> any/c any/c any/c)]
                style-property?)])
         style?
         style
         plain
         computed-style?
         (contract-out
          [style-add (-> style? style-property? any/c style?)]
          [combine-styles (-> style? ... style?)]
          [compute-style (-> (or/c computed-style? #f) style? computed-style?)]
          [computed-style-value (-> computed-style? style-property? any/c)]))

;; -----------------------------------------------------------------------------
;; styles and style properties

(define-values [impersonator-prop:impersonator-of
                has-impersonator-target?
                impersonator-target]
  (make-impersonator-property 'impersonator-of))

(struct style-property-key (name default combine-proc)
  #:name info:style-property-key
  #:constructor-name unused-make-style-property-key)

(struct uninherited-style-property-key info:style-property-key ()
  #:constructor-name make-uninherited-style-property-key)
(struct inherited-style-property-key info:style-property-key ()
  #:constructor-name make-inherited-style-property-key)

;; For when we want to print something that references a style property,
;; but we actually only have a style property key.
(define (wrap-key-for-print key)
  (unquoted-printing-string
   (format "#<style-property:~a>" (style-property-key-name key))))

(struct style-property (key guard-proc extract-proc)
  #:constructor-name internal-make-style-property
  #:property prop:impersonator-of (λ (self) (impersonator-target self #f))
  #:property prop:object-name
  (λ (self) (style-property-key-name (style-property-key self)))
  #:property prop:custom-write
  (λ (self out mode)
    (fprintf out "#<style-property:~a>" (object-name self))))

(define (make-style-property-from-key key)
  (internal-make-style-property key (λ (val) val) (λ (val) val)))

(define (make-style-property name
                             #:default [default #f]
                             #:combine [combine-proc #f])
  (make-style-property-from-key
   (make-uninherited-style-property-key name default combine-proc)))

(define (make-inherited-style-property name
                                       #:default [default #f]
                                       #:combine [combine-proc (λ (a b) b)])
  (make-style-property-from-key
   (make-inherited-style-property-key name default combine-proc)))

(define (print-style-mapping what mapping out mode #:as-expression? [try-as-expression? #f])
  (define sorted-mapping (sort mapping symbol<? #:key car))
  (define as-expression? (and try-as-expression?
                              (eq? mode 0)
                              (for/and ([key+vals (in-list sorted-mapping)])
                                (= (length (cdr key+vals)) 1))))
  (cond
    [(empty? mapping)
     (if as-expression?
         (write-string "plain" out)
         (fprintf out "#<~a:plain>" what))]
    [else
     (~> (for/list ([key+vals (in-list sorted-mapping)])
           (define entry (cons (unquoted-printing-string (format "~s" (car key+vals)))
                               (match (cdr key+vals)
                                 [(list val) (list val)]
                                 [vals (reverse vals)])))
           (if as-expression?
               (printing-sequence entry)
               (delimited-printing-sequence #:before "[" #:after "]" entry)))
         (cons (unquoted-printing-string what) _)
         (delimited-printing-sequence #:before (if as-expression? "(" "#<")
                                      #:after (if as-expression? ")" ">"))
         (custom-write/recur out mode))]))

(struct style (mapping)
  #:name info:style
  #:constructor-name make-style
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (self out mode)
    (~> (for/list ([(key vals) (in-immutable-hash (style-mapping self))])
          (cons (style-property-key-name key) vals))
        (print-style-mapping "style" _ out mode #:as-expression? #t))))

(struct computed-style (inherited uninherited)
  #:constructor-name make-computed-style
  #:property prop:custom-write
  (λ (self out mode)
    (~> (append (for/list ([(key val) (in-immutable-hash (computed-style-inherited self))])
                  (cons (style-property-key-name key) (list val)))
                (for/list ([(key val) (in-immutable-hash (computed-style-uninherited self))])
                  (cons (style-property-key-name key) (list val))))
        (print-style-mapping "computed-style" _ out mode))))

(define plain (make-style (hasheq)))

(define (style . args)
  (unless (even? (length args))
    (raise-arguments-error
     'style
     "expected an even number of arguments, but received an odd number"
     "arguments" args))

  (let loop ([s plain]
             [args args])
    (match args
      ['() s]
      [(list* prop val args)
       (unless (style-property? prop)
         (raise-argument-error 'style "style-property?" prop))
       (loop (style-add s prop val) args)])))

(define (style-add s prop val)
  (define key (style-property-key prop))
  (define val* ((style-property-guard-proc prop) val))
  (struct-copy info:style s
               [mapping (hash-update (style-mapping s)
                                     key
                                     (λ~>> (cons val*))
                                     '())]))

(define combine-styles
  (case-lambda
    [() plain]
    [(s) s]
    [ss (make-style (apply hash-union (map style-mapping ss) #:combine append))]))

(define (compute-style old new)
  (for/fold ([inherited (if old (computed-style-inherited old) (hasheq))]
             [uninherited (hasheq)]
             #:result (make-computed-style inherited uninherited))
            ([(key vals) (in-immutable-hash (style-mapping new))])
    (define default (style-property-key-default key))
    (define combine (style-property-key-combine-proc key))

    (define (go h)
      (cond
        [combine
         (hash-update
          h
          key
          (λ (old)
            (for/fold ([old old])
                      ([new (in-list vals)])
              (combine old new)))
          (λ () default))]
        [else
         (match vals
           [(list val)
            (hash-set h key val)]
           [_
            (raise-arguments-error 'compute-style "multiple values provided for single-valued style property"
                                   "property" (wrap-key-for-print key)
                                   "values..." (unquoted-printing-string
                                                (string-append*
                                                 (for/list ([val (in-list vals)])
                                                   (format "\n   ~e" val)))))])]))

    (if (inherited-style-property-key? key)
        (values (go inherited) uninherited)
        (values inherited (go uninherited)))))

(define (computed-style-value s prop)
  (define key (style-property-key prop))
  (define val (hash-ref (if (inherited-style-property-key? key)
                            (computed-style-inherited s)
                            (computed-style-uninherited s))
                        key
                        (λ () (style-property-key-default key))))
  ((style-property-extract-proc prop) val))

;; -----------------------------------------------------------------------------
;; style-property/c

(define (chaperone-style-property val
                                  #:guard wrap-guard
                                  #:extract wrap-extract
                                  . props)
  (apply chaperone-struct
         val
         style-property-guard-proc
         (λ (val guard-proc) (chaperone-procedure guard-proc wrap-guard))
         style-property-extract-proc
         (λ (val extract-proc) (chaperone-procedure extract-proc wrap-extract))
         props))

(define (impersonate-style-property val
                                    #:guard wrap-guard
                                    #:extract wrap-extract
                                    . props)
  (apply impersonate-struct
         (struct-copy style-property val
                      [guard-proc (impersonate-procedure (style-property-guard-proc val)
                                                         wrap-guard)]
                      [extract-proc (impersonate-procedure (style-property-extract-proc val)
                                                           wrap-extract)])
         struct:style-property
         impersonator-prop:impersonator-of val
         props))

(define (make-style-property-contract-property build-contract-property wrap-style-property)
  (define ((relation <?) a b)
    (and (style-property-contract? b)
         (<? (style-property-contract-in-ctc a)
             (style-property-contract-in-ctc b))))

  (build-contract-property
   #:name (λ (self)
            (define in-ctc (style-property-contract-in-ctc self))
            (define out-ctc (style-property-contract-out-ctc self))
            (if out-ctc
                (build-compound-type-name 'style-property/c in-ctc out-ctc)
                (build-compound-type-name 'style-property/c in-ctc)))
   #:first-order (λ (self) style-property?)
   #:stronger (relation contract-stronger?)
   #:equivalent (relation contract-equivalent?)
   #:late-neg-projection
   (λ (self)
     (define in-proc (get/build-late-neg-projection (style-property-contract-in-ctc self)))
     (define out-ctc (style-property-contract-out-ctc self))
     (define out-proc (if out-ctc (get/build-late-neg-projection out-ctc) in-proc))
     (λ (blame)
       (define (add-context blame swap?)
         (blame-add-context blame "the style property value of" #:swap? swap?))
       ; This should technically use `contract-pos/neg-doubling`, but cooperating
       ; with it is really annoying, and it probably doesn’t matter.
       (define in-proj (in-proc (add-context blame #t)))
       (define out-proj (out-proc (add-context blame #f)))

       (λ (val neg-party)
         (unless (style-property? val)
           (raise-blame-error
            blame val #:missing-party neg-party
            (list 'expected: "style-property?" 'given: "~e") val))

         (define blame+neg-party (cons blame neg-party))

         (define (wrap-guard val)
           (with-contract-continuation-mark blame+neg-party
             (in-proj val neg-party)))

         (define (wrap-extract val)
           (values (λ (val) (with-contract-continuation-mark blame+neg-party
                              (out-proj val neg-party)))
                   val))

         (wrap-style-property
          val #:guard wrap-guard #:extract wrap-extract
          impersonator-prop:contracted self
          impersonator-prop:blame blame+neg-party))))))

(struct style-property-contract (in-ctc out-ctc)
  #:property prop:custom-write contract-custom-write-property-proc)
(struct impersonator-style-property-contract style-property-contract ()
  #:property prop:contract
  (make-style-property-contract-property build-contract-property
                                         impersonate-style-property))
(struct chaperone-style-property-contract style-property-contract ()
  #:property prop:chaperone-contract
  (make-style-property-contract-property build-chaperone-contract-property
                                         chaperone-style-property))

(define (style-property/c in-ctcish [out-ctcish #f])
  (define in-ctc (coerce-contract 'style-property/c in-ctcish))
  (define out-ctc (and out-ctcish (coerce-contract 'style-property/c out-ctcish)))
  (if (and (chaperone-contract? in-ctc)
           (or (not out-ctc) (chaperone-contract? out-ctc)))
      (chaperone-style-property-contract in-ctc out-ctc)
      (impersonator-style-property-contract in-ctc out-ctc)))
