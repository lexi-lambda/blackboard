#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/contract
         racket/list
         syntax/parse/define)

(provide define-uniques
         define-struct-type-property
         (contract-out
          [make-unique (-> symbol? (values any/c predicate/c))]
          [make-standard-struct-type-property
           (->* [symbol?]
                [#:wrap-guard wrap-guard/c
                 #:value-contract contract?
                 #:allow-procedure? any/c
                 #:allow-plain-value? any/c
                 #:allow-field-index? any/c
                 #:supers (listof (cons/c struct-type-property? (-> any/c any/c)))
                 #:can-impersonate? any/c]
                (values struct-type-property?
                        procedure?
                        procedure?))]))

;; -----------------------------------------------------------------------------

(define (make-unique name)
  (struct unique ()
    #:authentic
    #:reflection-name name)
  (values (unique) unique?))

(define-simple-macro (define-unique name:id)
  #:with name? (format-id #'name "~a?" #'name #:subs? #t)
  (define-values [name name?] (make-unique 'name)))

(define-simple-macro (define-uniques name:id ...)
  (begin (define-unique name) ...))

;; -----------------------------------------------------------------------------

(define struct-type-info-list/c
  (flat-named-contract
   'struct-type-info-list/c
   (list/c symbol?
           exact-nonnegative-integer?
           exact-nonnegative-integer?
           struct-accessor-procedure?
           struct-mutator-procedure?
           (listof exact-nonnegative-integer?)
           (or/c struct-type? #f)
           boolean?)))

(define wrap-guard/c
  (-> any/c
      struct-type-info-list/c
      (-> any/c struct-type-info-list/c (-> any/c any/c))
      any/c))

(define (make-standard-struct-type-property
         name
         #:wrap-guard [wrap-guard (λ (v info guard) (guard v info))]
         #:value-contract [value/c any/c]
         #:allow-procedure? [allow-procedure? #t]
         #:allow-plain-value? [allow-plain-value? #t]
         #:allow-field-index? [allow-field-index? #t]
         #:supers [supers '()]
         #:can-impersonate? [can-impersonate? #f])
  (define-values [prop:name name? name-ref]
    (make-struct-type-property
     name
     (λ (v info)
       (wrap-guard
        v info
        (λ (v info)
          (cond
            [(and allow-procedure? (procedure? v))
             v]
            [(and allow-field-index? (exact-integer? v))
             (define self-ref (fourth info))
             (λ (self) (self-ref self v))]
            [else
             (λ (self) v)]))))
     supers
     can-impersonate?))
  (values prop:name name? (procedure-rename (λ (self) ((name-ref self) self)) name)))

(define-syntax-parser define-struct-type-property
  [(_ {~optional name:id}
      {~alt {~optional {~seq #:property-id property-id:id}}
            {~optional {~seq #:predicate-id predicate-id:id}}
            {~optional {~seq #:accessor-id accessor-id:id}}
            {~optional {~seq #:reflection-name reflection-name}}
            {~optional {~seq #:wrap-guard wrap-guard}}
            {~optional {~or* {~and #:disallow-procedure {~bind [allow-procedure? #'#f]}}
                             {~seq #:allow-procedure? allow-procedure?:expr}}}
            {~optional {~or* {~and #:disallow-plain-value {~bind [allow-plain-value? #'#f]}}
                             {~seq #:allow-plain-value? allow-plain-value?:expr}}}
            {~optional {~or* {~and #:disallow-field-index {~bind [allow-field-index? #'#f]}}
                             {~seq #:allow-field-index? allow-field-index?:expr}}}
            {~seq #:super super-prop super-proc}
            {~optional {~or* {~and #:can-impersonate {~bind [can-impersonate? #'#t]}}
                             {~seq #:can-impersonate? can-impersonate?:expr}}}}
      ...)
   #:declare reflection-name (expr/c #'symbol? #:name "#:reflection-name argument")
   #:declare wrap-guard (expr/c #'wrap-guard/c #:name "#:wrap-guard argument")
   #:declare super-prop (expr/c #'struct-type-property? #:name "#:super argument")
   #:declare super-proc (expr/c #'(-> any/c any/c) #:name "#:super argument")
   #:fail-unless (or (attribute name)
                     (and (attribute property-id)
                          (attribute predicate-id)
                          (attribute accessor-id)
                          (attribute reflection-name)))
                 (string-append "either a name identifier must be specified, or the "
                                "#:property-id, #:predicate-id, #:accessor-id, and "
                                "#:reflection-name options must all be provided")
   #:with {~var prop:name} (or (attribute property-id)
                               (format-id #'name "prop:~a" #'name #:subs? #t))
   #:with name? (or (attribute predicate-id)
                    (format-id #'name "~a?" #'name #:subs? #t))
   #:with name-ref (or (attribute accessor-id)
                       (format-id #'name "~a-ref" #'name #:subs? #t))
   (syntax/loc this-syntax
     (define-values [prop:name name? name-ref]
       (make-standard-struct-type-property {~? reflection-name.c 'name}
                                           {~? {~@ #:wrap-guard wrap-guard.c}}
                                           {~? {~@ #:allow-procedure? allow-procedure?}}
                                           {~? {~@ #:allow-plain-value? allow-plain-value?}}
                                           {~? {~@ #:allow-field-index? allow-field-index?}}
                                           #:supers (list (cons super-prop.c super-proc.c) ...)
                                           {~? {~@ #:can-impersonate? can-impersonate?}})))])
