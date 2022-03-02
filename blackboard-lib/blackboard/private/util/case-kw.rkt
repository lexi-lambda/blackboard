#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/set
                     racket/syntax
                     syntax/parse/lib/function-header
                     threading)
         racket/contract
         (only-in racket/contract/private/guts
                  nth-argument-of
                  nth-case-of)
         racket/format
         racket/list
         racket/match
         racket/set
         racket/string
         syntax/parse/define
         threading)

(provide case-kw-lambda
         case-kw->)

;; -----------------------------------------------------------------------------
;; case-kw-lambda

(define unsupplied-case-kw-arg (gensym 'unsupplied-case-kw-arg))

(define (raise-no-cases-matched-error who args kws kw-args)
  (define kws+args (sort (map cons kws kw-args) keyword<? #:key car))
  (raise (exn:fail:contract:arity
          (apply string-append
                 (symbol->string who) ": arity mismatch;\n"
                 " no case matching the given arguments\n"
                 "  arguments...:"
                 (append (for/list ([arg (in-list args)]
                                    #:break (eq? arg unsupplied-case-kw-arg))
                           (format "\n   ~e" arg))
                         (for/list ([kw+arg (in-list kws+args)]
                                    #:unless (eq? (cdr kw+arg) unsupplied-case-kw-arg))
                           (format "\n   ~a ~e" (car kw+arg) (cdr kw+arg)))))
          (current-continuation-marks))))

(begin-for-syntax
  (define unsupplied-arg-pat #'(== unsupplied-case-kw-arg eq?))
  (define (supplied-arg-pat stx) #`(and (not #,unsupplied-arg-pat) #,stx))

  (define (make-arity-mask #:req req-args #:opt opt-args #:rest? rest?)
    (arithmetic-shift (if rest?
                          -1
                          (bitwise-not (arithmetic-shift -1 (add1 opt-args))))
                      req-args))

  (define-syntax-class case
    #:attributes [formals {formal 1} {formal.name 1} {formal.kw 1} {formal.default 1}
                          rest {body 1} any-kw? any-opt?]
    #:commit
    (pattern [{~and formals (formal:formal ... . {~or* rest:id ()})} body ...+]
      #:attr any-kw? (and (ormap values (attribute formal.kw)) #t)
      #:attr any-opt? (and (ormap values (attribute formal.default)) #t))))

;; Like `case-lambda`, but accepts optional and keyword arguments like `lambda`.
(define-syntax-parser case-kw-lambda
  ; No cases: expand to ordinary `case-lambda`.
  [(_)
   (syntax/loc this-syntax
     (case-lambda))]

  ; Single case: expand to ordinary `lambda`.
  [(_ case:case)
   (syntax/loc this-syntax
     (lambda case.formals case.body ...))]

  [(_ case:case ...+)
   (define any-kw? (ormap values (attribute case.any-kw?)))
   (define any-opt? (ormap values (attribute case.any-opt?)))
   (cond
     ; No keywords or optional arguments: expand to ordinary `case-lambda`.
     [(and (not any-kw?) (not any-opt?))
      (syntax/loc this-syntax
        (case-lambda case ...))]

     ; No keywords: expand to `case-lambda` via `case-opt-lambda`.
     [(not any-kw?)
      (syntax/loc this-syntax
        (case-opt-lambda case ...))]

     ; We have keywords, so we have to build a complicated `lambda`.
     [else
      (define any-rest? (ormap values (attribute case.rest)))

      (define-values {min-pos max-pos req-kws all-kws arity-mask}
        (for/fold ([min-pos #f]
                   [max-pos (and (not any-rest?) 0)]
                   [req-kws #f]
                   [all-kws (seteq)]
                   [arity-mask 0])
                  ([kws (in-list (attribute case.formal.kw))]
                   [defaults (in-list (attribute case.formal.default))]
                   [rest (in-list (attribute case.rest))])
          (define new-min-pos (for/sum ([kw (in-list kws)]
                                        [default (in-list defaults)]
                                        #:when (and (not kw) (not default)))
                                1))
          (define new-max-pos (count not kws))
          (define new-req-kws (for/seteq ([kw (in-list kws)]
                                          [default (in-list defaults)]
                                          #:when (and kw (not default)))
                                (syntax-e kw)))

          (values (if min-pos (min min-pos new-min-pos) new-min-pos)
                  (and max-pos (max max-pos new-max-pos))
                  (if req-kws
                      (set-intersect req-kws new-req-kws)
                      new-req-kws)
                  (for/fold ([all-kws all-kws])
                            ([kw (in-list kws)]
                             #:when kw)
                    (set-add all-kws (syntax-e kw)))
                  (bitwise-ior arity-mask (make-arity-mask #:req new-min-pos
                                                           #:opt (- new-max-pos new-min-pos)
                                                           #:rest? rest)))))

      (define opt-pos (and max-pos (- max-pos min-pos)))
      (define req-kw-lst (set->list req-kws))
      (define opt-kw-lst (set->list (set-subtract all-kws req-kws)))

      (define/syntax-parse [req-pos-id ...]
        (for/list ([i (in-range min-pos)])
          (generate-temporary 'req-pos)))
      (define/syntax-parse [opt-pos-id ...]
        (if opt-pos
            (for/list ([i (in-range opt-pos)])
              (generate-temporary 'opt-pos))
            '()))

      (define/syntax-parse [{req-kw req-kw-id} ...]
        (for/list ([req-kw (in-list req-kw-lst)])
          (list req-kw (generate-temporary req-kw))))
      (define/syntax-parse [{opt-kw opt-kw-id} ...]
        (for/list ([opt-kw (in-list opt-kw-lst)])
          (list opt-kw (generate-temporary opt-kw))))

      (define/syntax-parse {~or* #f rest-id}
        (and any-rest? (generate-temporary 'rest)))

      (define/syntax-parse [match-expr ...]
        #'[req-pos-id ...
           opt-pos-id ...
           {~? rest-id}
           req-kw-id ...
           opt-kw-id ...])
      (define/syntax-parse [ignore-pat ...] (make-list (length (attribute match-expr)) #'_))

      (define inferred-name (syntax-local-name))
      (define name-sym (cond
                         [(symbol? inferred-name) inferred-name]
                         [(identifier? inferred-name) (syntax-e inferred-name)]
                         [else (string->symbol (format "~a" inferred-name))]))

      (define lambda-expr
        (quasisyntax/loc this-syntax
          (lambda (req-pos-id ...
                   [opt-pos-id unsupplied-case-kw-arg] ...
                   {~@ req-kw req-kw-id} ...
                   {~@ opt-kw [opt-kw-id unsupplied-case-kw-arg]} ...
                   . {~? rest-id ()})
            (match* {match-expr ...}
              #,@(for/list ([ids (in-list (attribute case.formal.name))]
                            [kws (in-list (attribute case.formal.kw))]
                            [defaults (in-list (attribute case.formal.default))]
                            [rest-id (in-list (attribute case.rest))]
                            [bodies (in-list (attribute case.body))])
                   (define-values {req-pos-ids opt-pos-ids req-kw-ids opt-kw-ids opt-binds}
                     (for/fold ([req-pos-ids '()]
                                [opt-pos-ids '()]
                                [req-kw-ids (hasheq)]
                                [opt-kw-ids (hasheq)]
                                ; We need to combine *all* the optional bindings into a
                                ; single list, because default expressions can refer to
                                ; earlier optional bindings, so we must take care to build
                                ; and evaluate those bindings in the same order they
                                ; appeared in the input syntax.
                                [opt-binds '()]
                                #:result (values (reverse req-pos-ids)
                                                 (reverse opt-pos-ids)
                                                 req-kw-ids
                                                 opt-kw-ids
                                                 (reverse opt-binds)))
                               ([id (in-list ids)]
                                [kw (in-list kws)]
                                [default (in-list defaults)])

                       (cond
                         [default
                          (define opt-id (generate-temporary id))
                          (define opt-bind #`[#,id (if (eq? #,opt-id unsupplied-case-kw-arg)
                                                       #,default
                                                       #,opt-id)])
                          (if kw
                              (values req-pos-ids
                                      opt-pos-ids
                                      req-kw-ids
                                      (hash-set opt-kw-ids (syntax-e kw) opt-id)
                                      (cons opt-bind opt-binds))
                              (values req-pos-ids
                                      (cons opt-id opt-pos-ids)
                                      req-kw-ids
                                      opt-kw-ids
                                      (cons opt-bind opt-binds)))]
                         [else
                          (if kw
                              (values req-pos-ids
                                      opt-pos-ids
                                      (hash-set req-kw-ids (syntax-e kw) id)
                                      opt-kw-ids
                                      opt-binds)
                              (values (cons id req-pos-ids)
                                      opt-pos-ids
                                      req-kw-ids
                                      opt-kw-ids
                                      opt-binds))])))
                   (define-values [req-req-pos-ids req-opt-pos-ids] (split-at req-pos-ids min-pos))
                   (define consumed-opt-pos (+ (length req-opt-pos-ids) (length opt-pos-ids)))
                   (define wrapped-body #`(let* #,opt-binds #,@bodies))

                   #`[{#,@req-req-pos-ids
                       #,@(if any-rest?
                              (list #`(list* #,@req-opt-pos-ids
                                             #,(if rest-id
                                                   #'opts+rest
                                                   #`(? (lambda (lst)
                                                          ; This case doesn’t take a rest argument, so make
                                                          ; sure we don’t have more arguments than it can handle.
                                                          (<= (length lst) '#,(length opt-pos-ids)))
                                                        opts+rest))))
                              (append (map supplied-arg-pat req-opt-pos-ids)
                                      opt-pos-ids
                                      (make-list (- opt-pos consumed-opt-pos) unsupplied-arg-pat)))
                       #,@(for/list ([kw (in-list req-kw-lst)])
                            (hash-ref req-kw-ids kw))
                       #,@(for/list ([kw (in-list opt-kw-lst)])
                            (cond
                              [(hash-ref req-kw-ids kw #f) => supplied-arg-pat]
                              [(hash-ref opt-kw-ids kw #f) => values]
                              [else unsupplied-arg-pat]))}
                      #,(cond
                          [any-rest?
                           ; Suppose we have a case that looks like this:
                           ;
                           ;     [([a 1] [b 2] [c 3] . more) ....]
                           ;
                           ; Since there’s a rest argument, *all* of the optional
                           ; arguments are delivered as a list, which is bound in the
                           ; match pattern as `opts+rest`. We need to unpack the first
                           ; three elements of that list into bindings `a`, `b`, and `c`,
                           ; but the list could have fewer than three elements, in which
                           ; case some of the bindings need to be replaced with
                           ; unsupplied-case-kw-arg.
                           ;
                           ; Arranging for that binding structure would be a little thorny
                           ; to do by hand, but fortunately we can let `lambda` do it for
                           ; us. We just expand into
                           ;
                           ;     (apply (lambda ([a unsupplied-case-kw-arg]
                           ;                     [b unsupplied-case-kw-arg]
                           ;                     [c unsupplied-case-kw-arg]
                           ;                     . more)
                           ;              ....)
                           ;            opt-pos-ids)
                           ;
                           ; and `lambda` will generate the necessary code.
                           (define/syntax-parse [opt-pos-id ...] opt-pos-ids)
                           #`(apply (lambda ([opt-pos-id unsupplied-case-kw-arg] ...
                                             . #,(or rest-id #'()))
                                      #,wrapped-body)
                                    opts+rest)]
                          [else
                           wrapped-body])])

              [{ignore-pat ...}
               (raise-no-cases-matched-error
                '#,name-sym
                (list* req-pos-id ... {~? rest-id (list opt-pos-id ...)})
                (list 'req-kw ... 'opt-kw ...)
                (list req-kw-id ... opt-kw-id ...))]))))

      (define annotated-expr (syntax-property lambda-expr 'inferred-name inferred-name))

      ; In many cases, our constructed lambda expression will inherently have
      ; the most specific arity it can be given, in which case we have no need
      ; to bother altering it. But in some situations, we can do better, such as
      ; in the following case:
      ;
      ;     (case-kw-lambda
      ;       [(a b)               1]
      ;       [(a b c d)           2]
      ;       [(a b c d e f #:g g) 3]
      ;
      ; We’ll generate a lambda expression like
      ;
      ;     (lambda (a b [c <u>] [d <u>] [e <u>] [f <u>] #:g [g <u>]) ....)
      ;
      ; and that lambda will be given an arity that expects anywhere between 2
      ; and 6 by-position arguments. However, no cases actually accept 3 or 5
      ; by-position arguments, so we want to explicitly reduce the arity.
      (define inherent-arity-mask (make-arity-mask #:req min-pos
                                                   #:opt opt-pos
                                                   #:rest? any-rest?))
      (if (eqv? arity-mask inherent-arity-mask)
          annotated-expr
          #`(procedure-reduce-arity-mask #,annotated-expr '#,arity-mask))])])

;; A variant of `case-lambda` that allows optional arguments but not keywords.
;; Unlike `case-kw-lambda`, this can be compiled to `case-lambda`, so we use it
;; when a use of `case-kw-lambda` doesn’t actually include any keywords.
(define-syntax-parser case-opt-lambda
  [(_ case ...)
   (define/syntax-parse ([fn-bind {simple-case ...}] ...)
     (for/list ([case (in-list (attribute case))])
       (define/syntax-parse
         [{~and formals ({~alt req-id:id
                               [opt-id:id default:expr]}
                         ... . {~or* rest-id:id ()})}
          body ...+]
         case)
       (define/syntax-parse fn-id (generate-temporary 'fn))
       (list #`[fn #,(syntax/loc this-syntax
                       (lambda formals body ...))]
             (if (attribute rest-id)
                 (list #'[(req-id ... . rest-id) (apply fn-id req-id ... rest-id)])
                 (for/list ([i (in-range (add1 (length (attribute opt-id))))])
                   (define opt-ids (take (attribute opt-id) i))
                   #`[(req-id ... #,@opt-ids) (fn-id req-id ... #,@opt-ids)])))))
   #`(let (fn-bind ...)
       #,(syntax/loc this-syntax
           (case-lambda simple-case ... ...)))])

;; -----------------------------------------------------------------------------
;; case-kw->

(begin-for-syntax
  (define current-contract-obligation-key (make-parameter #f))

  (define-syntax-class (ctc #:positive? [positive? #f])
    #:description "a contract expression"
    #:opaque
    #:attributes [ctc]
    #:commit
    (pattern {~and e:expr {~not {~literal ...}}}
      #:attr ctc #`(coerce-contract
                    'case->
                    #,(syntax-property
                       #'e
                       (if positive?
                           'racket/contract:positive-position
                           'racket/contract:negative-position)
                       (current-contract-obligation-key)))))

  (define-splicing-syntax-class arg-ctc
    #:description #f
    #:attributes [kw ctc]
    #:commit
    (pattern {~seq kw:keyword ~! :ctc})
    (pattern :ctc #:attr kw #f))

  (define-splicing-syntax-class arg-ctcs
    #:description #f
    #:attributes [{pos-ctc 1} {kw 1} {kw-ctc 1}]
    (pattern {~seq arg:arg-ctc ...}
      #:do [(define-values {pos-ctcs kws kw-ctcs}
              (for/fold ([pos-ctcs '()]
                         [kws '()]
                         [kw-ctcs '()])
                        ([kw (in-list (attribute arg.kw))]
                         [ctc (in-list (attribute arg.ctc))])
                (if kw
                    (values pos-ctcs (cons kw kws) (cons ctc kw-ctcs))
                    (values (cons ctc pos-ctcs) kws kw-ctcs))))]
      #:attr {pos-ctc 1} (reverse pos-ctcs)
      #:do [(define kws+ctcs (sort (map cons kws kw-ctcs) keyword<? #:key (λ~> car syntax-e)))]
      #:attr {kw 1} (map car kws+ctcs)
      #:attr {kw-ctc 1} (map cdr kws+ctcs)))

  (define-syntax-class range-ctc
    #:description "a range contract"
    #:attributes [{ctc 1}]
    #:commit
    #:literals [any values]
    (pattern any #:attr {ctc 1} #f)
    (pattern (values ~! {~var || (ctc #:positive? #t)} ...))
    (pattern {~var e (ctc #:positive? #t)} #:attr {ctc 1} (list #'e.ctc)))

  (define-syntax-class arrow-ctc
    #:description "an arrow contract"
    #:attributes [struct-e arity-mask req-kws opt-kws]
    #:commit
    #:literals [-> ->*]

    (pattern (-> ~! doms:arg-ctcs
                 {~optional {~seq rest:ctc {~literal ...}}}
                 range:range-ctc)
      #:fail-when (check-duplicates (attribute doms.kw) eq? #:key syntax-e) "duplicate keyword"

      #:attr struct-e
      #'(case-kw-arrow-contract
         (list doms.pos-ctc ...)
         (list 'doms.kw ...)
         (list doms.kw-ctc ...)
         {~? rest.ctc #f}
         {~? (list range.ctc ...) #f})

      #:attr arity-mask (make-arity-mask #:req (length (attribute doms.pos-ctc))
                                         #:opt 0
                                         #:rest? (attribute rest))
      #:attr req-kws (list->seteq (map syntax-e (attribute doms.kw)))
      #:attr opt-kws (seteq))

    (pattern (->* ~! [req-doms:arg-ctcs]
                  {~optional [opt-doms:arg-ctcs]}
                  {~optional {~seq #:rest ~! rest:ctc}}
                  range:range-ctc)
      #:fail-when (check-duplicates (append (attribute req-doms.kw)
                                            (or (attribute opt-doms.kw) '()))
                                    eq? #:key syntax-e)
      "duplicate keyword"

      #:attr struct-e
      #'(case-kw-arrow*-contract
         (list req-doms.pos-ctc ...)
         (list {~? {~@ opt-doms.pos-ctc ...}})
         (list 'req-doms.kw ...)
         (list req-doms.kw-ctc ...)
         (list {~? {~@ 'opt-doms.kw ...}})
         (list {~? {~@ opt-doms.kw-ctc ...}})
         {~? rest.ctc #f}
         {~? (list range.ctc ...) #f})

      #:attr arity-mask (make-arity-mask #:req (length (attribute req-doms.pos-ctc))
                                         #:opt (length (or (attribute opt-doms.pos-ctc) '()))
                                         #:rest? (attribute rest))
      #:attr req-kws (list->seteq (map syntax-e (attribute req-doms.kw)))
      #:attr opt-kws (list->seteq (map syntax-e (or (attribute opt-doms.kw) '()))))))

;; Like `case->`, but for `case-kw-lambda` procedures. Currently, the
;; implementation is not particularly efficient, but it gets the job done.
(define-syntax (case-kw-> stx)
  (parameterize ([current-contract-obligation-key (gensym)])
    (syntax-parse stx
      #:track-literals
      [(_ case:arrow-ctc ...)

       (define req-kws (apply set-union (seteq) (attribute case.req-kws)))
       (define opt-kws (set-subtract (apply set-union (seteq) (attribute case.opt-kws)) req-kws))

       (define/syntax-parse [req-kw ...] (set->list req-kws))
       (define/syntax-parse [opt-kw ...] (set->list opt-kws))

       (~> #`(make-case-kw-contract
              (list case.struct-e ...)
              #,(apply bitwise-ior (attribute case.arity-mask))
              (seteq 'req-kw ...)
              (seteq 'opt-kw ...))
           (syntax-property 'racket/contract:contract
                            (vector (current-contract-obligation-key) '() '())))])))

(struct case-kw-arrow-contract
  (pos-ctcs
   kws
   kw-ctcs
   rest-ctc
   range-ctcs)
  #:transparent)

(struct case-kw-arrow*-contract
  (req-pos-ctcs
   opt-pos-ctcs
   req-kws
   req-kw-ctcs
   opt-kws
   opt-kw-ctcs
   rest-ctc
   range-ctcs)
  #:transparent)

(struct case-kw-arrow-proj
  (min-pos
   max-pos
   pos-projs
   req-kws
   all-kws
   kw-projs
   rest-proj
   range-proj)
  #:transparent)

;; Given a list of range contracts, builds a projection of the form
;;   f : (-> blame? (-> any/c neg-party? (-> any/c ... any)))
;; such that ((f blame) val neg-party) can be used as a result-wrapper-proc for
;; a chaperoned or impersonated procedure. The result is specialized to use a
;; more efficient strategy for the overwhelmingly common cases that only one or
;; two values are expected to be returned.
(define (build-range-proj ctcs case-idx)
  (define num-expected (length ctcs))
  (define (wrong-number blame neg-party val results)
    (bad-number-of-results blame #:missing-party neg-party val
                           num-expected results case-idx))

  (match (map get/build-late-neg-projection ctcs)
    [(list proj)
     (λ (blame)
       (define val-proj (proj blame))
       (λ (val neg-party)
         (case-lambda
           [(result) (with-contract-continuation-mark (cons blame neg-party)
                       (val-proj result neg-party))]
           [results (wrong-number blame neg-party val results)])))]
    [(list proj1 proj2)
     (λ (blame)
       (define val-proj1 (proj1 blame))
       (define val-proj2 (proj2 blame))
       (λ (val neg-party)
         (case-lambda
           [(result1 result2) (with-contract-continuation-mark (cons blame neg-party)
                                (values (val-proj1 result1 neg-party)
                                        (val-proj2 result2 neg-party)))]
           [results (wrong-number blame neg-party val results)])))]
    [projs
     (λ (blame)
       (define val-projs (map (λ (proj) (proj blame)) projs))
       (λ (val neg-party)
         (λ results
           (with-contract-continuation-mark (cons blame neg-party)
             (if (= (length results) num-expected)
                 (apply values (map (λ (proj result) (proj result neg-party)) val-projs results))
                 (wrong-number blame neg-party val results))))))]))

;; Builds a late-neg projection for a single case of a `case-kw->` contract.
;;
;; (-> (or/c case-kw-arrow-contract? case-kw-arrow*-contract?)
;;     case-kw-arrow-proj?)
(define (get/build-case-proj case case-idx)
  (match case
    ;; `->` case
    [(case-kw-arrow-contract pos-ctcs kws kw-ctcs rest-ctc range-ctcs)
     (define num-args (length pos-ctcs))
     (case-kw-arrow-proj
      num-args
      (and (not rest-ctc) num-args)
      (map get/build-late-neg-projection pos-ctcs)
      kws
      kws
      (map get/build-late-neg-projection kw-ctcs)
      (and rest-ctc
           ; For `->` cases, normalize a `...` contract so it accepts a list of
           ; rest arguments, just like a `#:rest` contract in an `->*` case.
           (let ([elem-proj (get/build-late-neg-projection rest-ctc)])
             (λ (blame)
               (define elem-val-proj (elem-proj blame))
               (λ (vals neg-party)
                 (for/list ([val (in-list vals)])
                   (elem-val-proj val neg-party))))))
      (and~> range-ctcs (build-range-proj case-idx)))]

    ;; `->*` case
    [(case-kw-arrow*-contract req-pos-ctcs opt-pos-ctcs
                              req-kws req-kw-ctcs opt-kws opt-kw-ctcs
                              rest-ctc range-ctcs)
     (define min-args (length req-pos-ctcs))
     (case-kw-arrow-proj
      min-args
      (and (not rest-ctc) (+ min-args (length opt-pos-ctcs)))
      (map get/build-late-neg-projection (append req-pos-ctcs opt-pos-ctcs))
      req-kws
      (append req-kws opt-kws)
      (map get/build-late-neg-projection (append req-kw-ctcs opt-kw-ctcs))
      (and~> rest-ctc get/build-late-neg-projection)
      (and~> range-ctcs (build-range-proj case-idx)))]))

;; Applies a projection for a single case of a `case-kw->` contract to
;; a blame object.
(define (apply-case-proj case blame)
  (define (app-rest proj)
    (proj (blame-add-context blame "the rest argument of" #:swap? #t)))
  (define (app-range proj)
    (proj (blame-add-context blame "the range of")))

  (match case
    [(case-kw-arrow-proj min-pos max-pos pos-projs
                         req-kws all-kws kw-projs
                         rest-proj range-proj)
     (case-kw-arrow-proj
      min-pos
      max-pos
      (for/list ([proj (in-list pos-projs)]
                 [n (in-naturals 1)])
        (proj (blame-add-context blame (nth-argument-of n) #:swap? #t)))
      req-kws
      (sort all-kws keyword<?)
      (for/hasheq ([kw (in-list all-kws)]
                   [proj (in-list kw-projs)])
        (values kw (proj (blame-add-context blame (format "the ~a argument of" kw)))))
      (and~> rest-proj app-rest)
      (and~> range-proj app-range))]))

(define (build-contract-name . args)
  (apply apply build-compound-type-name args))

(define (english-list vs)
  (match vs
    [(list a)        (~a a)]
    [(list a b)      (~a a " and " b)]
    [(list as ... b) (~a (string-join (map ~a as) ", ") ", and " b)]))

;; Checks whether a sorted list of unique keywords `as` is a subset
;; of a sorted list of keywords `bs` in linear time.
(define (keywords<? as bs)
  (match* {as bs}
    [{'() _} #t]
    [{_ '()} #f]
    [{(cons a as) (cons a bs)}
     (keywords<? as bs)]
    [{_ (cons _ bs)}
     (keywords<? as bs)]))

(define (build-case-kw-contract-property build-contract-property
                                         wrap-procedure)
  (define (arity-mask-includes? sup sub)
    (eqv? sub (bitwise-and sub sup)))

  (define (build-first-order self)
    (define arity-mask (case-kw-contract-arity-mask self))
    (define req-kws (case-kw-contract-req-kws self))
    (define opt-kws (case-kw-contract-opt-kws self))
    (λ (val)
      (and (procedure? val)
           (arity-mask-includes? (procedure-arity-mask val) arity-mask)
           (let ()
             (define-values {v-req-kws-lst v-all-kws-lst} (procedure-keywords val))
             (define v-all-kws (and v-all-kws-lst (list->seteq v-all-kws-lst)))
             (and (for/and ([kw (in-list v-req-kws-lst)])
                    (set-member? req-kws kw))
                  (or (not v-all-kws)
                      (and (subset? req-kws v-all-kws)
                           (subset? opt-kws v-all-kws))))))))

  (build-contract-property
   #:name
   (λ (self)
     (define (range-ctcs->name ranges)
       (match ranges
         [#f 'any]
         [(list range) range]
         [_ (build-contract-name 'values ranges)]))

     (build-contract-name
      'case-kw->
      (for/list ([case (in-list (case-kw-contract-cases self))])
        (match case
          [(case-kw-arrow-contract pos-ctcs kws kw-ctcs rest-ctc range-ctcs)
           (build-contract-name
            '->
            (append pos-ctcs
                    (append-map list kws kw-ctcs)
                    (if rest-ctc (list rest-ctc '...) '())
                    (list (range-ctcs->name range-ctcs))))]

          [(case-kw-arrow*-contract req-pos-ctcs opt-pos-ctcs
                                    req-kws req-kw-ctcs opt-kws opt-kw-ctcs
                                    rest-ctc range-ctcs)
           (build-contract-name
            '->*
            (build-contract-name (append req-pos-ctcs (append-map list req-kws req-kw-ctcs)))
            (append (if (and (empty? opt-pos-ctcs) (hash-empty? opt-kws))
                        '()
                        (list (build-contract-name
                               (append opt-pos-ctcs (append-map list opt-kws opt-kw-ctcs)))))
                    (if rest-ctc (list '#:rest rest-ctc) '())
                    (list (range-ctcs->name range-ctcs))))]))))
   #:first-order build-first-order
   #:late-neg-projection
   (λ (self)
     (define first-order-passes? (build-first-order self))
     (define arity-mask (case-kw-contract-arity-mask self))
     (define req-kws (case-kw-contract-req-kws self))
     (define opt-kws (case-kw-contract-opt-kws self))

     (define case-ctcs (case-kw-contract-cases self))
     (define case-projs (for/list ([ctc (in-list case-ctcs)]
                                   [idx (in-naturals)])
                          (get/build-case-proj ctc idx)))

     (λ (blame)
       (define case-val-projs
         (for/list ([proj (in-list case-projs)]
                    [n (in-naturals 1)])
           (apply-case-proj proj (blame-add-context blame (nth-case-of n)))))

       (λ (val neg-party)
         (define (bail msg . args)
           (apply raise-blame-error blame val #:missing-party neg-party
                  (list 'expected msg 'given: "~e")
                  (append args (list val))))

         (unless (procedure? val)
           (bail "a procedure"))

         (unless (first-order-passes? val)
           (define val-arity-mask (procedure-arity-mask val))
           (define-values {val-req-kws-lst val-all-kws-lst} (procedure-keywords val))

           (for ([i (in-range (integer-length arity-mask))]
                 #:when (bitwise-bit-set? arity-mask i)
                 #:unless (bitwise-bit-set? val-arity-mask i))
             (bail "a procedure that accepts ~a argument~a" i (if (= i 1) "" "s")))

           (when (and (negative? arity-mask) (not (negative? val-arity-mask)))
             (bail "a procedure that accepts arbitrarily many arguments"))

           (for ([req-kw (in-list val-req-kws-lst)]
                 #:unless (set-member? req-kws req-kw))
             (bail "a procedure that does not require a ~a argument" req-kw))

           (when val-all-kws-lst
             (define v-all-kws (list->seteq val-all-kws-lst))
             (for ([kw (in-immutable-set req-kws)]
                   #:unless (set-member? v-all-kws kw))
               (bail "a procedure that accepts a ~a argument" kw))
             (for ([kw (in-immutable-set opt-kws)]
                   #:unless (set-member? v-all-kws kw))
               (bail "a procedure that accepts an optional ~a argument" kw))))

         (define (app-proj proj arg)
           (proj arg neg-party))

         (wrap-procedure
          val
          (make-keyword-procedure
           (λ (kws kw-args . pos-args)
             (with-contract-continuation-mark (cons blame neg-party)
               (define num-pos-args (length pos-args))
               (let loop ([cases case-val-projs])
                 (match cases
                   [(cons (case-kw-arrow-proj min-pos max-pos pos-projs
                                              req-kws all-kws kw-projs
                                              rest-proj range-proj)
                          cases)
                    (cond
                      [(and (>= num-pos-args min-pos)
                            (or rest-proj
                                (<= num-pos-args max-pos))
                            (keywords<? req-kws kws)
                            (keywords<? kws all-kws))

                       (define pos-args* (let loop ([projs pos-projs]
                                                    [args pos-args])
                                           (match* {projs args}
                                             [{(cons proj projs) (cons arg args)}
                                              (cons (proj arg neg-party) (loop projs args))]
                                             [{_ '()} '()]
                                             [{_ _} (rest-proj args)])))

                       (define args* (if (empty? kws)
                                         pos-args*
                                         (cons (for/list ([kw (in-list kws)]
                                                          [arg (in-list kw-args)])
                                                 ((hash-ref kw-projs kw) arg neg-party))
                                               pos-args*)))

                       (if range-proj
                           (apply values (range-proj val neg-party) args*)
                           (apply values args*))]
                      [else
                       (loop cases)])]
                   ['()
                    (raise-blame-error
                     (blame-swap blame) #:missing-party neg-party
                     (~a "no case matching " num-pos-args " by-position "
                         "argument" (if (= num-pos-args 1) "" "s")
                         (if (empty? kws) ""
                             (~a " and keyword" (if (= (length kws) 1) "" "s")
                                 " " (english-list kws)))))])))))
          impersonator-prop:contracted self
          impersonator-prop:blame (cons blame neg-party)))))))

(struct case-kw-contract (cases arity-mask req-kws opt-kws)
  #:property prop:custom-write contract-custom-write-property-proc)
(struct chaperone-case-kw-contract case-kw-contract ()
  #:property prop:chaperone-contract
  (build-case-kw-contract-property build-chaperone-contract-property
                                   chaperone-procedure))
(struct impersonator-case-kw-contract case-kw-contract ()
  #:property prop:contract
  (build-case-kw-contract-property build-contract-property
                                   impersonate-procedure))

(define (make-case-kw-contract cases arity-mask req-kws opt-kws)
  ((if (for/and ([case (in-list cases)])
         (match case
           [(case-kw-arrow-contract pos-ctcs _ kw-ctcs rest-ctc range-ctcs)
            (and (andmap chaperone-contract? pos-ctcs)
                 (andmap chaperone-contract? kw-ctcs)
                 (or (not rest-ctc) (chaperone-contract? rest-ctc))
                 (or (not range-ctcs) (andmap chaperone-contract? range-ctcs)))]
           [(case-kw-arrow*-contract req-pos-ctcs opt-pos-ctcs
                                     _ req-kw-ctcs _ opt-kw-ctcs
                                     rest-ctc range-ctcs)
            (and (andmap chaperone-contract? req-pos-ctcs)
                 (andmap chaperone-contract? opt-pos-ctcs)
                 (andmap chaperone-contract? req-kw-ctcs)
                 (andmap chaperone-contract? opt-kw-ctcs)
                 (or (not rest-ctc) (chaperone-contract? rest-ctc))
                 (or (not range-ctcs) (andmap chaperone-contract? range-ctcs)))]))
       chaperone-case-kw-contract
       impersonator-case-kw-contract)
   cases arity-mask req-kws opt-kws))
