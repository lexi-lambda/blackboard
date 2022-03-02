#lang racket/base

(require racket/list
         racket/match
         threading

         "content.rkt"
         "open-type.rkt"
         "size.rkt"
         "util/print.rkt"
         "util/struct.rkt")

(define (sub a b) (error 'sub "fixme"))

(define rdr:thin-space (rdr:spring (ems 3/18)))
(define rdr:medium-space (rdr:spring (ems 4/18) #:stretch (ems 2/18) #:shrink (ems 4/18)))
(define rdr:thick-space (rdr:spring (ems 5/18) #:stretch (ems 5/18)))

(define absent-v (gensym 'absent))

;; -----------------------------------------------------------------------------
;; math fragments

;; A /math fragment/ is a fragment of renderable mathematics. The `content`
;; field contains the actual renderable structure. Each fragment also includes a
;; `name` field, which is used to `print` the fragment.
;;
;; Some math fragments also have additional information used by `term` to parse
;; mathematical expressions containing infix operators:
;;
;;   * The `binop` function constructs an /infix fragment/, which has a
;;     precedence and associativity stored in `op-info`.
;;
;;   * When `term` parses an expression, it records information about the
;;     operator used in the expression’s outermost spine in the `spine-op`
;;     field. For example, the term
;;
;;         (let ([<$> (binop 4 'left ....)])
;;           (term "a" <$> "b"))
;;
;;     records that its spine was formed from an `infixl 4` binary operator.
;;     This allows parentheses to be automatically inserted around the fragment
;;     if it is later inserted into a larger expression immediately under an
;;     operator with a tighter precedence.
(struct math
  (name     ; any/c
   props    ; hash-equal?
   renderer ; renderable-content?
   #;op-fixity
   #;spine-op)
  #:name math-struct
  #:constructor-name make-math-struct
  #:property prop:object-name (struct-field-index name)
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (self out mode)
    (define (write-prefix) (write-string "#<math: " out))
    (define (write-suffix) (write-string ">" out))
    (define name (math-name self))
    (match mode
      [(or #f #t)
       (write-prefix)
       (fprintf out "~.v" name)
       (write-suffix)]
      [0
       (print name out)]
      [1
       (write-prefix)
       (print name out)
       (write-suffix)])))

;; A math fragment that is also a procedure. This is useful for defining math
;; fragments that behave as constructors when applied to arguments.
(struct math:procedure math-struct (proc)
  ; We never construct instances of `math:procedure` directly, only subtypes of
  ; it; see the comment in `make-math`.
  #:constructor-name unused-make-math:procedure)

(define (make-math renderer
                   #:name name
                   #:props [props (hash)]
                   #:proc [proc #f])
  (cond
    [proc
     ; Rather than attach a `prop:procedure` property to the `math:procedure` type
     ; directly, create a subtype with `proc` as its value. This allows the result
     ; to respond properly to `procedure-arity` while still receiving a `self` argument.
     (struct math:procedure* math:procedure ()
       #:property prop:procedure proc)
     (math:procedure* name props renderer proc)]
    [else
     (make-math-struct name props renderer)]))

;; Like `struct-copy`, but uses `make-math` to preserve an attached procedure.
(define (math-copy m
                   #:name [name (math-name m)]
                   #:props [props (math-props m)]
                   #:renderer [renderer (math-renderer m)]
                   #:proc [proc (and (math:procedure? m) (math:procedure-proc m))])
  (make-math renderer #:name name #:props props #:proc proc))

(define (math-rename m name)
  (math-copy m #:name name))

(define (set-math-procedure m proc)
  (math-copy m #:proc proc))

(define math-property
  (case-lambda
    [(m k)
     (hash-ref (math-props m) k #f)]
    [(m k v)
     (math-copy m #:props (if v
                              (hash-set (math-props m) k v)
                              (hash-remove (math-props m) k)))]))

(define empty-math (make-math (rdr:blank) #:name ""))

(define (math->pict m)
  (render-pict (math-renderer m)))

;; -----------------------------------------------------------------------------
;; decoding math

(define (pre-math? v)
  (or (math? v)
      (string? v)
      (and (list? v) (andmap pre-math? v))))

(define (text str #:name [name absent-v])
  (if (and (string=? str "")
           (eq? name absent-v))
      empty-math
      (make-math (rdr:text str)
                 #:name (if (eq? name absent-v)
                            (quasiexpr (text ,str))
                            name))))

(require pict racket/function)
(define (mathrm str)
  (make-math (rdr:math str)
             #:name (quasiexpr (mathrm ,str))))
(define (textsf str)
  (make-math (rdr:wrap (rdr:text str)
                       (λ (go)
                         (parameterize ([current-text-font
                                         (curry modern-text #:style 'sans-serif)])
                           (go))))
             #:name (quasiexpr (textsf ,str))))
(define (textit str)
  (make-math (rdr:wrap (rdr:text str)
                       (λ (go)
                         (parameterize ([current-text-italic? #t])
                           (go))))
             #:name (quasiexpr (textsf ,str))))

(define (var str)
  (cond
    [(string=? str "")
     empty-math]
    [(and (= (string-length str) 1)
          (let ([c (string-ref str 0)])
            (and (or (char<=? #\A c #\Z)
                     (char<=? #\a c #\z))
                 c)))
     => (λ (c)
          (make-math (rdr:math (string (math-char c #:italic? #t)))
                     #:name (quasiexpr (var ,str))))]
    [else
     (make-math (rdr:wrap (rdr:text str)
                          (λ (go)
                            (parameterize ([current-text-italic? #t]
                                           [current-text-bold? #f])
                              (go))))
                #:name (quasiexpr (var ,str)))]))

(define (default-decode-math-string str)
  (match str
    ["" empty-math]
    [(regexp #px"^([^_]+)_([^_]+)$" (list _ a b))
     (sub a b)]
    [_ (var str)]))

(define current-decode-math-string (make-parameter default-decode-math-string))
(define (decode-math-string str)
  ((current-decode-math-string) str))

(struct math-app (names)
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (self out mode) (print (quasiexpr (math ,@(math-app-names self))) out)))

;; Like (map math-name ms), but removes empty names and splices uses of `math`.
(define (math-sequence-names ms)
  (for/foldr ([names '()])
             ([m (in-list ms)])
    (match (math-name m)
      ["" names]
      [(math-app seq-names) (append seq-names names)]
      [name (cons name names)])))

(define (math #:name [name absent-v] . ms)
  (define outer-m
    (let loop ([m ms])
      (match m
        [(? math?) m]
        [(? string?) (decode-math-string m)]
        ['() empty-math]
        [(list m) (loop m)]
        [(? list?)
         (define ms (map loop m))
         (make-math (math-renderer ms)
                    #:name (if (eq? name absent-v)
                               (math-app (math-sequence-names ms))
                               #f))])))
  (if (eq? name absent-v)
      outer-m
      (math-rename outer-m name)))

;; -----------------------------------------------------------------------------

(define (math-content? v)
  (or (string? v)
      (and (list? v) (andmap math-content? v))))

;; -----------------------------------------------------------------------------
;; binary operators

(define (precedence? v)
  (and (rational? v)
       (exact? v)))

(define (associativity? v)
  (and (memq v '(none left right)) #t))

(struct binop-fixity (prec assoc) #:transparent)
(define-uniques math-prop:operator-fixity math-prop:spine-fixity)

(define (math-binop? v)
  (and (math? v)
       (binop-fixity? (math-property v math-prop:operator-fixity))))

;; Constructs a math fragment and assigns it the given fixity. `binop` supports
;; two different call patterns:
;;   1. (binop fixity m ...)
;;   2. (binop prec assoc m ...)
;; The second pattern is an abbreviation for (binop (binop-fixity prec assoc) m ...).
(define (binop . args)
  (define-values [fixity ms]
    (match args
      [(cons (? binop-fixity? fixity) ms) (values fixity ms)]
      [(list* prec assoc ms) (values (binop-fixity prec assoc) ms)]))
  ; Note: we intentionally unset a math procedure here, as it doesn’t make sense
  ; to keep the procedure used by a prefix application if the operator is
  ; intended to be used infix.
  (set-math-procedure (math-property (math ms) math-prop:operator-fixity fixity) #f))

;; -----------------------------------------------------------------------------
;; math expressions

(define current-application-fixity (make-parameter (binop-fixity 20 'left)))

(define (pre-expr? v)
  (or (math? v)
      (string? v)))

;; Like `seq`, `term` constructs a math fragment that combines a sequence of
;; smaller math fragments. Unlike `seq`, `term` consults the fixity annotations
;; on its arguments to automatically insert parentheses around arguments that
;; need them to preserve the appropriate grouping structure.
;;
;; Note that this means that `term` is more restrictive than `seq`: not all
;; sequences of math fragments form valid expressions. In particular, `term`
;; requires that infix operators appear between two sequences that themselves
;; form valid expressions, and that if two operators appear in the sequence with
;; the same precedence, they both share the same non-`'none` associativity.
;; Additionally, to avoid confusion about whether lists should provide grouping
;; or if they should simply splice into the enclosing sequence, `term` does not
;; accept lists.
(define (expr . pre-ms)
  (define fixity:app (current-application-fixity))

  (match pre-ms
    ['() empty-math]
    [(list m) (math m)]
    [_
     ;; Adds parentheses to an argument to an infix operator if necessary to
     ;; preserve grouping. The `left-or-right` argument specifies whether `m`
     ;; appears to the left or to the right of the operator.
     (define (maybe-parens m left-or-right op-info)
       (define rdr (math-renderer m))
       (match (math-property m math-prop:spine-fixity)
         [#f rdr]
         [(binop-fixity spine-prec spine-assoc)
          (match-define (binop-fixity op-prec op-assoc) op-info)
          (if (or
               ; If the inner operator is looser, we definitely need parens.
               (< spine-prec op-prec)
               ; Otherwise, we might need parens if the precedence is the same.
               (and (= spine-prec op-prec)
                    (or
                     ; If either operator is nonfix, we need parens.
                     (eq? 'none spine-assoc)
                     (eq? 'none op-assoc)
                     ; If they have different associativities, we need parens.
                     (not (eq? spine-assoc op-assoc))
                     ; If they associate in the wrong direction, we need parens
                     ; to maintain the right grouping.
                     (not (eq? left-or-right spine-assoc)))))
              (rdr:list (rdr:math "(") rdr (rdr:math ")"))
              rdr)]))

     ;; Consumes one or more non-infix terms in `ms`. If multiple non-infix
     ;; terms appear in sequence, they are parsed as a function application.
     ;; The `op` argument is only used for error reporting.
     (define (consume-prefix-sequence op ms)
       (match ms
         ['()
          (raise-arguments-error 'term "missing second argument to binary operator"
                                 "operator" op)]
         [(cons (? math-binop? new-op) _)
          (raise-arguments-error 'term "unexpected infix operator"
                                 "operator" new-op)]
         [(cons m (and ms (or '() (cons (? math-binop?) _))))
          (values m ms)]
         [(cons m ms)
          (let loop ([rdrs (list (maybe-parens m 'left fixity:app))]
                     [ms ms])
            (match ms
              [(or '() (cons (? math-binop?) _))
               (values (make-math (apply rdr:list (add-between (reverse rdrs) rdr:thick-space))
                                  #:name #f ; will be overridden later, anyway
                                  #:props (hash math-prop:spine-fixity fixity:app))
                       ms)]
              [(cons m ms)
               (loop (cons (maybe-parens m 'right fixity:app) rdrs)
                     ms)]))]))

     ;; Extends the given `lhs` with a (possibly empty) series of infix
     ;; applications, stopping at either the end of the sequence or the first
     ;; operator that binds looser than `min-prec`. This implements the
     ;; “precedence climbing” method to operator precedence parsing.
     (define (consume-infix-sequence min-prec lhs ms)
       (match ms
         [(cons (? math? op1 (app (λ~> (math-property math-prop:operator-fixity))
                                  (and op1-info (binop-fixity op1-prec op1-assoc))))
                ms)
          #:when (>= op1-prec min-prec)
          (define-values [rhs ms*] (consume-prefix-sequence op1 ms))
          (let loop ([rhs rhs]
                     [ms ms*])
            (match ms
              [(cons (? math? op2 (app (λ~> (math-property math-prop:operator-fixity))
                                       (binop-fixity op2-prec op2-assoc)))
                     _)
               (cond
                 [(and (= op1-prec op2-prec)
                       (or (eq? 'none op1-assoc)
                           (eq? 'none op2-assoc)
                           (not (eq? op1-assoc op2-assoc))))
                  (raise-arguments-error 'decode-term "associativity conflict between binary operators of same precedence"
                                         "first operator" op1
                                         "second operator" op2
                                         "first associativity" op1-assoc
                                         "second associativity" op2-assoc
                                         "precedence" op1-prec)]
                 [(or (< op1-prec op2-prec)
                      (and (= op1-prec op2-prec)
                           (eq? 'right op1-assoc)))
                  (define-values [rhs* ms*] (consume-infix-sequence op2-prec rhs ms))
                  (loop rhs* ms*)]
                 [else
                  (failure-cont)])]
              [_
               (values (make-math (rdr:list (maybe-parens lhs 'left op1-info)
                                            rdr:thick-space
                                            (math-renderer op1)
                                            rdr:thick-space
                                            (maybe-parens rhs 'right op1-info))
                                  #:name #f ; will be overridden later, anyway
                                  #:props (hash math-prop:spine-fixity op1-info))
                       ms)]))]
         [_ (values lhs ms)]))

     (define ms (for/list ([m (in-list pre-ms)])
                  (match m
                    [(? string?) (decode-math-string m)]
                    [(? math?) m])))
     (define-values [lhs ms*] (consume-prefix-sequence #f ms))
     (match-define-values [m '()] (consume-infix-sequence -inf.0 lhs ms*))
     (math-rename m (quasiexpr (term ,@(map math-name ms))))]))
