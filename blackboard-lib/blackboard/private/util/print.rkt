#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/match
         racket/pretty
         (only-in syntax/parse ~seq)
         syntax/parse/define
         threading)

(provide (contract-out
          [custom-write/recur (-> any/c output-port? custom-write-mode/c void?)]
          [make-custom-write/recur (-> output-port? custom-write-mode/c (->* [any/c] [output-port?] void?))]

          [empty-printing-value? predicate/c]
          [empty-printing-value empty-printing-value?]
          [custom-printing-value (->* [(-> output-port? custom-write-mode/c any)]
                                      [#:quotable (or/c 'self 'never 'always 'maybe)]
                                      any/c)]

          [with-printing-overflow-handler (->* [output-port?
                                                (-> output-port? any)
                                                (-> (->* [output-port?] void?) any)]
                                               [#:width exact-positive-integer?
                                                #:space-after exact-nonnegative-integer?]
                                               void?)]
          [printing-sequence (->* [list?]
                                  [#:space-after exact-nonnegative-integer?
                                   #:hang exact-nonnegative-integer?]
                                  any/c)]
          [delimited-printing-sequence (->* [list?]
                                            [#:before string?
                                             #:after string?
                                             #:hang exact-nonnegative-integer?]
                                            any/c)]
          [printing-append (-> any/c ... any/c)]
          [printing-add-separators (->* [list?] [#:trailing any/c #:leading any/c] list?)]
          [printing-hang (->* [any/c
                               any/c]
                              [#:indent exact-nonnegative-integer?
                               #:space-after exact-nonnegative-integer?]
                              any/c)]

          [constructor-style-printing-value (->* [(or/c symbol? string?) list?] [#:expression? any/c] any/c)]
          [make-constructor-style-printer (->* [(-> any/c (or/c symbol? string?))
                                                (-> any/c list?)]
                                               [#:expression? any/c]
                                               (-> any/c output-port? custom-write-mode/c void?))])

         quasiexpr ~@ ~seq ~if)

;; -----------------------------------------------------------------------------

(define custom-write-mode/c (or/c #f #t 0 1))

(define (custom-write/recur v out mode)
  ((make-custom-write/recur out mode) v))

(define (make-custom-write/recur out mode)
  (match mode
    [#f       (λ (v [out out]) (display v out))]
    [#t       (λ (v [out out]) (write   v out))]
    [(or 0 1) (λ (v [out out]) (print   v out mode))]))

;; A value that always prints nothing.
(define-values [empty-printing-value empty-printing-value?]
  (let ()
    (struct empty-printing-value ()
      #:authentic
      #:property prop:custom-write (λ (self out mode) (void)))
    (values (empty-printing-value) empty-printing-value?)))

;; A helper to allow constructing “anonymous” custom printing values
;; of arbitrary quotability.
(define (custom-printing-value proc #:quotable [quotable 'self])
  (match quotable
    ['self   (custom-printing-value:self-quotable   proc)]
    ['never  (custom-printing-value:never-quotable  proc)]
    ['always (custom-printing-value:always-quotable proc)]
    ['maybe  (custom-printing-value:maybe-quotable  proc)]))

(struct custom-printing-value (procedure)
  #:name info:custom-printing-value
  #:constructor-name custom-printing-value:self-quotable
  #:transparent
  #:property prop:custom-write
  (λ (self out mode)
    ((custom-printing-value-procedure self) out mode)))

(struct custom-printing-value:never-quotable info:custom-printing-value ()
  #:property prop:custom-print-quotable 'never)
(struct custom-printing-value:always-quotable info:custom-printing-value ()
  #:property prop:custom-print-quotable 'always)
(struct custom-printing-value:maybe-quotable info:custom-printing-value ()
  #:property prop:custom-print-quotable 'maybe)

;; -----------------------------------------------------------------------------

(define (write-spaces n out)
  (cond
    [(<= n 0)
     (void)]
    [else
     (let loop ([n n])
       (cond
         [(> n 8)
          (write-string "        " out)
          (loop (- n 8))]
         [else
          (write-string
           (vector-ref #(" "
                         "  "
                         "   "
                         "    "
                         "     "
                         "      "
                         "       "
                         "        ")
                       (sub1 n))
           out)
          (void)]))]))

(define (with-printing-overflow-handler out
          #:width [width (pretty-print-columns)]
          #:space-after [space-after 0]
          single-line-proc
          multi-line-proc)
  (cond
    [(and (pretty-printing)
          (port-counts-lines? out))
     (define-values [tentative-out overflowed?]
       (let/ec escape
         (define tentative-out
           (make-tentative-pretty-print-output-port
            out
            (max 0 (- width space-after))
            (λ () (escape tentative-out #t))))
         (single-line-proc tentative-out)
         (values tentative-out #f)))

     (cond
       [overflowed?
        (tentative-pretty-print-port-cancel tentative-out)
        (define-values [line col posn] (port-next-location out))
        (multi-line-proc
         (λ ([out out])
           (pretty-print-newline out width)
           (define-values [line* col* posn*] (port-next-location out))
           (write-spaces (- col col*) out)))]
       [else
        (tentative-pretty-print-port-transfer tentative-out out)])]
    [else
     (single-line-proc out)])
  (void))

;; Prints like `vs`, but without any enclosing parentheses. When not pretty-
;; printing, this means each element of `vs` is just printed with one after the
;; other, with a single space between each one. When pretty-printing, however,
;; each element of `vs` will be printed on a separate line (with appropriate
;; indentation) if they do not fit within (- (pretty-print-colums) space-after).
(define (printing-sequence vs
                           #:space-after [space-after 0]
                           #:hang [hang-indent 0])
  (match vs
    ['() empty-printing-value]
    [(cons v vs)
     (custom-printing-value
      (λ (out mode)
        (define recur (make-custom-write/recur out mode))
        (with-printing-overflow-handler out #:space-after space-after
          (λ (out)
            (recur v out)
            (for ([v (in-list vs)])
              (write-char #\space out)
              (recur v out)))
          (λ (newline)
            (recur v)
            (for ([v (in-list vs)])
              (newline)
              (write-spaces hang-indent out)
              (recur v))))))]))

(define (delimited-printing-sequence vs
                                     #:before [before-str ""]
                                     #:after [after-str ""]
                                     #:hang [hang-indent 0])
  (define printing-vs (printing-sequence vs
                                         #:space-after (string-length after-str)
                                         #:hang hang-indent))
  (custom-printing-value
   (λ (out mode)
     (write-string before-str out)
     (custom-write/recur printing-vs out mode)
     (write-string after-str out))))

(define printing-append
  (case-lambda
    [()  empty-printing-value]
    [(v) v]
    [vs (custom-printing-value
         (λ (out mode) (for-each (make-custom-write/recur out mode) vs)))]))

(define (printing-add-separators vs
                                 #:trailing [trailing-v empty-printing-value]
                                 #:leading [leading-v empty-printing-value])
  (match vs
    ['() '()]
    [(list v) (list v)]
    [(cons v vs)
     (cons (printing-append v trailing-v)
           (let loop ([vs vs])
             (match vs
               [(list v)
                (list (printing-append leading-v v))]
               [(cons v vs)
                (cons (printing-append leading-v v trailing-v) (loop vs))])))]))

(define (printing-hang herald body
                       #:indent [indent-amount 1]
                       #:space-after [space-after 0])
  (custom-printing-value
   (λ (out mode)
     (define recur (make-custom-write/recur out mode))
     (with-printing-overflow-handler out
       #:space-after space-after
       (λ (out)
         (recur herald out)
         (write-char #\space out)
         (recur body out))
       (λ (newline)
         (recur herald)
         (newline)
         (write-spaces indent-amount out)
         (recur body))))))

;; -----------------------------------------------------------------------------

(define (do-constructor-style-print out mode name args #:expression? expression?)
  (define as-expression? (and expression? (eq? mode 0)))
  (define name-str (if as-expression?
                       (format "~a" name)
                       (format "~a:" name)))
  (~> (delimited-printing-sequence
       #:before (if as-expression? "(" "#<")
       #:after (if as-expression? ")" ">")
       (cons (unquoted-printing-string name-str) args))
      (custom-write/recur out mode)))

(define (constructor-style-printing-value name args #:expression? [expression? #t])
  (custom-printing-value
   #:quotable (if expression? 'never 'self)
   (λ (out mode)
     (do-constructor-style-print out mode name args #:expression? expression?))))

(define ((make-constructor-style-printer get-name get-args #:expression? [expression? #t])
         self out mode)
  (do-constructor-style-print
   out
   mode
   (get-name self)
   (get-args self)
   #:expression? expression?))

;; -----------------------------------------------------------------------------

(define (always-prints v [depth 0])
  (custom-printing-value #:quotable 'never (λ (out mode) (print v out depth))))

(begin-for-syntax
  (define-syntax-class qe-term
    #:attributes [e]
    #:description #f
    #:commit
    #:literals [unquote unquote-splicing]
    (pattern {~and e {~or* _:boolean _:number _:id _:keyword _:string ()}})
    (pattern (unquote ~! e*:expr)
      #:attr e #',(always-prints e*))
    (pattern ({~literal ~seq} ~! t:qe-head-term ...)
      #:attr e #',(printing-sequence `(t.e ...)))
    (pattern ({~literal ~if} ~! cond:expr then:qe-term else:qe-term)
      #:attr e #',(if cond `then.e `else.e))
    (pattern ({~and head-id {~or* unquote-splicing {~literal ~@}}} ~! . _)
      #:post {~fail #:when #'head-id "only allowed in a head context"}
      #:attr e #f)
    (pattern (t1:qe-head-term ...+ . t2:qe-term)
      #:attr e #'(t1.e ... . t2.e)))

  (define-syntax-class qe-head-term
    #:attributes [e]
    #:description #f
    #:commit
    #:literals [unquote-splicing]
    (pattern ({~literal ~@} ~! t1:qe-head-term ... . t2:qe-term)
      #:attr e #',@`(t1.e ... . t2.e))
    (pattern ({~literal ~if} ~! cond:expr then:qe-head-term {~optional else:qe-head-term})
      #:attr e #',@(if cond `(then.e) {~? `(else.e) '()}))
    (pattern (unquote-splicing ~! e*:expr)
      #:attr e #',@(map always-prints e*))
    (pattern :qe-term)))

;; Constructs a value that prints like an unquoted expression. For example:
;;
;;   > (quasiexpr (list (+ 1 2))
;;   (list (+ 1 2))
;;
;; Uses of `unquote` or `unquote-splicing` escape as in `quasiquote`, and any
;; value inserted via an escape is printed normally:
;;
;;   > (quasiexpr (list a ()))
;;   (list a ())
;;   > (quasiexpr (list ,'a ,'()))
;;   (list 'a '())
;;
;; Additionally, `~seq` can be used to group subsequences, which is mostly
;; useful to suppress line breaks between keyword argument pairs. For example:
;;
;;   > (quasiexpr (foo #:a 1 #:b 2 <really long argument>))
;;   (foo
;;    #:a
;;    1
;;    #:b
;;    2
;;    <really long argument>)
;;
;;   > (quasiexpr (foo {~seq #:a 1}
;;                     {~seq #:b 2}
;;                     <really long argument>))
;;   (foo
;;    #:a 1
;;    #:b 2
;;    <really long argument>)
(define-syntax-parse-rule (quasiexpr t:qe-term)
  (always-prints `t.e 1))

(define-syntax (~if stx)
  (raise-syntax-error #f "not allowed as an expression" stx))
