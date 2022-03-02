#lang racket/base

(require racket/contract
         racket/match)

(provide (contract-out
          [char:minus char?]

          [math-char-style/c flat-contract?]
          [math-char (->* [char?]
                          [#:bold? any/c
                           #:italic? any/c
                           #:style math-char-style/c]
                          char?)]

          [superscript-char (-> char? char?)]
          [subscript-char (-> char? char?)]
          [integer->superscript-string (-> exact-integer? string?)]
          [integer->subscript-string (-> exact-integer? string?)]))

;; -----------------------------------------------------------------------------

(define char:minus #\u2212)

(define (char+ c . ns)
  (integer->char (apply + (char->integer c) ns)))
(define (char-diff c1 c2)
  (- (char->integer c1) (char->integer c2)))

(define math-char-style/c
  (or/c 'serif 'sans-serif 'script 'fraktur 'monospace 'double-struck))

(define (math-char c
                   #:bold? [bold? #f]
                   #:italic? [italic? #f]
                   #:style [style 'serif])
  (define (bad-combo)
    (raise-arguments-error 'math-char "no character for style"
                           "character" c
                           "bold?" bold?
                           "italic?" italic?
                           "style" style))

  (define (regular stride c)
    (match* {bold? italic?}
      [{#f #f} c]
      [{#t #f} (char+ c stride)]
      [{#f #t} (char+ c (* stride 2))]
      [{#t #t} (char+ c (* stride 3))]))

  (define (latin c c-offset)
    (match* {style bold? italic? c}
      [{'serif         #f #t #\h} #\U210E]
      [{'serif         _  _  _  } (regular 52 (char+ #\U1D3CC c-offset))]
      [{'sans-serif    _  _  _  } (regular 52 (char+ #\U1D5A0 c-offset))]
      [{'script        #f #f #\B} #\U212C]
      [{'script        #f #f #\E} #\U2130]
      [{'script        #f #f #\F} #\U2131]
      [{'script        #f #f #\H} #\U210B]
      [{'script        #f #f #\I} #\U2110]
      [{'script        #f #f #\L} #\U2112]
      [{'script        #f #f #\M} #\U2133]
      [{'script        #f #f #\R} #\U211B]
      [{'script        #f #f #\e} #\U212F]
      [{'script        #f #f #\g} #\U210A]
      [{'script        #f #f #\o} #\U2134]
      [{'script        _  #f _  } (regular 52 (char+ #\U1D49C c-offset))]
      [{'fraktur       #f #f #\C} #\U212D]
      [{'fraktur       #f #f #\H} #\U210C]
      [{'fraktur       #f #f #\I} #\U2111]
      [{'fraktur       #f #f #\R} #\U211C]
      [{'fraktur       #f #f #\Z} #\U2128]
      [{'fraktur       _  #f _  } (regular 104 (char+ #\U1D504 c-offset))]
      [{'monospace     #f #f _  } (char+ #\U1D670 c-offset)]
      [{'double-struck #f #f #\C} #\U2102]
      [{'double-struck #f #f #\H} #\U210D]
      [{'double-struck #f #f #\N} #\U2115]
      [{'double-struck #f #f #\P} #\U2119]
      [{'double-struck #f #f #\Q} #\U211A]
      [{'double-struck #f #f #\R} #\U211D]
      [{'double-struck #f #f #\Z} #\U2124]
      [{'double-struck #f #f _  } (char+ #\U1D538 c-offset)]
      [{_              _  _  _  } (bad-combo)]))

  (define (greek c-offset)
    (match* {style bold? italic?}
      [{'serif      _  _ } (regular 58 (char+ #\U1D66E c-offset))]
      [{'sans-serif #t #f} (char+ #\U1D756 c-offset)]
      [{'sans-serif #t #t} (char+ #\U1D790 c-offset)]
      [{_           _  _ } (bad-combo)]))

  (define (digit c-offset)
    (match* {style bold? italic?}
      [{'serif         #t #f} (char+ #\U1D7CE c-offset)]
      [{'sans-serif    #f #f} (char+ #\U1D7E2 c-offset)]
      [{'sans-serif    #t #f} (char+ #\U1D7EC c-offset)]
      [{'monospace     #f #f} (char+ #\U1D7F6 c-offset)]
      [{'double-struck #f #f} (char+ #\U1D7D8 c-offset)]
      [{_              _  _ } (bad-combo)]))

  (cond
    [(and (not bold?) (not italic?) (eq? style 'serif)) c]
    [(char<=? #\A c #\Z) (latin c (char-diff c #\A))]
    [(char<=? #\a c #\z) (latin c (+ (char-diff c #\a) 26))]
    [(or (char<=? #\U391 c #\U3A1)
         (char<=? #\Σ c #\Ω)
         (char<=? #\α c #\ω))
     (greek (char-diff c #\U391))]
    [(char=? c #\Θ) (greek 11)]
    [(char=? c #\∇) (greek 19)]
    [(char=? c #\U2202) (greek 39)]
    [(char=? c #\U03F5) (greek 40)]
    [(char=? c #\U03D1) (greek 41)]
    [(char=? c #\U03F0) (greek 42)]
    [(char=? c #\U03D5) (greek 43)]
    [(char=? c #\U03F1) (greek 44)]
    [(char=? c #\U03D6) (greek 45)]
    [(char<=? #\0 c #\9) (digit (char-diff c #\0))]
    [else (bad-combo)]))

(define (superscript-char c)
  (match c
    [#\1             #\u00B9]
    [#\2             #\u00B2]
    [#\3             #\u00B3]
    [#\+             #\u207A]
    [(== char:minus) #\u207B]
    [#\=             #\u207C]
    [#\(             #\u207D]
    [#\)             #\u207E]
    [#\n             #\u207F]
    [_
     (if (char<=? #\0 c #\9)
         (char+ #\u2070 (char-diff c #\0))
         (raise-arguments-error 'superscript-char "no superscript variant for character"
                                "character" c))]))

(define (subscript-char c)
  (cond
    [(char<=? #\0 c #\9)
     (char+ #\u2080 (char-diff c #\0))]
    [else
     (match c
       [#\+             #\u208A]
       [(== char:minus) #\u208B]
       [#\=             #\u208C]
       [#\(             #\u208D]
       [#\)             #\u208E]
       [#\a             #\u2090]
       [#\e             #\u2091]
       [#\o             #\u2092]
       [#\x             #\u2093]
       [#\ə             #\u2094]
       [#\h             #\u2095]
       [#\k             #\u2096]
       [#\l             #\u2097]
       [#\m             #\u2098]
       [#\n             #\u2099]
       [#\p             #\u209A]
       [#\s             #\u209B]
       [#\t             #\u209C]
       [_
        (raise-arguments-error 'subscript-char "no subscript variant for character"
                               "character" c)])]))

(define (integer->script-string n convert-char)
  (define num-str (number->string n))
  (define len (string-length num-str))
  (define str (if (immutable? num-str) (make-string len) num-str))
  (for ([i (in-range len)])
    (define c (string-ref num-str i))
    (string-set! str i (convert-char (if (char=? c #\-) char:minus c))))
  str)

(define (integer->superscript-string n)
  (integer->script-string n superscript-char))

(define (integer->subscript-string n)
  (integer->script-string n subscript-char))
