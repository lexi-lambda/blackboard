#lang racket/base

(require racket/contract
         "draw/font.rkt"
         "style.rkt")

(provide (contract-out
          [font (style-property/c font-description?)]
          [math-font (style-property/c font-description?)]
          [math-depth (style-property/c math-script-depth?)]
          [math-shift (style-property/c (or/c 'normal 'compact))]
          [math-style (style-property/c (or/c 'normal 'compact))]
          [visibility (style-property/c (or/c 'visible 'hidden))]))

;; -----------------------------------------------------------------------------

(define font (make-inherited-style-property 'font
                                            #:default (make-font-description #:size 16)
                                            #:combine combine-font-descriptions))
(define math-font (make-inherited-style-property 'math-font
                                                 #:default (make-font-description #:family "math")))

(define math-depth (make-inherited-style-property 'math-depth #:default 0))
(define math-shift (make-inherited-style-property 'math-shift #:default 'normal))
(define math-style (make-inherited-style-property 'math-style #:default 'normal))
(define visibility (make-inherited-style-property 'visibility #:default 'visible))
