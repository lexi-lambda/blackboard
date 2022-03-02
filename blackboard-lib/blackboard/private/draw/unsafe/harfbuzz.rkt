#lang racket/base

(require ffi/cvector
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         threading)

(provide _hb_font_t
         (struct-out hb_ot_math_kern_entry_t)

         hb_version_string

         hb_font_destroy
         hb_font_create
         hb_font_reference
         hb_font_make_immutable
         hb_font_get_face

         hb_font_get_nominal_glyph
         hb_font_get_ppem
         hb_font_get_scale

         hb_ot_metrics_get_position

         hb_ot_math_has_data
         hb_ot_math_get_constant
         hb_ot_math_get_glyph_italics_correction
         hb_ot_math_get_glyph_kernings)

;; -----------------------------------------------------------------------------
;; types

(define _hb_codepoint_t _uint32)
(define _hb_ot_name_id_t _uint)
(define _hb_position_t _int32)

(define _hb_codepoint_t/char
  (make-ctype _hb_codepoint_t
              char->integer
              integer->char))

(define _hb_tag_t
  (make-ctype _uint32
              (λ~> (integer-bytes->integer #f #t))
              (λ~> (integer->integer-bytes 4 #f #t))))

(define-cpointer-type _hb_face_t)
(define-cpointer-type _hb_font_t)
(define-cpointer-type _hb_language_t)

(define _hb_direction_t
  (_enum '(HB_DIRECTION_INVALID
           HB_DIRECTION_LTR
           HB_DIRECTION_RTL
           HB_DIRECTION_TTB
           HB_DIRECTION_BTT)))

(define metrics-tag-sym-bytes-mapping
  (hasheq 'horizontal-ascender         #"hasc"
          'horizontal-descender        #"hdsc"
          'horizontal-line-gap         #"hlgp"
          'horizontal-clipping-ascent  #"hcla"
          'horizontal-clipping-descent #"hcld"
          'vertical-ascender           #"vasc"
          'vertical-descender          #"vdsc"
          'vertical-line-gap           #"vlgp"
          'horizontal-caret-rise       #"hcrs"
          'horizontal-caret-run        #"hcrn"
          'horizontal-caret-offset     #"hcof"
          'vertical-caret-rise         #"vcrs"
          'vertical-caret-run          #"vcrn"
          'vertical-caret-offset       #"vcof"
          'x-height                    #"xhgt"
          'cap-height                  #"cpht"
          'subscript-em-x-size         #"sbxs"
          'subscript-em-y-size         #"sbys"
          'subscript-em-x-offset       #"sbxo"
          'subscript-em-y-offset       #"sbyo"
          'superscript-em-x-size       #"spxs"
          'superscript-em-y-size       #"spys"
          'superscript-em-x-offset     #"spxo"
          'superscript-em-y-offset     #"spyo"
          'strikeout-size              #"strs"
          'strikeout-offset            #"stro"
          'underline-size              #"unds"
          'underline-offset            #"undo"))

(define metrics-tag-sym-mapping
  (for/hasheq ([(sym str) (in-immutable-hash metrics-tag-sym-bytes-mapping)])
    (values sym (cast str _hb_tag_t _uint32))))

(define metrics-tag-num-mapping
  (for/hasheqv ([(sym num) (in-immutable-hash metrics-tag-sym-mapping)])
    (values num sym)))

(define _hb_ot_metrics_tag_t
  (make-ctype
   _uint
   (λ (v) (hash-ref metrics-tag-sym-mapping v
                    (λ () (raise-arguments-error '_hb_ot_metrics_tag_t "unknown Racket value" "value" v))))
   (λ (v) (hash-ref metrics-tag-num-mapping v
                    (λ () (raise-arguments-error '_hb_ot_metrics_tag_t "unknown C value" "value" v))))))

(define _hb_ot_math_constant_t
  (_enum '(script-percent-scale-down
           script-script-percent-scale-down
           delimited-sub-formula-min-height
           display-operator-min-height
           math-leading
           axis-height
           accent-base-height
           flattened-accent-base-height
           subscript-shift-down
           subscript-top-max
           subscript-baseline-drop-min
           superscript-shift-up
           superscript-shift-up-cramped
           superscript-bottom-min
           superscript-baseline-drop-max
           sub-superscript-gap-min
           superscript-bottom-max-with-subscript
           space-after-script
           upper-limit-gap-min
           upper-limit-baseline-rise-min
           lower-limit-gap-min
           lower-limit-baseline-drop-min
           stack-top-shift-up
           stack-top-display-style-shift-up
           stack-bottom-shift-down
           stack-bottom-display-style-shift-down
           stack-gap-min
           stack-display-style-gap-min
           stretch-stack-top-shift-up
           stretch-stack-bottom-shift-down
           stretch-stack-gap-above-min
           stretch-stack-gap-below-min
           fraction-numerator-shift-up
           fraction-numerator-display-style-shift-up
           fraction-denominator-shift-down
           fraction-denominator-display-style-shift-down
           fraction-numerator-gap-min
           fraction-num-display-style-gap-min
           fraction-rule-thickness
           fraction-denominator-gap-min
           fraction-denom-display-style-gap-min
           skewed-fraction-horizontal-gap
           skewed-fraction-vertical-gap
           overbar-vertical-gap
           overbar-rule-thickness
           overbar-extra-ascender
           underbar-vertical-gap
           underbar-rule-thickness
           underbar-extra-descender
           radical-vertical-gap
           radical-display-style-vertical-gap
           radical-rule-thickness
           radical-extra-ascender
           radical-kern-before-degree
           radical-kern-after-degree
           radical-degree-bottom-raise-percent)))

(define _hb_ot_math_kern_t (_enum '(top-right
                                    top-left
                                    bottom-right
                                    bottom-left)))

(define-cstruct _hb_feature_t
  ([tag* _hb_tag_t]
   [value _uint32]
   [start _uint]
   [end _uint]))

(define-cstruct _hb_ot_math_kern_entry_t
  ([max-correction-height _hb_position_t]
   [kern-value _hb_position_t]))

;; -----------------------------------------------------------------------------

(define-ffi-definer define-harfbuzz (ffi-lib "libharfbuzz" '("0" #f)))

(define-harfbuzz hb_version_string (_fun -> _string/utf-8))

;; -----------------------------------------------------------------------------
;; hb-font

(define-harfbuzz hb_font_destroy (_fun _hb_font_t -> _void)
  #:wrap (releaser))
(define-harfbuzz hb_font_create (_fun _hb_face_t -> _hb_font_t)
  #:wrap (allocator hb_font_destroy))
(define-harfbuzz hb_font_reference (_fun [font : _hb_font_t] -> _hb_font_t -> font)
  #:wrap (retainer hb_font_destroy))
(define-harfbuzz hb_font_make_immutable (_fun _hb_font_t -> _void))
(define-harfbuzz hb_font_get_face (_fun _hb_font_t -> _hb_face_t))

(define-harfbuzz hb_font_get_nominal_glyph
  (_fun _hb_font_t
        _hb_codepoint_t/char
        [glyph : (_ptr o _hb_codepoint_t)]
        -> [found? : _bool]
        -> (and found? glyph)))

(define-harfbuzz hb_font_get_ppem
  (_fun _hb_font_t
        [x : (_ptr o _uint)]
        [y : (_ptr o _uint)]
        -> _void
        -> (values x y)))

(define-harfbuzz hb_font_get_scale
  (_fun _hb_font_t
        [x : (_ptr o _int)]
        [y : (_ptr o _int)]
        -> _void
        -> (values x y)))

;; -----------------------------------------------------------------------------
;; hb-ot-metrics and hb-ot-math

(define-harfbuzz hb_ot_metrics_get_position
  (_fun _hb_font_t
        _hb_ot_metrics_tag_t
        [pos : (_ptr o _hb_position_t)]
        -> [found? : _bool]
        -> (and found? pos)))

(define-harfbuzz hb_ot_math_has_data (_fun _hb_face_t -> _bool))
(define-harfbuzz hb_ot_math_get_constant
  (_fun _hb_font_t _hb_ot_math_constant_t -> _hb_position_t))
(define-harfbuzz hb_ot_math_get_glyph_italics_correction
  (_fun _hb_font_t _hb_codepoint_t -> _hb_position_t))

(define-harfbuzz hb_ot_math_get_glyph_kernings
  (_fun #:retry (again [count 0])
        _hb_font_t
        _hb_codepoint_t
        _hb_ot_math_kern_t
        [_uint = 0]
        [read-count : (_ptr io _uint) = count]
        [entries : (_cvector o _hb_ot_math_kern_entry_t count)]
        -> [kernings-count : _uint]
        -> (cond
             [(> kernings-count read-count) (again kernings-count)]
             [(zero? kernings-count)        #f]
             [else                          entries]))
  #:fail (λ () #f))
