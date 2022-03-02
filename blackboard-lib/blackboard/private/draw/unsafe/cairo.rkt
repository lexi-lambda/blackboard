#lang racket/base

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/cvector
         ffi/unsafe/define
         racket/draw/unsafe/cairo-lib
         racket/draw/unsafe/cairo)

(provide _cairo_scaled_font_t
         (struct-out cairo_matrix_t)
         (struct-out cairo_glyph_t)
         (struct-out cairo_text_cluster_t)
         make-cairo-vector

         cairo_get_matrix
         cairo_set_matrix
         cairo_translate

         cairo_font_face_reference
         cairo_font_face_destroy

         cairo_font_options_create
         cairo_font_options_destroy
         cairo_font_options_set_antialias
         cairo_font_options_set_hint_style
         cairo_font_options_set_hint_metrics

         cairo_scaled_font_create
         cairo_scaled_font_destroy
         cairo_scaled_font_get_ctm
         cairo_scaled_font_get_font_face
         cairo_scaled_font_get_font_matrix
         cairo_scaled_font_get_font_options

         cairo_save
         cairo_restore
         cairo_move_to
         cairo_new_path
         cairo_get_scaled_font
         cairo_set_scaled_font
         cairo_show_text_glyph
         cairo_show_text_glyphs)

;; -----------------------------------------------------------------------------
;; types

(define-cpointer-type _cairo_font_face_t)
(define-cpointer-type _cairo_font_options_t)
(define-cpointer-type _cairo_scaled_font_t)

(define _cairo_antialias_t (_enum '(default none gray subpixel fast good best)))
(define _cairo_hint_style_t (_enum '(default none slight medium full)))
(define _cairo_hint_metrics_t (_enum '(default off on)))

(define-cstruct _cairo_matrix_t
  ([xx _double*] [yx _double*]
   [xy _double*] [yy _double*]
   [x0 _double*] [y0 _double*])
  #:malloc-mode 'atomic-interior)

(define-cstruct _cairo_glyph_t
  ([index _ulong]
   [x _double*]
   [y _double*])
  #:malloc-mode 'atomic-interior)

(define-cstruct _cairo_text_cluster_t
  ([num-bytes _int]
   [num-glyphs _int])
  #:malloc-mode 'atomic-interior)

(define (make-cairo-vector type length)
  (make-cvector* (malloc type length 'atomic-interior)
                 type
                 length))

;; -----------------------------------------------------------------------------

(define-ffi-definer define-cairo cairo-lib)

(define-cairo cairo_font_face_destroy (_fun _cairo_font_face_t -> _void)
  #:wrap (releaser))
(define-cairo cairo_font_face_reference (_fun [in : _cairo_font_face_t] -> _cairo_font_face_t -> in)
  #:wrap (retainer cairo_font_face_destroy))

#;(begin
  (define-cairo cairo_font_options_destroy (_fun _cairo_font_options_t -> _void)
  #:wrap (deallocator))
(define-cairo cairo_font_options_create (_fun -> _cairo_font_options_t)
  #:wrap (allocator cairo_font_options_destroy)))

(define-cairo cairo_font_options_set_antialias (_fun _cairo_font_options_t _cairo_antialias_t -> _void))
(define-cairo cairo_font_options_set_hint_style (_fun _cairo_font_options_t _cairo_hint_style_t -> _void))
(define-cairo cairo_font_options_set_hint_metrics (_fun _cairo_font_options_t _cairo_hint_metrics_t -> _void))

(define-cairo cairo_scaled_font_destroy (_fun _cairo_scaled_font_t -> _void)
  #:wrap (releaser))
(define-cairo cairo_scaled_font_create
  (_fun _cairo_font_face_t
        _cairo_matrix_t-pointer
        _cairo_matrix_t-pointer
        _cairo_font_options_t
        -> _cairo_scaled_font_t)
  #:wrap (allocator cairo_scaled_font_destroy))
(define-cairo cairo_scaled_font_get_ctm (_fun _cairo_scaled_font_t _cairo_matrix_t-pointer -> _void))
(define-cairo cairo_scaled_font_get_font_face (_fun _cairo_scaled_font_t -> _cairo_font_face_t))
(define-cairo cairo_scaled_font_get_font_matrix (_fun _cairo_scaled_font_t _cairo_matrix_t-pointer -> _void))
(define-cairo cairo_scaled_font_get_font_options (_fun _cairo_scaled_font_t _cairo_font_options_t -> _void))

(define-cairo cairo_get_scaled_font (_fun _cairo_t -> _cairo_scaled_font_t))
(define-cairo cairo_set_scaled_font (_fun _cairo_t _cairo_scaled_font_t -> _void))

;; simple case of only one glyph
(define-cairo cairo_show_text_glyph
  (_fun _cairo_t
        [utf8 : _string/utf-8]
        [utf8_len : _int = (string-utf-8-length utf8)]
        _cairo_glyph_t-pointer
        [_int = 1]
        [_cairo_text_cluster_t-pointer = (make-cairo_text_cluster_t utf8_len 1)]
        [_int = 1]
        -> _void)
  #:c-id cairo_show_text_glyphs)

(define-cairo cairo_show_text_glyphs
  (_fun _cairo_t
        [utf8 : _string/utf-8]
        [_int = (string-utf-8-length utf8)]
        [glyphs : (_cvector i)]
        [_int = (cvector-length glyphs)]
        [clusters : (_cvector i)]
        [_int = (cvector-length clusters)]
        -> _void))
