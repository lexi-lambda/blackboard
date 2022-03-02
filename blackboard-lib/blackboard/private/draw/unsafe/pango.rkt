#lang racket/base

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/cvector
         ffi/unsafe/define
         racket/draw/unsafe/glib
         racket/draw/unsafe/pango
         racket/flonum
         (only-in "cairo.rkt" _cairo_scaled_font_t)
         (only-in "harfbuzz.rkt" _hb_font_t))

(provide (struct-out GSList)
         (struct-out PangoRectangle)
         (struct-out PangoGlyphInfo)
         (struct-out PangoGlyphString) PangoGlyphString-glyphs
         (struct-out PangoGlyphItem)
         (struct-out PangoLayoutLine) PangoLayoutLine-first-run PangoLayoutLine-last-run

         pango_version_string
         g_object_ref
         g_object_unref

         pango_language_from_string
         pango_language_get_default
         pango_language_to_string

         pango_font_description_new
         pango_font_description_free
         pango_font_description_get_family
         pango_font_description_get_set_fields
         pango_font_description_get_size
         pango_font_description_get_style
         pango_font_description_get_weight
         pango_font_description_set_absolute_size
         pango_font_description_set_family
         pango_font_description_set_style
         pango_font_description_set_weight

         pango_font_describe_with_absolute_size
         pango_font_get_hb_font
         pango_cairo_font_get_scaled_font

         pango_fontset_foreach

         pango_font_map_create_context
         pango_font_map_load_font
         pango_font_map_load_fontset
         pango_cairo_font_map_new
         pango_cairo_font_map_set_resolution

         pango_cairo_context_set_font_options
         pango_cairo_update_context

         pango_attr_fallback_new
         pango_attr_font_features_new

         pango_attr_list_new
         pango_attr_list_unref
         pango_attr_list_insert

         pango_layout_new
         pango_layout_get_line_readonly
         pango_layout_set_attributes
         pango_layout_set_font_description
         pango_layout_set_single_paragraph_mode
         pango_layout_set_text
         pango_cairo_show_layout

         pango_layout_line_get_extents
         pango_cairo_show_layout_line

         px->pango
         pango->px)

;; -----------------------------------------------------------------------------
;; types

(define _PangoGlyph _uint32)
(define _PangoGlyphUnit _int32)

(define-cpointer-type _PangoContext)
(define-cpointer-type _PangoFont)
(define-cpointer-type _PangoFontset)
(define-cpointer-type _PangoFontDescription)
(define-cpointer-type _PangoFontMap)
(define-cpointer-type _PangoLanguage)
(define-cpointer-type _PangoLayout)

(define _PangoStyle
  (_enum '(normal
           oblique
           italic)
         #:unknown 'normal))

(define _PangoFontMask
  (_bitmask '(family
              style
              variant
              weight
              stretch
              size
              gravity
              variations)))

(define-cstruct _GSList
  ([data _pointer]
   [next _GSList-pointer])
  #:malloc-mode 'atomic-interior)

(define-cstruct _PangoRectangle
  ([x _int]
   [y _int]
   [width _int]
   [height _int])
  #:malloc-mode 'atomic-interior)

(define-cstruct _PangoAnalysis
  ([shape-engine _pointer]
   [lang-engine _pointer]
   [font _PangoFont]
   [level _uint8]
   [gravity _uint8]
   [flags _uint8]
   [script _uint8]
   [language _PangoLanguage]
   [extra-attrs _GSList-pointer/null])
  #:malloc-mode 'atomic-interior)

(define-cstruct _PangoItem
  ([offset _int]
   [length _int]
   [num-chars _int]
   [analysis _PangoAnalysis])
  #:malloc-mode 'atomic-interior)

(define-cstruct _PangoGlyphGeometry
  ([width _PangoGlyphUnit]
   [x-offset _PangoGlyphUnit]
   [y-offset _PangoGlyphUnit])
  #:malloc-mode 'atomic-interior)

(define-cstruct _PangoGlyphVisAttr
  ([flags _uint])
  #:malloc-mode 'atomic-interior)

(define-cstruct _PangoGlyphInfo
  ([glyph _PangoGlyph]
   [geometry _PangoGlyphGeometry]
   [attr _PangoGlyphVisAttr])
  #:malloc-mode 'atomic-interior)

(define-cstruct _PangoGlyphString
  ([num-glyphs _int]
   [glyphs-pointer _PangoGlyphInfo-pointer]
   [log-clusters _pointer])
  #:malloc-mode 'atomic-interior)

(define (PangoGlyphString-glyphs pgs)
  (make-cvector* (PangoGlyphString-glyphs-pointer pgs)
                 _PangoGlyphInfo
                 (PangoGlyphString-num-glyphs pgs)))

(define-cstruct _PangoGlyphItem
  ([item _PangoItem-pointer]
   [glyphs _PangoGlyphString-pointer]
   [y-offset _int]
   [start-x-offset _int]
   [end-x-offset _int])
  #:malloc-mode 'atomic-interior)

(define-cstruct _PangoLayoutLine
  ([layout _PangoLayout]
   [start-index _int]
   [length _int]
   [runs _GSList-pointer/null]
   [flags _uint])
  #:malloc-mode 'atomic-interior)

(define (PangoLayoutLine-first-run pll)
  (and (PangoLayoutLine-runs pll)
       (cast (GSList-data (PangoLayoutLine-runs pll))
             _pointer
             _PangoGlyphItem-pointer)))

(define (PangoLayoutLine-last-run pll)
  (and (PangoLayoutLine-runs pll)
       (cast (GSList-data (g_slist_last (PangoLayoutLine-runs pll)))
             _pointer
             _PangoGlyphItem-pointer)))

;; -----------------------------------------------------------------------------

(define-glib g_slist_last (_fun _GSList-pointer/null -> _GSList-pointer/null))
(define-gobj g_object_ref (_fun [object : _pointer] -> _pointer -> object)
  #:wrap (retainer g_object_unref))

;; -----------------------------------------------------------------------------

(define-ffi-definer define-pango pango-lib)
(define-ffi-definer define-pangocairo pangocairo-lib)

(define-pango pango_version_string (_fun -> _string/utf-8))

(define-pango pango_language_from_string (_fun _string/utf-8 -> _PangoLanguage))
(define-pango pango_language_to_string (_fun _PangoLanguage -> _string/utf-8))

(define-pango pango_font_description_get_family (_fun _PangoFontDescription -> _string/utf-8))
(define-pango pango_font_description_get_set_fields (_fun _PangoFontDescription -> _PangoFontMask))
(define-pango pango_font_description_get_size (_fun _PangoFontDescription -> _int))
(define-pango pango_font_description_get_style (_fun _PangoFontDescription -> _PangoStyle))
(define-pango pango_font_description_get_weight (_fun _PangoFontDescription -> _int))
(define-pango pango_font_description_set_style (_fun _PangoFontDescription _PangoStyle -> _void))

(define-pango pango_font_describe_with_absolute_size (_fun _PangoFont -> _PangoFontDescription)
  #:wrap (allocator pango_font_description_free))
(define-pango pango_font_get_hb_font (_fun _PangoFont -> _hb_font_t))
(define-pangocairo pango_cairo_font_get_scaled_font (_fun _PangoFont -> _cairo_scaled_font_t))

(define-pango pango_fontset_foreach
  (_fun [fontset : _PangoFontset]
        (_cprocedure (list _PangoFontset _PangoFont _pointer) _bool
                     #:keep #f
                     #:atomic? #t
                     #:wrapper (λ (proc) (λ (fontset font ptr) (proc font))))
        [_pointer = #f]
        -> _void
        -> (void/reference-sink fontset)))

(define-pango pango_font_map_load_fontset
  (_fun _PangoFontMap
        _PangoContext
        _PangoFontDescription
        _PangoLanguage
        -> _PangoFontset/null)
  #:wrap (allocator g_object_unref))
(define-pangocairo pango_cairo_font_map_set_resolution (_fun _PangoFontMap _double -> _void))

(define-pango pango_layout_set_single_paragraph_mode (_fun _PangoLayout _bool -> _void))
(define-pango pango_layout_line_get_extents
  (_fun _PangoLayoutLine-pointer
         [ink : (_ptr o _PangoRectangle atomic-interior)]
         [logical : (_ptr o _PangoRectangle atomic-interior)]
         -> _void
         -> (values ink logical)))

(define PANGO_SCALE.0 (->fl PANGO_SCALE))

(define (px->pango v)
  (* v PANGO_SCALE.0))
(define (pango->px v)
  (fl/ (->fl v) PANGO_SCALE.0))
