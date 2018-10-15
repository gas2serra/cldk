(in-package :cldk-driver-xcb)
;;/usr/include/xcb/xproto.h
;;/usr/include/xcb/xcb.h
(defconstant ATOM-NONE 0)
(defconstant ATOM-ANY 0)
(defconstant ATOM-PRIMARY 1)
(defconstant ATOM-SECONDARY 2)
(defconstant ATOM-ARC 3)
(defconstant ATOM-ATOM 4)
(defconstant ATOM-BITMAP 5)
(defconstant ATOM-CARDINAL 6)
(defconstant ATOM-COLORMAP 7)
(defconstant ATOM-CURSOR 8)
(defconstant ATOM-CUT-BUFFER0 9)
(defconstant ATOM-CUT-BUFFER1 10)
(defconstant ATOM-CUT-BUFFER2 11)
(defconstant ATOM-CUT-BUFFER3 12)
(defconstant ATOM-CUT-BUFFER4 13)
(defconstant ATOM-CUT-BUFFER5 14)
(defconstant ATOM-CUT-BUFFER6 15)
(defconstant ATOM-CUT-BUFFER7 16)
(defconstant ATOM-DRAWABLE 17)
(defconstant ATOM-FONT 18)
(defconstant ATOM-INTEGER 19)
(defconstant ATOM-PIXMAP 20)
(defconstant ATOM-POINT 21)
(defconstant ATOM-RECTANGLE 22)
(defconstant ATOM-RESOURCE-MANAGER 23)
(defconstant ATOM-RGB-COLOR-MAP 24)
(defconstant ATOM-RGB-BEST-MAP 25)
(defconstant ATOM-RGB-BLUE-MAP 26)
(defconstant ATOM-RGB-DEFAULT-MAP 27)
(defconstant ATOM-RGB-GRAY-MAP 28)
(defconstant ATOM-RGB-GREEN-MAP 29)
(defconstant ATOM-RGB-RED-MAP 30)
(defconstant ATOM-STRING 31)
(defconstant ATOM-VISUALID 32)
(defconstant ATOM-WINDOW 33)
(defconstant ATOM-WM-COMMAND 34)
(defconstant ATOM-WM-HINTS 35)
(defconstant ATOM-WM-CLIENT-MACHINE 36)
(defconstant ATOM-WM-ICON-NAME 37)
(defconstant ATOM-WM-ICON-SIZE 38)
(defconstant ATOM-WM-NAME 39)
(defconstant ATOM-WM-NORMAL-HINTS 40)
(defconstant ATOM-WM-SIZE-HINTS 41)
(defconstant ATOM-WM-ZOOM-HINTS 42)
(defconstant ATOM-MIN-SPACE 43)
(defconstant ATOM-NORM-SPACE 44)
(defconstant ATOM-MAX-SPACE 45)
(defconstant ATOM-END-SPACE 46)
(defconstant ATOM-SUPERSCRIPT-X 47)
(defconstant ATOM-SUPERSCRIPT-Y 48)
(defconstant ATOM-SUBSCRIPT-X 49)
(defconstant ATOM-SUBSCRIPT-Y 50)
(defconstant ATOM-UNDERLINE-POSITION 51)
(defconstant ATOM-UNDERLINE-THICKNESS 52)
(defconstant ATOM-STRIKEOUT-ASCENT 53)
(defconstant ATOM-STRIKEOUT-DESCENT 54)
(defconstant ATOM-ITALIC-ANGLE 55)
(defconstant ATOM-X-HEIGHT 56)
(defconstant ATOM-QUAD-WIDTH 57)
(defconstant ATOM-WEIGHT 58)
(defconstant ATOM-POINT-SIZE 59)
(defconstant ATOM-RESOLUTION 60)
(defconstant ATOM-COPYRIGHT 61)
(defconstant ATOM-NOTICE 62)
(defconstant ATOM-FONT-NAME 63)
(defconstant ATOM-FAMILY-NAME 64)
(defconstant ATOM-FULL-NAME 65)
(defconstant ATOM-CAP-HEIGHT 66)
(defconstant ATOM-WM-CLASS 67)
(defconstant ATOM-WM-TRANSIENT-FOR 68)

(defconstant CW-BACK-PIXMAP        #x0001)
(defconstant CW-BACK-PIXEL         #x0002)
(defconstant CW-BORDER-PIXMAP      #x0004)
(defconstant CW-BORDER-PIXEL       #x0008)
(defconstant CW-BIT-GRAVITY        #x0010)
(defconstant CW-WIN-GRAVITY        #x0020)
(defconstant CW-BACKING-STORE      #x0040)
(defconstant CW-BACKING-PLANES     #x0080)
(defconstant CW-BACKING-PIXELS     #x0100)
(defconstant CW-OVERRIDE-REDIRECT  #x0200)
(defconstant CW-SAVE-UNDER         #x0400)
(defconstant CW-EVENT-MASK         #x0800)
(defconstant CW-DONT-PROPAGATE     #x1000)
(defconstant CW-COLORMAP           #x2000)
(defconstant CW-CURSOR             #x4000)


(defconstant COPY-FROM-PARENT 0)
;;...
(defconstant WINDOW-CLASS-INPUT-OUTPUT 1)

;; property change
(defconstant PROP-MODE-REPLACE 0)
(defconstant PROP-MODE-PREPEND 1)
(defconstant PROP-MODE-APPEND  2)

;; config

(defconstant CONFIG-WINDOW-X #x01)
(defconstant CONFIG-WINDOW-Y #x02)
(defconstant CONFIG-WINDOW-WIDTH #x04)
(defconstant CONFIG-WINDOW-HEIGHT #x08)
(defconstant CONFIG-WINDOW-BORDER-WIDTH #x10)
(defconstant CONFIG-WINDOW-SIBLING #x20)
(defconstant CONFIG-WINDOW-STACK-MODE #x40)
(defconstant STACK-MODE-ABOVE #x00)
(defconstant STACK-MODE-BELOW #x01)
(defconstant STACK-MODE-TOP-IF #x02)
(defconstant STACK-MODE-BOTTOM-IF #x03)
(defconstant STACK-MODE-OPPOSITE #x04)

(cffi:defctype window-t    :uint32)
(cffi:defctype gcontext-t    :uint32)
(cffi:defctype visualid-t  :uint32)
(cffi:defctype colormap-t  :uint32)
(cffi:defctype atom-t      :uint32)
(cffi:defctype button-t    :uint8)
(cffi:defctype timestamp-t :uint32)
(cffi:defctype cursor-t :uint32)
(cffi:defctype font-t :uint32)

(cffi:defcstruct void-cookie-t
  (sequence       :uint16))

(cffi:defcstruct get-geometry-reply-t
  (response_type :uint8)
  (depth :uint8)
  (sequence :uint16)
  (length :uint32)
  (root window-t)
  (x :int16)
  (y :int16)
  (width :uint16)
  (height :uint16)
  (border-width :uint16))

(cffi:defcstruct generic-error-t
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (resource-id    :uint32)
  (minor-code     :uint16)
  (major_code     :uint8)
  (pad            :uint8 :count 21)
  (full-sequence  :uint32))
(cffi:defcstruct screen-t
  (root               window-t)
  (default-colormap   colormap-t)
  (white-pixel        :uint32)
  (black-pixel        :uint32)
  (current-input-masks :uint32)
  (width-in-pixels    :uint16)
  (height-in-pixels   :uint16)
  (width-in-mm        :uint16)
  (height-in-mm       :uint16)
  (min-installed-maps :uint16)
  (max-installed-maps :uint16)
  (root-visual        visualid-t)
  (backing-stores     :uint8)
  (save-unders        :uint8)
  (root-depth         :uint8)
  (allowed-depths-len :uint8)
  )

(cffi:defcstruct screen-iterator-t
  (data  :pointer)
  (rem   :int)
  (index :int))

(cffi:defcstruct intern-atom-cookie-t
  (sequence :uint))

(cffi:defcstruct intern-atom-reply-t
  (response-type  :uint8)
  (pad0           :uint8)
  (sequence       :uint16)
  (length         :uint32)
  (atom           :uint32) )

(cffi:defcstruct rectangle-t
  (x :int16) (y :int16)
  (width :uint16) (height :uint16))
;;==============================================================================
;;------------------------------------------------------------------------------
(cffi:defcfun ("xcb_request_check" request-check) :pointer
  (c :pointer)
  (cookie :uint32))
;;------------------------------------------------------------------------------
(cffi:defcfun ("xcb_connect" connect) :pointer
  (displayname :string)
  (screenp :pointer))
;;------------------------------------------------------------------------------
(cffi:defcfun ("xcb_disconnect" disconnect) :void
  (c :pointer))
;;------------------------------------------------------------------------------
(cffi:defcfun ("xcb_get_setup" get-setup) :pointer
  (c :pointer))
;;------------------------------------------------------------------------------
(cffi:defcfun ("xcb_setup_roots_iterator" setup-roots-iterator) (:struct screen-iterator-t)
  (setup :pointer))
;;------------------------------------------------------------------------------
(cffi:defcfun ("xcb_setup_roots_length" setup-roots-length) :int32
  (setup :pointer))
;;------------------------------------------------------------------------------
(cffi:defcfun ("xcb_generate_id" generate-id) :uint32
  (c :pointer))
;;------------------------------------------------------------------------------
(cffi:defcfun ("xcb_flush" xcb-flush) :int
  (c        :pointer))
;;==============================================================================
;;------------------------------------------------------------------------------
(defxcb ("xcb_create_window" xcb-create-window) 
  (depth        :uint8)
  (wid          window-t)
  (parent       window-t)
  (x :uint16)    (y :uint16)
  (width :uint16) (height :uint16)
  (border-width :uint16)
  (_class       :uint16)
  (visual       visualid-t)
  (value-mask   :uint32)
  (value-list   (:pointer :uint32)))

;;------------------------------------------------------------------------------
(defxcb ("xcb_map_window" xcb-map-window) 
  (window   window-t))

(defxcb ("xcb_unmap_window" xcb-unmap-window) 
  (window   window-t))

(defxcb ("xcb_destroy_window" xcb-destroy-window)
  (window   window-t))

(defxcb ("xcb_get_geometry" xcb-get-geometry)
  (window   window-t))

;;------------------------------------------------------------------------------
;; This one is weird as it cannot be checked.
;;
;; The reply is a buffer with atom (uint32) at offset 8...
(cffi:defcfun ("xcb_intern_atom" intern-atom) :uint32
  (c        :pointer)
  (only-if-exists :uint8)
  (name-len       :uint16)
  (name           :string))

(cffi:defcfun ("xcb_intern_atom_reply" intern-atom-reply)
    :pointer 
  (c        :pointer)
  (cookie   :uint32)
  (err      :pointer)) ;;null to ignore error


;;-
(defxcb ("xcb_change_property" change-property) 
  (mode     :uint8)
  (window   window-t)
  (property atom-t)
  (type     atom-t)
  (format   :uint8)
  (data-len :uint32)
  (data     :pointer))



(defxcb ("xcb_create_pixmap" create-pixmap)
  (depth     :uint32)
  (id        :uint32)
  (drawable  :uint32)
  (width     :uint16)
  (height    :uint16))


(defxcb ("xcb_free_pixmap" free-pixmap)
  (pixmap    :uint32))


(defxcb ("xcb_clear_area" clear-area)
  (exposures  :uint8)
  (window     :uint32)
  (x :int16)  (y :int16)
  (w :uint16) (h :uint16))



(cffi:defctype xcb-query-pointer-cookie-t :uint32)
(cffi:defcstruct xcb-query-pointer-replay-t
  (response-type :uint8)
  (same-screen :uint8)
  (sequence :uint16)
  (length :uint32)
  (root window-t)
  (child window-t)
  (root-x :int16)
  (root-y :int16)
  (win-x :int16)
  (win-y :int16)
  (mask :uint16)
  (pad0 :uint8)
  (pad1 :uint8))

(cffi:defcfun ("xcb_query_pointer" xcb-query-pointer) xcb-query-pointer-cookie-t
  (c :pointer)
  (window   window-t))
(cffi:defcfun ("xcb_query_pointer_reply" xcb-query-pointer-reply)
    (:pointer)
  (c :pointer)
  (cookie xcb-query-pointer-cookie-t)
  (err :pointer))

(cffi:defcfun ("xcb_get_geometry_reply" xcb-get-geometry-reply)
    (:pointer)
  (c :pointer)
  (cookie xcb-query-pointer-cookie-t)
  (err :pointer))



;;;
;;;
;;;

(cffi:defcstruct translate-coordinates-reply-t
  (response_type :uint8)
  (same-screen :uint8)
  (sequence :uint16)
  (length :uint32)
  (child window-t)
  (dest-x :int16)
  (dest-y :int16))

(cffi:defcstruct translate-coordinates-cookie-t
  (sequence :uint))

(defxcb ("xcb_translate_coordinates" xcb-translate-coordinates)
  (src-window window-t)
  (dst-window window-t)
  (src-x :int16)
  (src-y :int16))

(cffi:defcfun ("xcb_translate_coordinates_reply" xcb-translate-coordinates-reply)
    (:pointer (:struct translate-coordinates-reply-t))
  (c :pointer)
  (cookie translate-coordinates-cookie-t)
  (err :pointer))

(defxcb ("xcb_configure_window" xcb-configure-window)
  (window window-t)
  (value-mask :uint16)
  (value-list :pointer))

(defxcb ("xcb_change_window_attributes" xcb-change-window-attributes)
  (window window-t)
  (value-mask :uint16)
  (value-list :pointer))

(defxcb ("xcb_create_gc" xcb-create-gc)
  (gc gcontext-t)
  (window window-t)
  (value-mask :uint16)
  (value-list :pointer))

(defxcb ("xcb_free_cursor" xcb-free-cursor)
  (cursor cursor-t))

(defxcb ("xcb_create_glyph_cursor" xcb-create-glyph-cursor)
  (cid cursor-t)
  (font-source font-t)
  (mask-font font-t)
  (source-char :uint16)
  (mask-char :uint16)
  (f-red :uint16)
  (f-green :uint16)
  (f-blue :uint16)
  (b-red :uint16)
  (b-green :uint16)
  (b-blue :uint16))

(defxcb ("xcb_open_font" xcb-open-font)
  (fid font-t)
  (font-name-length :uint16)
  (font-name :string))

(defxcb ("xcb_put_image" xcb-put-image)
  (format :uint8)
  (draw window-t)
  (gc gcontext-t)
  (width :uint16)
  (height :uint16)
  (dest-x :uint16)
  (dest-y :int16)
  (left-pad :uint8)
  (depth :uint8)
  (size :uint32)
  (data :pointer))

#|xcb_put_image(conn, image->format, draw, gc,
		       image->width, image->height,
		       x, y, left_pad,
		       image->depth,
		       image->size,
		       image->data);

xcb_font_t font = xcb_generate_id (connection);
        xcb_void_cookie_t fontCookie = xcb_open_font_checked (connection,
                                                              font,
                                                              strlen ("cursor"),
                                                              "cursor" );
        testCookie (fontCookie, connection, "can't open font");

        xcb_cursor_t cursor = xcb_generate_id (connection);
        xcb_create_glyph_cursor (connection,
                                 cursor,
                                 font,
                                 font,
                                 cursorId,
                                 cursorId + 1,
                                 0, 0, 0, 0, 0, 0 );

        xcb_gcontext_t gc = xcb_generate_id (connection);

        uint32_t mask = XCB_GC_FOREGROUND | XCB_GC_BACKGROUND | XCB_GC_FONT;
        uint32_t values_list[3];
        values_list[0] = screen->black_pixel;
        values_list[1] = screen->white_pixel;
        values_list[2] = font;

        xcb_void_cookie_t gcCookie = xcb_create_gc_checked (connection, gc, window, mask, values_list);
        testCookie (gcCookie, connection, "can't create gc");

        mask = XCB_CW_CURSOR;
        uint32_t value_list = cursor;
        xcb_change_window_attributes (connection, window, mask, &value_list);

        xcb_free_cursor (connection, cursor);

        fontCookie = xcb_close_font_checked (connection, font);
        testCookie (fontCookie, connection, "can't close font");
|#
