(in-package :cldk-display-xcb)

(defconstant IMAGE-FORMAT-XY-BITMAP #x00)
(defconstant IMAGE-FORMAT-XY-PIXMAP #x01)
(defconstant IMAGE-FORMAT-Z-PIXMAP #x02)
(cffi:defctype format-t :uint32)

(cffi:defcfun ("xcb_image_create_native" xcb-image-create-native)
    :pointer
  (c :pointer) 
  (width :uint16)
  (height :uint16)
  (format format-t)
  (depth :uint8)
  (base :pointer)
  (bytes :uint32)
  (data :pointer))

(cffi:defcfun ("xcb_image_destroy" xcb-image-destroy)
    :void
  (image :pointer))

(cffi:defcfun ("xcb_image_get_pixel" xcb-image-get-pixel)
    :uint32
  (image :pointer)
  (x :uint16)
  (y :uint16))

(cffi:defcfun ("xcb_image_put_pixel" xcb-image-put-pixel)
    :void
  (image :pointer)
  (x :uint16)
  (y :uint16)
  (pixel :uint32))

(cffi:defcfun ("xcb_image_put" xcb-image-put)
    :void
  (c :pointer)
  (draw window-t)
  (gc gcontext-t)
  (image :pointer)
  (x :int16)
  (y :int16)
  (left_pad :uint8))

#|
xcb_image_put
xcb_image_subimage
|#
#|
format = XCB_IMAGE_FORMAT_Z_PIXMAP;


printf ("get_image %d %d\n", width, height);
image = xcb_image_get (c, win,
			  0, 0, width, height,
			  0xaaaaaaaa,
			  format);


 pixel1 = xcb_image_get_pixel (image, left_x, y);
	  right_x = width - left_x-1;
	  if (left_x != right_x)
	    {
	      pixel2 = xcb_image_get_pixel (image, right_x, y);
	      xcb_image_put_pixel (image, left_x, y, pixel2);
	    }
xcb_image_put_pixel (image, right_x, y, pixel1);

xcb_image_put (c, new_win, gc, image, 0, 0, 0);
  image = xcb_image_get (c, new_win,
			 0, 0, width, height,
			 ~0,
			 format);

  fgcolor = xcb_generate_id(c);
  mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  valgc[0] = screen->black_pixel;
  valgc[1] = 0; /* no graphics exposures */
  xcb_create_gc(c, fgcolor, win, mask, valgc);

  bgcolor = xcb_generate_id(c);
  mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  valgc[0] = screen->white_pixel;
  valgc[1] = 0; /* no graphics exposures */
  xcb_create_gc(c, bgcolor, win, mask, valgc);
xcb_image_destroy(im);

	im = xcb_image_create_native(c, WIDTH, HEIGHT,
				        format, depth, 0, 0, 0);

 image = xcb_image_create_from_bitmap_data((uint8_t *)test_bits,
					      test_width, test_height);
    native_image = xcb_image_native(c, image, 1);
|#
