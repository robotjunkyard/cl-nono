(in-package :cl-nono)

(defparameter *on-color*  sdl:*white*)
(defparameter *off-color* (sdl:color :r (+ 12 12)
				     :g (+ 12 24)
				     :b (+ 12 48)))
(defparameter *grid-color sdl:*black*)

(defparameter *picture-names*
  '("dwarf" "note" "robot" "squirrel" "wine"))

(defparameter *bitmaps*
  (make-array 0 :adjustable t :fill-pointer 0))

(defparameter *draw-colors*
  #(sdl:*black*
    sdl:*white*))

(defun load-pictures ()
  (loop for picture-name in *picture-names* do
       (let*
	   ((image  (sdl:load-image
		     (format nil "gfx/~a.png" picture-name)))
	    (w      (sdl:width image))
	    (h      (sdl:height image))
	    (bitmap (make-array (list w h)
				:element-type 'BIT
				:initial-element (the bit 0)
				:adjustable nil)))
	 (loop for x below w do
	      (loop for y below h do
		   (multiple-value-bind
			 (r g b a)
		       (sdl:color-* (sdl:read-pixel-* x y
						      :surface image))
		     (setf (aref bitmap x y)
			   (if (= 0 r g b)
			       0 1)))))
	 (vector-push-extend
	  bitmap
	  *bitmaps*)
	 (sdl:free image)
	 )))

(defun draw-bitmap (bitmap
		    &key
		      pixel-size
		      (x 0)
		      (y 0)
		      (draw-grid  nil)
		      (off-color  *off-color*)
		      (on-color   *on-color*)
		      (grid-color sdl:*black*))
  (declare (type uint8 pixel-size)
	   (type int8 x y)
	   (type boolean draw-grid))
  (destructuring-bind (w h)
      (array-dimensions bitmap)
    (loop for bx below w do
	 (loop for by below h do
	      (let ((color (if (= 0 (aref bitmap bx by))
			       off-color
			       on-color)))
	      (sdl:draw-box-* (+ x (* pixel-size bx))
			      (+ y (* pixel-size by))
			      pixel-size
			      pixel-size
			      :color color)
	      (when draw-grid
		(sdl:draw-rectangle-* (+ x (* pixel-size bx))
				(+ y (* pixel-size by))
				pixel-size
				pixel-size
				:color grid-color))

)))))
