(in-package :cl-nono)

(defparameter *on-color*  sdl:*white*)
(defparameter *off-color* (sdl:color :r (+ 12 12)
				     :g (+ 12 24)
				     :b (+ 12 48)))
(defparameter *grid-color* sdl:*black*)
(defparameter *outer-lines-color* (sdl:color :r 16
					     :g 44
					     :b 46))

(defparameter *draw-colors*
  #(sdl:*black*
    sdl:*white*))

(defun load-picture (name)
  (let*
      ((image  (sdl:load-image
		(format nil "gfx/~a.png" (string-downcase name))))
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
    (sdl:free image)
    bitmap))

(defun draw-bitmap (bitmap
		    &key
		      pixel-size
		      (x 0)
		      (y 0)
		      (draw-grid  nil)
		      (draw-outer-lines t)
		      (off-color  *off-color*)
		      (on-color   *on-color*)
		      (grid-color *grid-color*))
  (declare (type uint8 pixel-size)
	   (type int16 x y)
	   (type boolean draw-grid))
  (destructuring-bind (w h)
      (array-dimensions bitmap)
    ;; draw bitmap
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
				:color grid-color)))))
    ;; draw outer lines, if needed
    (when draw-outer-lines 
      (loop for bx to w do
	   (sdl:draw-line-* (+ x (* bx pixel-size)) 0
			    (+ x (* bx pixel-size)) *y-res*
			    :color *outer-lines-color*))
      (loop for by to h do
	   (sdl:draw-line-* 0 (+ y (* by pixel-size)) 
			    *x-res* (+ y (* by pixel-size))
			    :color *outer-lines-color*)))))

