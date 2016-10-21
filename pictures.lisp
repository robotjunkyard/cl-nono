;;; CL-NONO, a small nonogram puzzle game for SDL written in Common Lisp
;;; Copyright (C) 2016  Nick Baker <njb@robotjunkyard.org>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;;;; pictures.lisp
;;;
;;; Using SDL to load and display pictures.

(in-package :cl-nono)

(defparameter *on-color*  sdl:*white*
  "Color of an 'on' pixel for a drawn bitmap/board.")
(defparameter *on-highlighted-color* sdl:*yellow*
  "Color of an 'on' pixel when mouse hovers over it.")
(defparameter *off-color* (sdl:color :r (+ 12 12)
				     :g (+ 12 24)
				     :b (+ 12 48))
  "Color of an 'off'/'no-pixel'")
(defparameter *grid-color* sdl:*black*
  "Color of the inner grid.")
(defparameter *outer-lines-color* (sdl:color :r 16
					     :g 44
					     :b 46)
  "Color of the outer-picture grid lines.")
(defparameter *outer-lines-color-lighter* (sdl:color :r (+ 24 16)
						     :g (+ 48 44)
						     :b (+ 48 46))
  "Color of the lighter gridlines, used to delineate 'every X number of squares' for quick, easy visual reading by the player.")

(defun load-picture (name)
  "Uses SDL to load a .PNG, then returns it as a simple bitmap.  Bitmap is a 2D array whose element-type is BIT.

In the source PNG, any pixel that is not a pure black (0, 0, 0) color is transformed into a 1."
  (let*
      ((image  (sdl:load-image
		(format nil "gfx/~a.png" (string-downcase name))
		:image-type :PNG
		))
       (w      (sdl:width image))
       (h      (sdl:height image))
       (bitmap (make-array (list w h)
			   :element-type 'BIT
			   :initial-element 0
			   :adjustable nil)))
    (loop for x below w do
	 (loop for y below h do
	      (multiple-value-bind
		    (r g b)
		  (sdl:color-* (sdl:read-pixel-* x y
						 :surface image))
		(setf (aref bitmap x y)
		      (if (= 0 r g b)
			  0 1)))))
    (sdl:free image)  ;; we don't need the SDL surface anymore
    bitmap))

(defun draw-bitmap (bitmap
		    &key
		      pixel-size
		      (x 0)
		      (y 0)
		      (draw-grid  nil)
		      (draw-outer-lines t)
		      (draw-only-on nil)
		      (off-color  *off-color*)
		      (on-color   *on-color*)
		      (grid-color *grid-color*)
		      (highlight-hovered *highlight-hovered-square*))
  "Draw a bitmap on the screen with various 'features' like the grid and various options.

A bitmap is a 2D array, but unlike what LOAD-PICTURE may presume it is NOT strictly of element-type BIT; it can draw those, but it will also accept the value -1 of a square to denote those 'Marked-As-No-Pixel' by the player, which is drawn as an X.

One may argue this is sloppy and incorrect use of nomenclature.  I'm inclined to agree :)"
  (declare (type uint8 pixel-size)
	   (type int16 x y)
	   (type boolean draw-grid highlight-hovered))
  (destructuring-bind (w h)
      (array-dimensions bitmap)
    ;; draw bitmap
    (loop for bx below w do
	 (loop for by below h 
	    for value = (aref bitmap bx by) do
	      (let ((color (if (>= 0 value)
			       (if (and highlight-hovered
					(or
					 (= bx *hovered-square-x*)
					 (= by *hovered-square-y*)))
				   *highlight-color*
				   off-color)
			       (if (and highlight-hovered
					(= bx *hovered-square-x*)
					(= by *hovered-square-y*))
				   *on-highlighted-color*
				   on-color))))
		(unless (and draw-only-on
			     (/= value 1))
		  (sdl:draw-box-* (+ x (* pixel-size bx))
				  (+ y (* pixel-size by))
				  pixel-size
				  pixel-size
				  :color color))
		(unless draw-only-on
		  (when (= -1 value)
		    (sdl:draw-line-* (+ x (* (1+ bx) pixel-size))
				     (+ y (* (1+ by) pixel-size))
				     (+ x (* bx pixel-size))
				     (+ y (* by pixel-size))
				     :color sdl:*red*)
		    (sdl:draw-line-* (+ x (* bx pixel-size))
				     (+ y (* (1+ by) pixel-size))
				     (+ x (* (1+ bx) pixel-size))
				     (+ y (* by pixel-size))
				     :color sdl:*red*)))
		(when draw-grid
		  (sdl:draw-rectangle-*
		   (+ x (* pixel-size bx))
		   (+ y (* pixel-size by))
		   pixel-size
		   pixel-size
		   :color grid-color)))))
    ;; draw outer lines, if needed
    (when draw-outer-lines 
      (loop for bx to w do
	   (sdl:draw-line-* (+ x (* bx pixel-size)) 0
			    (+ x (* bx pixel-size)) *y-res*
			    :color (if (= 0 (mod bx 4))
				       *outer-lines-color-lighter*
				       *outer-lines-color*)))
      (loop for by to h do
	   (sdl:draw-line-* 0 (+ y (* by pixel-size)) 
			    *x-res* (+ y (* by pixel-size))
			    :color (if (= 0 (mod by 4))
				       *outer-lines-color-lighter*
				       *outer-lines-color*))))))

