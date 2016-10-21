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

;;;;;; cl-nono/board.lisp

(in-package #:cl-nono)

(defstruct (board
	     (:constructor
	      make-board
	      (puzzle)))
  (puzzle puzzle :read-only t)
  (squares (make-array (list (array-dimension (puzzle-solution puzzle) 0)
			     (array-dimension (puzzle-solution puzzle) 1))
		       :element-type '(integer -1 1)
		       :initial-element 0))
  (mistakes 0 :type uint16))

(defun center-board ()
  (when *board*
    (setq *puzzle-x*
	  (- (truncate *x-res* 2)
	     (truncate (* *pixel-size* 
			  (array-dimension 
			   (board-squares *board*) 0)) 2)))))

(let ((tbuf (make-array 16 :element-type 'character
			:adjustable t
			:fill-pointer 0)))
  (defun draw-board (board x y
		     &key 
		       (pixel-size 16)
		       (draw-hints t)
		       (draw-grid  t)
		       (draw-only-on nil)
		     &aux
		       (puzzle (board-puzzle board)))
    (declare (type board board)
	     (type uint8 pixel-size)
	     (type int16 x y)
	     (type boolean draw-hints draw-grid))
    ;; set "text cursor" of format for this text buffer back to start
    (setf (fill-pointer tbuf) 0)
    (draw-bitmap (board-squares board)
		 :pixel-size pixel-size :x x :y y :draw-grid draw-grid
		 :draw-outer-lines draw-grid
		 :draw-only-on draw-only-on)
    (when draw-hints
      ;; print row hints
      (loop
	 for  row-hint in (puzzle-hints-h puzzle)
	 with row = 0
	 do
	   (format tbuf "~v<~{ ~d~}~>" 11 (or row-hint '(0)))
	   (sdl:draw-string-solid-* tbuf
				    (- x 108) (+ y (* row pixel-size))
				    :color sdl:*white*)
	   (setf (fill-pointer tbuf) 0)
	   (incf row))
      
      ;; print column hints
      (loop
	 for  col-hints in (puzzle-hints-v puzzle)
	 with col = 0
	 with drx = (+ x (* col pixel-size))
	 do
	   (unless col-hints
	     (setq col-hints '(0)))
	   (loop 
	      with num-hints = (length col-hints)
	      for i below num-hints
	      for hint in col-hints
	      with by = (- y (* pixel-size num-hints))
	      for dry = (+ by (* i pixel-size))
	      do
		(format tbuf "~d" hint)
		(sdl:draw-string-solid-* 
		 tbuf
		 (+ drx (if (>= hint 10)
			    0
			    4))
		 dry)
		(setf (fill-pointer tbuf) 0))
	   (incf col)
	   (setf (fill-pointer tbuf) 0
		 drx (+ x (* col pixel-size)))))))

(defun determine-square-clicked-on (board scr-x scr-y
				    &key 
				      (tx *puzzle-x*)
				      (ty *puzzle-y*)
				      (pixel-size *pixel-size*))
  (declare (type board board)
	   (type int16 scr-x scr-y))
  (let ((mx (- scr-x tx))
	(my (- scr-y ty)))
    (when (and (< 0 mx (* pixel-size
			   (array-dimension (board-squares board) 0)))
	       (< 0 my (* pixel-size
			   (array-dimension (board-squares board) 1))))
      (values (truncate mx pixel-size)
	      (truncate my pixel-size)))))

(defun board-equals-solution? (board)
  (declare (type board board))
  (let ((squares (board-squares board))
	(solution (puzzle-solution (board-puzzle board))))
    (loop for x below (array-dimension squares 0) do
	 (loop for y below (array-dimension squares 1) do
	      (if (and (= 1 (aref solution x y))
		       (/= 1 (aref squares x y)))
		  (return-from board-equals-solution? nil)))))
  t)

