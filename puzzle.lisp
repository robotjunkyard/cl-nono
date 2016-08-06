(in-package :cl-nono)

(defun hints-row (bitmap row)
  (destructuring-bind (w h)
      (array-dimensions bitmap)
    (let ((runlen  0)
	  (hints   ()))
      (declare
       (type uint8 runlen)
       (type list hints))
      (loop for x below w do
	   (let ((this (aref bitmap x row)))
	     (when (= 1 this)
	       (incf runlen))
	     (when (and (or (= x (1- w))
			    (= 0 this))
			(< 0 runlen))
	       (push runlen hints)
	       (setq runlen 0))))
      (reverse hints))))

(defun hints-col (bitmap col)
  (destructuring-bind (w h)
      (array-dimensions bitmap)
    (let ((runlen  0)
	  (hints   ()))
      (declare
       (type uint8 runlen)
       (type list hints))
      (loop for y below h do
	   (let ((this (aref bitmap col y)))
	     (when (= 1 this)
	       (incf runlen))
	     (when (and (or (= y (1- w))
			    (= 0 this))
			(< 0 runlen))
	       (push runlen hints)
	       (setq runlen 0))))
      (reverse hints))))

(defstruct (puzzle
	     (:constructor 
	      make-puzzle
	      (bitmap)))
  (solution  bitmap :read-only t)
  (hints-v   (loop for col below (array-dimension bitmap 0)
		collect (hints-col bitmap col))
	     :read-only t)
  (hints-h   (loop for row below (array-dimension bitmap 1)
		collect (hints-col bitmap row)) 
	     :read-only t))

(defun draw-puzzle ()
 )
