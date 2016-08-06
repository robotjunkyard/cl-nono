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
	      (name bitmap)))
  (name      name)
  (solution  bitmap :read-only t)
  (hints-v   (loop for col below (array-dimension bitmap 0)
		collect (hints-col bitmap col))
	     :read-only t)
  (hints-h   (loop for row below (array-dimension bitmap 1)
		collect (hints-row bitmap row)) 
	     :read-only t))

(defun load-puzzle (name)
  (let ((bitmap (load-picture name)))
    (make-puzzle name bitmap)))

(defun draw-puzzle (puzzle x y &key (pixel-size *pixel-size*))
  (let ((tbuf (make-array 16 :element-type 'character
			  :adjustable t
			  :fill-pointer 0)))
    (draw-bitmap (puzzle-solution puzzle)
		 :pixel-size pixel-size :x x :y y :draw-grid t)

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
	    with by  = (- y (* pixel-size num-hints))
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
	       drx (+ x (* col pixel-size))))))


