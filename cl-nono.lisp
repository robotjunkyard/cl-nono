;;;; cl-nono.lisp

(in-package #:cl-nono)

(defparameter *solved-puzzles* '())
(defparameter *state* 'PLAYING
  "States can be: PLAYING, REVIEW")
(defparameter *board* nil)
(defparameter *status-text*
  (make-array 0 :element-type 'character
	      :adjustable t :fill-pointer 0))

(defparameter *end-of-game-text*
 "[ENTER] New Puzzle / [R] Retry Puzzle / [ESC] Exit")

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun list-puzzles ()
  (loop for x in (directory "gfx/*.png")
     collect
       (string-downcase
	(pathname-name x))))

(defun draw-puzzle-name ()
  (sdl:draw-string-solid-*
   (puzzle-name (board-puzzle *board*))
   (truncate (+ *puzzle-x* (* 32.0 (sin (* (sdl:system-ticks) 0.00225)))))
   (- *puzzle-y* 24)
   :color (cycle-victory-color)))

(defun find-excessively-hinted-puzzles ()
  "Scans every puzzle and returns any that have hint lines with more than five hints."
  (let ((puzzle-objs 
	 (loop for puzzle-name in (list-puzzles) collect
	      (load-puzzle puzzle-name))))
    (loop for puz in puzzle-objs 
       when (find-if #'(lambda (lst)
			 (< 5 (length lst)))
		     (append (puzzle-hints-v puz)
			     (puzzle-hints-h puz)))
       collect puz)))

(defun update-swank ()
#-SWANK  nil
#+SWANK  (continuable
          (let ((connection (or swank::*emacs-connection*
                                (swank::default-connection))))
            (when connection
              (swank::handle-requests connection t)))))

(let ((i 0)
      (spin-chars #(#\| #\/ #\- #\\)))
  (defun spin-char ()
    (setq i (mod (1+ i) (length spin-chars)))
    (aref spin-chars i)))

(let ((i 0)		  
      (victory-colors
       (coerce (list sdl:*white*
		     sdl:*red*
		     sdl:*blue*
		     sdl:*green*
		     sdl:*cyan*
		     sdl:*magenta*)
	       'vector)))
  (defun cycle-victory-color ()
    (setq i (mod (1+ i) (length victory-colors)))
    (aref victory-colors i)))
			

(defun setup-window ()
  (print
   (sdl:window
    *x-res* *y-res*
    :title-caption "cl-nono.lisp"
    :icon-caption "cl-nono.lisp"
    :double-buffer t
    :fullscreen *fullscreen*))
  (setf (sdl:frame-rate) 30)
  (unless (sdl:initialise-default-font sdl:*font-9x18b*)
    (error "Can not initialize font."))
  (print sdl:*default-display*))

(defun init-board (name)
  (setq *board* (make-board (load-puzzle (string-downcase name))))
  (center-board))
  
(defun random-unsolved-puzzle-name ()
  (let* ((puzzle-names (set-difference (list-puzzles) *solved-puzzles*
				      :test #'equalp))
	 (num-puzzles (length puzzle-names)))
    (if (= num-puzzles 0)
	nil
	(nth (random num-puzzles) puzzle-names))))

(defun force-mouse-to-center ()
  (setf (aref (sdl:mouse-position) 0)
	(truncate *x-res* 2)
	(aref (sdl:mouse-position) 1)
	(truncate *y-res* 2)))

(defun main (&optional (name nil))
  (sdl:with-init (sdl:sdl-init-video)
    (setq *state* 'PLAYING)
    (setup-window)
    (let ((puzzle-name 
	   (if name
	       name
	       (random-unsolved-puzzle-name))))
      (init-board puzzle-name))
    (sdl:with-events ()
      (:video-expose-event 
       ()
       (sdl:update-display))
      (:idle
       ()
       #+SWANK (update-swank)
       (cond
	 ((sdl:key-pressed-p :SDL-KEY-ESCAPE)
	  (sdl:quit-sdl)
	  ;; required or else "memory fault" fatal error
	  (return-from main)))
       (sdl:clear-display sdl:*black*)
       (when *board*
	 (draw-board *board*
		     *puzzle-x* *puzzle-y*
		     :pixel-size *pixel-size*
		     :draw-hints (eq *state* 'PLAYING)
		     :draw-grid  (eq *state* 'PLAYING)
		     :draw-only-on (not (eq *state* 'PLAYING)))
	 (setf (fill-pointer *status-text*) 0)
	 (let ((mistakes (board-mistakes *board*)))
	   (ecase *state*
	     (PLAYING
	      (format *status-text* "mistakes: ~d" mistakes)
	      (sdl:draw-string-solid-* 
	       *status-text* 8 16
	       :color (if (= 0 mistakes) sdl:*green*
			  sdl:*red*)))
	     (REVIEW
	      (let ((spin (spin-char)))
		(format *status-text* "~a YOU WIN, with ~d mistakes ~a"
			spin mistakes spin))
	      (sdl:draw-string-solid-*
	       *status-text* 8 16
	       :color sdl:*white*)
	      (sdl:draw-string-solid-*
	       *end-of-game-text* 8 0
	       :color sdl:*red*)
	      (draw-puzzle-name))))
       (sdl:update-display)))

      (:key-down-event
       (:KEY key :MOD-KEY mod)     
       (format t "Key ~a pressed~%" key)
       (case key
	 ;; Note that ESC already handled in INIT event further up
	 ((:SDL-KEY-F11)
	  (setq *fullscreen* (not *fullscreen*))
	  (setup-window))
	 ((:SDL-KEY-R)
	  (when (eq *state* 'REVIEW)
	    (init-board (puzzle-name (board-puzzle *board*)))
	    (setq *state* 'PLAYING)))
	 ((:SDL-KEY-RETURN)
	  (format t "Derp~%")
	  (when (eq *state* 'REVIEW)
	    (let ((puzzle-name 
		   (random-unsolved-puzzle-name)))
	      (when puzzle-name
		(init-board puzzle-name)
		(setq *state* 'PLAYING)))))))

      (:mouse-motion-event
       (:x x :y y :x-rel x-rel :y-rel y-rel)
       (when *board*
	 (multiple-value-bind
	       (sqx sqy)
	     (determine-square-clicked-on *board*
					  x y)
	   (setq *hovered-square-x*  (or sqx -1)
		 *hovered-square-y*  (or sqy -1)))))

      (:mouse-button-down-event
       (:button button :state state :x x :y y)
       (format t "BTN ~a,  STATE ~a~%" button state)
       (when (and 
	      (eq *state* 'PLAYING)
	      *board*
	      (<= 0 *hovered-square-x* 
		 (1- (array-dimension (board-squares *board*) 0)))
	      (<= 0 *hovered-square-y*
		 (1- (array-dimension (board-squares *board*) 1))))
	 (let ((sqvalue (aref (board-squares *board*)
			      *hovered-square-x* *hovered-square-y*)))
	   (case button
	     (1     ;; left click
	      (let ((nuvalue (if (= sqvalue 0) 1 0)))
		(setf (aref (board-squares *board*)
			    *hovered-square-x* *hovered-square-y*)
		      nuvalue)
		(if (and (= 1 nuvalue)
			 (/= 1 (aref 
				(puzzle-solution 
				 (board-puzzle *board*))
				*hovered-square-x*
				*hovered-square-y*)))
		    (incf (board-mistakes *board*)))
		(when (board-equals-solution? *board*)
		  (setq *state* 'REVIEW)
		  (pushnew (puzzle-name (board-puzzle *board*))
			   *solved-puzzles*))))
	     ((2 3) ;; right click
	      (let ((nuvalue -1))
		(setf (aref (board-squares *board*)
			    *hovered-square-x* *hovered-square-y*)
		      nuvalue))))
	 (setq *mouse-pressed* t))))

      (:mouse-button-up-event
       (:button button :state state :x x :y y)
       (setq *mouse-pressed* nil))

      (:quit-event
       ()
       (format t "Okay, fine, be that way...~%")
       t))))
