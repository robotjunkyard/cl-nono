;;;; cl-nono.lisp

(in-package #:cl-nono)

(defparameter *solved-puzzles* '())
(defparameter *state* 'INTRO
  "States can be: INTRO, PLAYING, REVIEW")
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

(defun print-intro ()
  (let ((text #("+------cl-nono.lisp v0.92------+"
		"| a tiny nonogram puzzle clone |"
		"|     made in Common Lisp      |"
                "| email: njb@robotjunkyard.org |"
		"|                              |"
		"| HOW TO PLAY                  |"
		"| -----------                  |"
		"| Uncover  the  hidden picture |"
		"| by finding  only the squares |"
		"| which contain  pixels.       |"
		"|                              |"
		"| Rows and Columns have  hints |"
		"| given by numbers, indicating |"
		"| the existence of consecutive |"
		"| pixels along  them.          |"
		"|                              |"
		"| Flag squares you are certain |"
		"| are empty with an 'X' to aid |"
		"| you in your deductions.      |"
		"|                              |"
		"| Strive for zero mistakes!    |"
		"|                              |"
                "| LMouse: toggle on/off pixel  |"
                "| RMouse: flag square as X     |"
		"| F11   : window/fullscreen    |"
                "| Esc   : exit game any time   |"
                "|                              |"
		"| ON WORLD WIDE WEB            |"
		"| -----------------            |"
		"| http://robotjunkyard.itch.io |"
                "| Twitter: @robotjunkyard      |"
		"|                              |"
		"|    Press any key to begin    |"
		"|                              |"
		"+------------------------------+")))
    ;; print background stripes
    (loop for n below (min *x-res* *y-res*) by 8
       for i from 0 do
#|	 (sdl:draw-circle-* 
	  (truncate *x-res* 2)
	  (truncate *y-res* 2)
	  (truncate (+ n
		       (* 16
			  (sin (mod (* 0.0015 (sdl:sdl-get-ticks)) 26)))))
	  :color *off-color*)|#

	 (sdl:draw-circle-* 
	  (truncate *x-res* 2)
	  (truncate *y-res* 2)
	  (truncate (+ n
		       (mod (* 0.025 (sdl:sdl-get-ticks)) 16)))
	  :color (if (evenp i)
		     *outer-lines-color*
		     *outer-lines-color-lighter*)))

    ;; print splash text
    (let* ((visual-width (sdl:get-font-size (aref text 0) :size :w))
	   (visual-height (sdl:get-font-size (aref text 0) :size :h))
	   (x (- (truncate *x-res* 2) (truncate visual-width 2))))
      (sdl:draw-box-* x 0 visual-width
		      *y-res*
		      :color *off-color*)
      (loop for each-line across text
	 for y from 16 by (+ visual-height 4)
	 do
	   (sdl:draw-string-solid-* each-line x y
				    :color sdl:*black*)
	   (sdl:draw-string-solid-* each-line (- x 3) (- y 3)
				    :color sdl:*white*)))))

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
  (sdl:window
   *x-res* *y-res*
   :title-caption "cl-nono.lisp"
   :icon-caption "cl-nono.lisp"
   :double-buffer t
   :fullscreen *fullscreen*)
  (setf (sdl:frame-rate) 30)
  (unless (sdl:initialise-default-font sdl:*font-9x18b*)
    (error "Can not initialize font.")))

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

(defun parse-args-and-run ()
  (let ((arg (nth 1 sb-ext:*posix-argv*)))
    (if arg
	(let ((arg (string-downcase arg)))
	  (if (find arg (list-puzzles) :test #'equalp)
	      (main arg)
	      (format t "Error: puzzle '~a' not found!~%" arg)))
	(main))))
  
(defun main (&optional (name nil))
  (setq *random-state* (make-random-state t))
  (sdl:with-init (sdl:sdl-init-video)
    (setq *state* (if name 
		      (progn
			(init-board name)
			'PLAYING)
		      'INTRO))
    (setup-window)
    (sdl:with-events ()
      (:video-expose-event 
       ()
       (sdl:update-display))
      (:idle
       ()
       #+SWANK (update-swank)
       (cond
	 ((sdl:key-pressed-p :SDL-KEY-ESCAPE)
	  (save-game)
	  (sdl:quit-sdl)
	  ;; required or else "memory fault" fatal error
	  (return-from main))
	 ((sdl:key-pressed-p :SDL-KEY-F11)
	  (setq *fullscreen* (not *fullscreen*))
	  (setup-window)))
       (sdl:clear-display sdl:*black*)
       (when (and (member *state* '(PLAYING REVIEW))
		  *board*)
	 (draw-board *board*
		     *puzzle-x* *puzzle-y*
		     :pixel-size *pixel-size*
		     :draw-hints (eq *state* 'PLAYING)
		     :draw-grid  (eq *state* 'PLAYING)
		     :draw-only-on (not (eq *state* 'PLAYING)))
	 (setf (fill-pointer *status-text*) 0)
	 (let ((mistakes (board-mistakes *board*)))
	   (case *state*
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
	      (draw-puzzle-name)))))
       (when (eq *state* 'INTRO)
	 (print-intro))
       (sdl:update-display))

      (:key-down-event
       (:KEY key :MOD-KEY mod)     
       (if (and (eq *state* 'INTRO)
		(not (member key '(:SDL-KEY-F11
				   :SDL-KEY-ESCAPE))))
	   (if (probe-file "savegame.cons")
	       (progn
		 (setup-loaded-game (load-game))
		 (delete-savegame-file)
		 (setq *state* 'PLAYING))
	       (let ((puzzle-name 
		      (if name
			  name
			  (random-unsolved-puzzle-name))))
		 (setq *state* 'PLAYING)
		 (init-board puzzle-name)))
	   (case key
	     ;; Note that ESC already handled in INIT event further up
	     ((:SDL-KEY-R)
	      (when (eq *state* 'REVIEW)
		(init-board (puzzle-name (board-puzzle *board*)))
		(setq *state* 'PLAYING)))
	     ((:SDL-KEY-RETURN)
	      (when (eq *state* 'REVIEW)
		(let ((puzzle-name 
		       (random-unsolved-puzzle-name)))
		  (when puzzle-name
		    (init-board puzzle-name)
		    (setq *state* 'PLAYING))))))))
      
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
       (format t "-----~%Exiting already?!~%")
       (format t "Okay, fine, be that way...~%~%")
       t))))

(defun save-game-and-die ()
  (sb-ext:save-lisp-and-die
   "cl-nono"
   :executable t
   :toplevel #'parse-args-and-run
   :save-runtime-options t))
