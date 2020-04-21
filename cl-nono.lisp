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

;;;;;; cl-nono.lisp

(in-package #:cl-nono)

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

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
    "Use the amazing, awesome power of *~CLOSURES~* to abstract-away the cycling of colors!"
    (setq i (mod (1+ i) (length victory-colors)))
    (aref victory-colors i)))

(defun print-intro ()
  (let ((text #("+------cl-nono.lisp v0.99------+"
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
	 (sdl:draw-circle-* 
	  (truncate *x-res* 2)
	  (truncate *y-res* 2)
	  (truncate (+ n (mod (* 0.025 (sdl:sdl-get-ticks)) 16)))
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
	   ;; shadow
	   (sdl:draw-string-solid-* each-line x y
				    :color sdl:*black*)
	   ;; white text above shadow
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
  "Scans every puzzle and returns any that have hint lines with more than five hints.  Mainly used during development-time."
  (let ((puzzle-objs 
	 (loop for puzzle-name in (list-puzzles) collect
	      (load-puzzle puzzle-name))))
    (loop for puz in puzzle-objs 
       when (find-if #'(lambda (lst)
			 (< 5 (length lst)))
		     (append (puzzle-hints-v puz)
			     (puzzle-hints-h puz)))
       collect puz)))

;; If SWANK feature is present, that is because SBCL is currently running
;; under an emacs+SLIME environment.  This will enable us to drop to the
;; SBCL REPL within emacs while the CL-NONO program is running, which allows
;; for neat things like changing functions or variables in real-time without
;; shutting down or restarting anything (usually, at least).
;;
;; This gets called in 'main' further down, in every idle frame.
(defun update-swank ()
#-SWANK  nil
#+SWANK  (continuable
          (let ((connection (or swank::*emacs-connection*
                                (swank::default-connection))))
            (when connection
              (swank::handle-requests connection t))))
)

(let ((i 0)
      (spin-chars #(#\| #\/ #\- #\\)))
  (defun spin-char ()
    (setq i (mod (1+ i) (length spin-chars)))
    (aref spin-chars i)))
			
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
 
(defun main (&optional (name nil))
  (setq *random-state* (make-random-state t))
  (sdl:with-init (sdl:sdl-init-video)
    ;; if parameter specified, bypass splash and jump right into puzzle
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
	 (draw-board *board* *puzzle-x* *puzzle-y*
		     :pixel-size   *pixel-size*
		     :draw-hints   (eq *state* 'PLAYING)
		     :draw-grid    (eq *state* 'PLAYING)
		     :draw-only-on (not (eq *state* 'PLAYING)))
	 ;; set "cursor" for FORMAT back to index 0
	 (setf (fill-pointer *status-text*) 0)
	 (let ((mistakes (board-mistakes *board*)))
	   (case *state*
	     (PLAYING
	      (format *status-text* "mistakes: ~d" mistakes)
	      (sdl:draw-string-solid-* 
	       *status-text* 8 16
	       :color (if (= 0 mistakes)
			  sdl:*green*
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
       (:KEY key)     
       (if (and (eq *state* 'INTRO)
		(not (member key '(:SDL-KEY-F11
				   :SDL-KEY-ESCAPE))))
	   (if (probe-file (savegame-path))
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
       (:x x :y y)
       (when *board*
	 (multiple-value-bind
	       (sqx sqy)
	     (determine-square-clicked-on *board*
					  x y)
	   (setq *hovered-square-x*  (or sqx -1)
		 *hovered-square-y*  (or sqy -1)))))
      
      (:mouse-button-down-event
       (:button button)
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
       ()
       ;;(:button button :state state :x x :y y)
       (setq *mouse-pressed* nil))

      (:quit-event
       ()
       (format t "-----~%Exiting already?!~%")
       (format t "Okay, fine, be that way...~%~%")
       t))))

(defun parse-args-and-run ()
  (let ((arg (nth 1 sb-ext:*posix-argv*)))
    (if arg
	(let ((arg (string-downcase arg)))
	  (if (find arg (list-puzzles) :test #'equalp)
	      (main arg)
	      (format t "Error: puzzle '~a' not found!~%" arg)))
	(main))))

(defun save-game-and-die ()
  (sb-ext:save-lisp-and-die
   "cl-nono"
   :executable t
   :toplevel #'parse-args-and-run
   :save-runtime-options t
   ;; if ZLIB was compiled into your copy of SBCL (for some dumb reason it
   ;; is not included in the default binaries on sbcl.org's website),
   ;; then uncomment this to generate a smaller executable file.
   ;;;; :compression 9 
   ))
