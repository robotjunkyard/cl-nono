;;;; cl-nono.lisp

(in-package #:cl-nono)

(defparameter *state* 'PLAYING
  "States can be: PLAYING, REVIEW")
(defparameter *board* nil)
(defparameter *status-text*
  (make-array 0 :element-type 'character
	      :adjustable t :fill-pointer 0))

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
#-SWANK  nil
#+SWANK  (continuable
          (let ((connection (or swank::*emacs-connection*
                                (swank::default-connection))))
            (when connection
              (swank::handle-requests connection t)))))

(defun setup-window ()
  (print "-------")
  (print sdl:*default-display*)
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
  
(defun force-mouse-to-center ()
  (setf (aref (sdl:mouse-position) 0)
	(truncate *x-res* 2)
	(aref (sdl:mouse-position) 1)
	(truncate *y-res* 2)))

(defun main ()
  (sdl:with-init (sdl:sdl-init-video)
    (setq *state* 'PLAYING)
    (setup-window)
    (setq *board* (make-board (load-puzzle "wine")))
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
		     :draw-grid  (eq *state* 'PLAYING))
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
	      (format *status-text* "YOU WIN, with ~d mistakes" mistakes)
	      (sdl:draw-string-solid-*
	       *status-text* 8 16
	       :color sdl:*white*)
	      (sdl:draw-string-solid-*
	       (puzzle-name (board-puzzle *board*))
	       *puzzle-x*
	       (- *puzzle-y* 24)
	       :color sdl:*green*)
	      )))
       (sdl:update-display)))

      (:key-down-event
       (:KEY key :MOD-KEY mod)       
       (case key
	 ((:SDL-KEY-F11)
	  (setq *fullscreen* (not *fullscreen*))
	  (setup-window))))

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
		  (setq *state* 'REVIEW))))
	     ((2 3) ;; right click
	      (let ((nuvalue -1))
		(setf (aref (board-squares *board*)
			    *hovered-square-x* *hovered-square-y*)
		      nuvalue))))
	 (setq *mouse-pressed* t))))))

      (:mouse-button-up-event
       (:button button :state state :x x :y y)
       (setq *mouse-pressed* nil))

      (:quit-event
       ()
       (format t "Okay, fine, be that way...~%")
       t))
