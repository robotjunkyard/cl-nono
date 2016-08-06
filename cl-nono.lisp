;;;; cl-nono.lisp

(in-package #:cl-nono)

(defparameter *puzzle* nil)

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
  (setf (sdl:frame-rate) 10)
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
    (setup-window)
    (setq *puzzle* (load-puzzle "squirrel"))
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
       (when *puzzle*
	 (draw-puzzle *puzzle*
		      *puzzle-x* *puzzle-y*
		      :pixel-size *pixel-size*)
		      )
       (when *puzzle*
	 (sdl:draw-string-solid-* 
	  (format nil (puzzle-name *puzzle*) 10 50)
	  0 0
	  :color sdl:*red*))
       (sdl:update-display))

      (:key-down-event
       (:KEY key :MOD-KEY mod)
       
       (case key
	 ((:SDL-KEY-F11)
	  (setq *fullscreen* (not *fullscreen*))
	  (setup-window))))

      (:mouse-motion-event
       (:x x :y y :x-rel x-rel :y-rel y-rel)

       )

      (:mouse-button-down-event
       (:button button :state state :x x :y y)
       (setq *mouse-pressed* t))

      (:mouse-button-up-event
       (:button button :state state :x x :y y)
       (setq *mouse-pressed* nil))

      (:quit-event
       ()
       (format t "Okay, fine, be that way...~%")
       t))))
