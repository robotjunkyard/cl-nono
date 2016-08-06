(in-package :cl-nono)

(defparameter *font-for-hints* "dejavu")
(defparameter *fullscreen* nil)
(defparameter *mouse-pressed* nil)
(declaim (type boolean *fullscreen* *mouse-pressed*))

(defparameter *x-res* 640)
(defparameter *y-res* 480)
(declaim (type uint16 *x-res* *y-res*))

(defparameter *window-offset-x* 0)
(defparameter *window-offset-y* 0)
(declaim (type uint16 *window-offset-x* *window-offset-y*))

;; what tile is the mouse cursor hovering over in current frame?
(defparameter *hovered-tile-x* -1)
(defparameter *hovered-tile-y* -1)
(declaim (type int16 *hovered-tile-x* *hovered-tile-y*))

