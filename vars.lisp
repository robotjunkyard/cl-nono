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
;;;;;; vars.lisp

(in-package :cl-nono)

;;;;; (defparameter *font-for-hints* "dejavu")
(defparameter *fullscreen* nil)
(defparameter *mouse-pressed* nil)
(declaim (type boolean *fullscreen* *mouse-pressed*))

;; 640x480 is a pleasant resolution to work in and it is easy on the eyes
(defparameter *x-res* 640)
(defparameter *y-res* 480)
(declaim (type uint16 *x-res* *y-res*))

(defparameter *pixel-size* 20)  ;; onscreen box size of a puzzle's pixel
(defparameter *puzzle-x*   160)
(defparameter *puzzle-y*   104)

(defparameter *window-offset-x* 0)
(defparameter *window-offset-y* 0)
(declaim (type uint16 *window-offset-x* *window-offset-y*))

;; what tile is the mouse cursor hovering over in current frame?
(defparameter *hovered-square-x* -1)
(defparameter *hovered-square-y* -1)
(declaim (type int8 *hovered-tile-x* *hovered-tile-y*))

(defparameter *highlight-hovered-square* t)
(declaim (type boolean *highlight-hovered-square*))

(defparameter *highlight-color*
  (sdl:color :r 24
	     :g 64
	     :b 96))

(defparameter *solved-puzzles* '())
(defparameter *state* 'INTRO
  "States can be: INTRO, PLAYING, REVIEW")
(defparameter *board* nil)
(defparameter *status-text*
  (make-array 0 :element-type 'character
	      :adjustable t :fill-pointer 0))
(defparameter *end-of-game-text*
 "[ENTER] New Puzzle / [R] Retry Puzzle / [ESC] Exit")
