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
;;;;;; types.lisp
;;;
;;; Even though declaring every integer 'FIXNUM' is just fine for such a
;;; tiny program as this one, as a matter of habit I like to give as many
;;; hints to the compiler as possible about ranges of integer variables
;;; whenever I can.  Overkill for something like this, but oh well.  Feel
;;; free to delete this file and switch all the DECLARE TYPE *INT8/16s
;;; back to FIXNUMs.
;;;
;;; I make these typedefs, specifically, because I am lazy and 'int8' is
;;; easier to type than 'signed-byte 8', etc.

(in-package :cl-nono)

(deftype uint8 ()
  '(unsigned-byte 8))
(deftype int8 ()
  '(signed-byte 8))
(deftype uint16 ()
  '(unsigned-byte 16))
(deftype int16 ()
  '(signed-byte 16))
