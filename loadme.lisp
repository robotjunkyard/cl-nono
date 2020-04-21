(load "package.lisp")
(in-package #:cl-nono)

(quicklisp:quickload "lispbuilder-sdl-image")
(quicklisp:quickload "lispbuilder-sdl")

(load "types.lisp")
(load "vars.lisp")
(load "pictures.lisp")
(load "puzzle.lisp")
(load "board.lisp")
(load "serial.lisp")
(load "cl-nono.lisp")

(format t "~%---------------~%")
(format t "CL-NONO loaded!~%")
(format t "---------------~%~%")
(format t "!! Run this command to begin playing:~%~%")
(format t "     (cl-nono:main)~%~%")
(format t "!! To exit Common Lisp and return to the shell, type:~%~%")
(format t "     (sb-ext:quit)~%")

