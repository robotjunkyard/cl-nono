;;;;;; cl-nono.asd
;;;
;;; This is an ASDF system definition file for the CL-NONO project.
;;;
;;; I've never futzed with ASDF directly too much, and IMO it is best to let
;;; Quicklisp by Zach Beane (http://www.quicklisp.org) handle this for you.

;;; Where quicklisp installs itself (usually 'quicklisp' dir under your respective
;;; user home directory), there will be a 'local-projects' directory.  In that,
;;; make directory symbolic links to the actual location of your project's main
;;; source folder.  For CL-NONO I have this directory junction made:
;;;
;;; Directory of C:\Users\Nick\AppData\Roaming\quicklisp\local-projects
;;;
;;; 10/21/2016  02:04 AM    <DIR>          .
;;; 10/21/2016  02:04 AM    <DIR>          ..
;;; 10/21/2016  02:04 AM    <JUNCTION>     cl-nono [c:\src\cl-nono]
;;; 10/21/2016  02:04 AM                40 system-index.txt
;;;
;;; With this 'CL-NONO' symbolic link here, Quicklisp allows me to load the
;;; entire CL-NONO project by simply type in the SBCL repl:
;;;
;;;   (quicklisp:quickload :CL-NONO)
;;;
;;; It will then see CL-NONO under local-projects, and then it knows what to
;;; load when it sees this very file you are reading right now.

(asdf:defsystem #:cl-nono
  :description "CL-NONO, a small, lightweight Nonogram puzzle game written in Common Lisp"
  :author "Nick Baker <njb@robotjunkyard.org>"
  :license "GPL3"
  :depends-on (#:lispbuilder-sdl-image
	       #:lispbuilder-sdl)
  :serial t
  :components ((:file "package")
	       (:file "types")
	       (:file "vars")
	       (:file "pictures")
	       (:file "puzzle")
	       (:file "board")
	       (:file "serial")
               (:file "cl-nono")))

