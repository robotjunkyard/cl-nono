;;;; cl-nono.asd

(asdf:defsystem #:cl-nono
  :description "CL-NONO, a lightweight Nonogram game written in Common Lisp"
  :author "Nick Baker <njb@robotjunkyard.org>"
  :license "Closed, but will be open when project is completed"
  :depends-on (#:lispbuilder-sdl)
  :serial t
  :components ((:file "package")
	       (:file "types")
	       (:file "defs")
	       (:file "vars")
	       (:file "pictures")
	       (:file "puzzle")
	       (:file "board")
	       (:file "serial")
               (:file "cl-nono")))

