(in-package :cl-nono)

(defun clamp (val min max)
  (max (min val max) min))
