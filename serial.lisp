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
;;;;;; serial.lisp
;;;
;;; Serialization of savegames.
;;; Savegames are stored in a file called savegame.cons.  This file
;;; resides under a directory called .CL-NONO that gets created under
;;; the user's home directory as understood by the program.  In
;;; Linux this is usually /home/user.  In Windows this is sometimes
;;; C:\Users\username, but also sometimes is a deeper folder called
;;; C:\Users\username\AppData\Roaming.  In any case the game will
;;; print, to standard output, the location of savegame.cons when it
;;; saves your game upon exit.

(in-package #:cl-nono)

(defun savegame-path ()
  (merge-pathnames (make-pathname :directory '(:relative ".cl-nono")
				  :name "savegame"
				  :type "cons")
		   (user-homedir-pathname)))

(defun save-game ()
  (when *board*
    (let ((obj 
	   (list 
	    :SOLVED-PUZZLES *solved-puzzles*
	    :CURRENT-PUZZLE-NAME (puzzle-name (board-puzzle *board*))
	    :CURRENT-PUZZLE-PLAYER-SQUARES (board-squares *board*)
	    :CURRENT-PUZZLE-PLAYER-MISTAKES (board-mistakes *board*))))
      (ensure-directories-exist (savegame-path))
      (with-open-file (file (savegame-path)
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(format file "~S" obj)
	(format t "* Game saved to ~a~%" (savegame-path))
	(format t "  cl-nono will continue from this savegame the next time you play.~%")))))

(defun load-game ()
  ;; turn off reader macros for security
  (when (probe-file (savegame-path))
    (with-open-file (file (savegame-path)
			  :direction :input)
      (let ((data (make-string (file-length file))))
	(read-sequence data file)
	(read-from-string data)))))

(defun delete-savegame-file ()
  (when (probe-file (savegame-path))
    (delete-file (savegame-path))
    T))

(defun setup-loaded-game (sglist)
  (let ((solved-puzzles (getf sglist :SOLVED-PUZZLES))
	(current-puzzle-name (getf sglist :CURRENT-PUZZLE-NAME))
	(current-puzzle-player-squares
	 (getf sglist :CURRENT-PUZZLE-PLAYER-SQUARES))
	(current-puzzle-player-mistakes
	 (getf sglist :CURRENT-PUZZLE-PLAYER-MISTAKES)))
    (when (and current-puzzle-name current-puzzle-player-squares
	       current-puzzle-player-mistakes
	       (stringp current-puzzle-name)
      (let* ((puz (load-puzzle (string-downcase current-puzzle-name)))
	     (board (make-board puz)))
	(setf (board-squares board) current-puzzle-player-squares
	      (board-mistakes board) current-puzzle-player-mistakes
	      *board* board
	      *solved-puzzles* solved-puzzles))
      T))
    nil))
