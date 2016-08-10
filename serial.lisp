(in-package #:cl-nono)

(defun save-game ()
  (when *board*
    (let ((obj 
	   (list 
	    :SOLVED-PUZZLES *solved-puzzles*
	    :CURRENT-PUZZLE-NAME (puzzle-name (board-puzzle *board*))
	    :CURRENT-PUZZLE-PLAYER-SQUARES (board-squares *board*)
	    :CURRENT-PUZZLE-PLAYER-MISTAKES (board-mistakes *board*))))
      (with-open-file (file "./savegame.cons" 
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(format file "~S" obj)
	(format t "* Game saved to savegame.cons~%")))))

(defun load-game ()
  ;; turn off reader macros for security
  (when (probe-file "./savegame.cons")
    (with-open-file (file "./savegame.cons"
			  :direction :input)
      (let ((data (make-string (file-length file))))
	(read-sequence data file)
	(read-from-string data)))))

(defun delete-savegame-file ()
  (when (probe-file "./savegame.cons")
    (delete-file "./savegame.cons")
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
