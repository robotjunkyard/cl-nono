cl-nono.lisp
by:  njb@robotjunkyard.org

A Common Lisp adaptation of a nonogram type puzzle game.
These games are also known as "Picross", "Paint By Numbers", etc.

Made Aug 5-6, 2016, using Steel Bank Common Lisp 1.3.1 x64 for Linux.

Requirements:
   Quicklisp        http://www.quicklisp.org

Only 16x16 graphics have been tested.  New ones can be made in
.PNG format.  The game loads them as pure 0/1 bitmaps, so any color
in the source .PNG not strictly a (0, 0, 0) will be treated as
a 1.

Controls:
  Right Mouse: Mark square as "Do Not Touch", denoted by a Red X
  Left Mouse:  Mark square as a Pixel, or
       	       Unmarks "Do Not Touch" square as blank non-Pixel square.
  Escape:      Exit Game
