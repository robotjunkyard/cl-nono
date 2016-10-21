CL-NONO
version 0.99
by njb@robotjunkyard.org

http://robotjunkyard.itch.io

An adaptation of a nonogram-type puzzle game.
These games are also alternatively known around
the market as "Picross", "Paint By Numbers", etc.

Wikipedia article on Nonogram puzzles:
https://en.wikipedia.org/wiki/Nonogram

System Requirements:
   Linux or Windows
   libsdl  (Uses 1.x, not 2.0)

Controls:
  Right Mouse: Mark square as "Do Not Touch", denoted by a Red X
  Left Mouse:  Mark square as a Pixel, or
       	       Unmarks "Do Not Touch" square as blank non-Pixel square.
  F11:         Toggle Fullscreen/Window
  Escape:      Exit Game

Notes:
Made Aug 5-8, 2016, using Steel Bank Common Lisp.

Due to ridiculous file sizes of executables generated
by SBCL and how tiny this game is, I've decided it to
be a source-only release.  I have included instructions
how to load and build in README-SOURCECODE.txt.

Only 16x16 graphics have been tested and included.
New pictures can be made in .PNG format in the 'gfx'
folder.  The game loads them as pure 0/1 bitmaps, so
any color in the source .PNG not strictly a purely
black (0, 0, 0) color will be treated as a 1.

Send bug reports, comments, flames, praises,
and/or suggestions to:  njb@robotjunkyard.org


CHANGELOG
---------
Updated in v0.99:
  - Update serialization to put savegame.cons in OS-independent folder.
  - Add some more comments and musings in source files here-and-there.
  - LISPBUILDER-SDL could not load PNG files in Windows version of Lisp,
    only BMP files without SDL_IMAGE.  So, added LISPBUILDER-SDL-IMAGE
    package to dependencies in CL-NONO.ASD.
  - Now using GPLv3 license.
Updated in v0.92:
  - Now properly initializes random state, so order of puzzles are now
    jumbled and not the same every time.
  - Game now saves your progress if you ESC out mid-puzzle.  Automatically
    loads that puzzle when you next run it.  Saved to 'savegame.cons' in
    the working folder.

