CL-NONO
version 0.99
by njb@robotjunkyard.org

An adaptation of a nonogram-type puzzle game.
These games are also alternatively known around
the market as "Picross", "Paint By Numbers", etc.

System Requirements:
   Linux or Windows  (maybe it'll work on Mac, too?)
   Steel Bank Common Lisp
      http://www.sbcl.org
   Quicklisp
      http://www.quicklisp.org
   SDL & SDL_image 1.2 Runtime Binaries
      https://www.libsdl.org/

Controls:
  Right Mouse: Mark square as "Do Not Touch", denoted by a Red X
  Left Mouse:  Mark square as a Pixel, or
       	       Unmarks "Do Not Touch" square as blank non-Pixel square.
  F11:         Toggle Fullscreen/Window
  Escape:      Exit Game

Notes:
Made Aug 5-8, 2016, using Steel Bank Common Lisp.

Due to ridiculous file sizes of executables generated
by SBCL and how tiny this game is, I've decided for
this to be a source-only release.  For those who are
unfamiliar with Common Lisp, I have included instructions
how to load and run CL-NONO.

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


ACTUALLY INSTALLING/RUNNING IT
------------------------------

1) Unzip cl-nono's zip to a location of your choice
2) Install Steel Bank Common Lisp if not already.
3) Install latest Quicklisp from http://www.quicklisp.org, according to
   the instructions on that site.
   * Recommended to run (ql:add-to-init-file) afterwards so Quicklisp
     gets loaded automatically by SBCL whenever it starts up.

Then, open your shell and chdir to where you extracted cl-nono and
run SBCL.  In the REPL (prompt), type:

   (load "loadme.lisp")

After that has completed, you may play by typing:
   (cl-nono:main)

To exit back to shell, type:
   (sb-ext:quit)

TROUBLESHOOTING
---------------
Issue:    You get an error about missing libraries (DLLs or .so.* files) while
          loading LOADME.LISP.

Solutions:
   1) If you are in Linux, install these two packages (named as such in
      Ubuntu/Mint;  exact package names may differ depending on your
      distribution):

      libsdl1.2debian
      libsdl-image1.2

   2) If you are in Windows, navigate to http://www.libsdl.org and
      click on the "SDL 1.2" link under the Download category.  From
      there, download:

           SDL-1.2.15-win32.zip      (if using 32-bit Windows & SBCL)
        or SDL-1.2.15-win32-x64.zip  (if using 64-bit Windows & SBCL)

      from the Runtime Libraries section of that download page.

      In addition, download SDL_image runtime libraries on that site from
      this URL:
         https://www.libsdl.org/projects/SDL_image/release-1.2.html

           SDL_image_1.2.12-win32.zip      (if using 32-bit)
        or SDL_image_1.2.12-win32-x64.zip  (if using 64-bit)

      Unzip the DLLs in both files you downloaded into the CL-NONO directory.

Have fun!

:njb@robotjunkyard.org


