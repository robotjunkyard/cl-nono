CL-NONO

A little nonogram puzzle game done in Common Lisp using SDL.
Comes with a small bundle of puzzles.

I. REQUIREMENTS
   Steel Bank Common Lisp
      http://www.sbcl.org
   Quicklisp
      http://www.quicklisp.org
   SDL + SDL_image 1.2 Runtime
      https://www.libsdl.org/


II. GETTING STARTED

Follow the instructions on Quicklisp's website to install Quicklisp,
which will automagically install a couple of other Lisp packages that
are also needed (namely, LISPBUILDER-SDL and LISPBUILDER-SDL-IMAGE).

It is highly recommended to also run  (ql:add-to-init-file)  once
Quicklisp is installed so that .sbclrc, in your home folder, instructs
SBCL to load Quicklisp automatically.

After that, Quicklisp should be installed.  Where it installed itself
(typically under the 'quicklisp' folder in your user home directory) there
will be a 'local-projects' folder under that.

You may either move the cl-nono source tree into this 'local-projects' folder
directly or, as I recommend, make a symbolic folder link in 'local-projects'
that points to cl-nono sources' actual location.

Once a 'cl-nono' folder/symlink exists in 'local-projects', you should be
able to load it into SBCL using Quicklisp's 'quickload':

   (ql:quickload :CL-NONO)

Once the CL-NONO files have been loaded by Quicklisp, type:
   (in-package :CL-NONO)

and then
   (main)

to play/tinker/etc.


III. ADDITIONAL NOTES

I have recently run into an issue with Quicklisp (or perhaps ASDF, the
module which actually loads and processes the CL-NONO.ASD file, could
be the actual culprit) in which the .lisp files do not get recompiled, rather
the cached .FASL files (which are compiled machine code of those .LISP
files -- think of .O or .OBJ files when you compile a .C file) from the
very first time :CL-NONO was Quickloaded, get loaded instead.

See if that bug happens before you start making changes.  Workaround:
make a small "loadall.lisp" file which loads the files listed in CL-NONO.ASD
in the same order and just (load "loadall.lisp") instead.  Loading the .lisp
files will work just as well as a workaround, minus compiler optimizations
exclusive to actual compiling.  A game like this isn't exactly demanding
on resources, so that's fine :)

A standalone binary can be generated with:
   (save-game-and-die)

SBCL generates gigantic binaries.  This may work in other implementations
of Common Lisp, some which produce significantly smaller executables,
but I have not personally tinkered with any of them yet.  It probably
wouldn't require too much effort to get CL-NONO working on different ones,
depending on the implementation being ported to.

Have fun!

:njb@robotjunkyard.org

