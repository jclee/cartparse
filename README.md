cartparse
=========

This is a utility to automatically reformat and refactor the source code of
Cart Life, an independent, open-source game written in the scripting language
of Adventure Game Studio.  It may or may not be useful for other purposes.

It also might serve as an example of how to write a comment-preserving parser
using Haskell's Parsec parsing library -- although maybe not a very good
example, given that it is my first Haskell program.

If you're going to try to build this, you're going to need an installation of
the Haskell Platform with GHC 7.6.3 or better.

How to use
----------

* Set the `CARTLIFE` environment variable to the Cart Life source directory
* In a shell with `ghc` in the path, run `parse.bat` or `parse.sh`.
* Caveat: I'm still working out exactly what this will do.

Reference
---------

* [Adventure Game Studio](http://www.adventuregamestudio.co.uk/) (has scripting language docs)
* [AGS source](https://github.com/adventuregamestudio/ags)
* [Official Cart Life Page](http://richardhofmeier.com/cartlife) (currently disabled)
* [Cart Life Archive](http://web.archive.org/web/20140407090725/http://www.richardhofmeier.com/cartlife/) (currently has game and source download links, courtesy of the Internet Archive)
