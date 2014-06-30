Coedit
======

Coedit is a simple IDE for the [D2](http://dlang.org) lang. (**Co** mpile & **Edit**).

Current features
----------------
- multi platform (Win/Linux).
- projects.
- multiple project configurations (set of switches and options).
- compile, run directly from the UI.
- instant run (without saving, script-like).
- synchronized edition in a block.
- D syntax highlighter, folding, identifier markup.
- current module member list.

Planed in version 1
-------------------
- project configurations templates (release, debug, etc.).
- basic auto completion (brackets, key-words, ...).
- console input handling.
- static library explorer (using JSON infos).

Project information
-------------------
- state: alpha 2.
- programmed in Object pascal with [Lazarus](http://www.lazarus.freepascal.org).
- based on *dmd* (*gdc* or *lmd* characteristics are not handled).
- no other third party dependencies (so far...but using *dscanner* and/or *dcd* is envisaged.)

Setup & test
------------
Coedit must be build from the sources:
- clone this repository (even if not mandatory, preferably from the latest tag, as tagged versions are more tested then the others.)
- both [dmd](http://dlang.org/download.html) and [Lazarus](http://www.lazarus.freepascal.org) must be setup.
- open "coedit.lpr" in *Lazarus*, set the build mode to *Release*
- press the Run button (or build)
- run coedit and project, open *"lazproj\test\coeditproj\test.coedit"* from the project menu to give a brief try.

Preview
-------
Windows version:
![Win screen-cap](lazproj/Gui.tease.png "Coedit GUI preview")
Linux version:
![Nux screen-cap](lazproj/Gui.tease.kde.png "Coedit GUI preview")