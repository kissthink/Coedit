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
- member list of the current module.
- manager for the static libraries.
- search and replace.
- [D Completion Daemon](https://github.com/Hackerpilot/DCD) integration for completion proposal and source code hints.
- mini file browser.

Missing features before the first beta
--------------------------------------
- Options editor. (the big missing stuff)
- console input handling. (workarounds exists)
- project configurations templates (release, debug, etc.). (detail)

Project information
-------------------
- state: alpha 4.
- license: MIT.
- programmed in Pascal with [Lazarus](http://www.lazarus.freepascal.org) as IDE.
- based on *dmd* (*gdc* or *lmd* switches are not handled).

Setup & test
------------
Coedit must be build from its sources.
The complete procedure is described in the first section of the [wiki](https://github.com/BBasile/Coedit/wiki)

Preview
-------
Windows version (Windows 7, x86):
![Win screen-cap](lazproj/Gui.tease.png "Coedit GUI preview")

Linux version (OpenSuse 13.1, x86_64):
![Nux screen-cap](lazproj/Gui.tease.kde.png "Coedit GUI preview")