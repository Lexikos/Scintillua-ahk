# Scintillua for SciTE4AutoHotkey

This project contains:

  - AutoHotkey lexers for use with Scintillua.
  - Additional files to facilitate installation in SciTE4AutoHotkey.
  - Customisations to lexer.lua to improve stability with large .ahk files.

About Scintillua:

> Scintillua adds dynamic Lua LPeg lexers to Scintilla. It is the quickest way to add new or customized syntax highlighting and code folding for programming languages to any Scintilla-based text editor or IDE. Scintillua was designed to be dropped into or compiled with any Scintilla environment.


## Installation

Download [Scintillua](https://foicica.com/scintillua/) and install as below.

  - Locate your SciTE home directory; the directory which contains SciTEUser.properties.
  - Unpack Scintillua and move the `lexers` directory into your home directory.

Copy the files from this repository (excluding this readme) into the `lexers` directory, overwriting lexer.lua.

  - Note: You may keep the original lexer.lua, but it will be more likely to crash if you open a very large .ahk file.

Add either the contents of `lexers/lpeg_s4a.properties` or the following line to your `SciTEUser.properties` file.

    import lexers/lpeg_s4a

Add either the contents of `lexers/lpeg_s4a.lua` or the following line to your `UserLuaScript.lua` file.

    dofile(props['SciteUserHome']..'/lexers/lpeg_s4a.lua')

Restart SciTE4AutoHotkey.
