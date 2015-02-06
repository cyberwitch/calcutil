CalcUtil v1.08
(C) 2007 Daniel Weisz.

	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
---------------------------------------------------------------------------------------------------

Author: Daniel Weisz aka magicdanw aka pcGuru()
	Routines for assembly shells written by Brandon Wilson in Noshell v1.2 (C) 2007
	Token replacement routines based on Omnicalc v1.1 routines by Michael Vincent (C) 2002-2003

This application adds a number of features to the TI-83+ series of graphing calculators.  If you have comments, bugs, criticisms, or suggestions, please email me at magicdanw@yahoo.com with "CalcUtil" in the subject.  I regularly visit the forums at unitedti.org, so a post in the ASM development section will likely get my attention as well.

To install, load the application (util.8xk) onto your calculator with a linking program such as TI Connect.  Run the application from the APPS menu and press [1] to choose Install.  If previous hooks are found (such as Omnicalc's hooks), you will be notified, and pressing ENTER will attempt to chain the hooks.  Once installed, you can use the following features:

1)Run archived programs.
To do this, simply run any archived BASIC or ASM program from the homescreen or in a program as if it were unarchived.

2)Leave out the Asm( token.
ASM programs (archived or unarchived) can be run like BASIC programs without the Asm( token.  Conversely, BASIC programs can be run with the Asm( token, though there is no reason to do this.

3)Archive and unarchive programs within a BASIC program.
CalcUtil overrides the Archive and Unarchive tokens to allow archiving and unarchiving any programs within a BASIC program (even the program itself, although if an unarchived program archives itself, execution will stop).

4)Have a program run at startup.
To run any program when the calculator turns on, create a BASIC program called PROGLIST.  On an empty line, write a capital S (for Startup), a colon, and then the program name.

5)Create shortcut keys to programs.
To assign a program to a shortcut key ([ON] + any number key [1] through [9]), create a BASIC program called PROGLIST.  On an empty line, write the key number, a colon, and then the program name.

Example:
----------------
PROGRAM:PROGLIST
:5:prgmPHYSICS
:S:prgmHELLO
:2:prgmMINESWEP
----------------

With this configuration, prgmHELLO would run on startup, prgmMINESWEP would run when [ON]+[2] is pressed at the same time, and prgmPHYSICS would be run when [ON]+[5] is pressed at the same time.

6)Edit archived programs.
To edit an archived and unlocked BASIC program, open it in the editor as with any unarchived program.  Ignore the odd title displayed.  Edit it as normal.  When you quit by pressing QUIT, OFF, or any key that changes app context, you will be asked to press one of the top keys to save or discard changes made to the program.

7)Calculate logs in bases other than 10.
CalcUtil overrides the log( function so that by adding a base as a second parameter, you can determine the log of a number in a base other than 10.

Version History
---------------

v1.0: Original release
v1.01: Bugfix: X key no longer causes startup program to run
v1.02: Bugfix: Programs run via startup or shortcut keys run at full speed
v1.03: Archive editor title changed from A234567@ to CalcUtil
       Cancel option added to archive editor
       MirageOS hotkey disabled to prevent RAM clears
       Startup bug on uninstallation fixed
       Stop is no longer replaced with Return (known issue: if a Stop is encountered in an unarchived program called from an archived program, a Syntax error will be thrown.  Simply press Quit to proceed as normal.
v1.04: Shortcut keys are disabled anywhere other than the homescreen, until CalcUtil can properly quit to the homescreen on it's own.
       2nd+Off now works in programs (such as at a Pause, Menu(, or Input command).
       Archived programs can once again be run via shortcut key or startup
       Archive editor title changed from CalcUtil to the "script" i, so a normal program named CalcUtil won't be overwritten.
v1.05: Fixed chaining Quick Apps Menu (Omnicalc)
v1.06: Shortcut keys work everywhere in OS, Stops don't throw errors
v1.07: Various bugfixes.  Real program name is shown in the archive save screen.  Programs can run archived subprograms.  Ion and MirageOS programs can be run from the homescreen without any shells or Noshell installed.