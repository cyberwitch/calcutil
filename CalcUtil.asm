; CalcUtil v2.03
; (C) 2007 Daniel Weisz.
;
;	This program is free software; you can redistribute it and/or modify
;	it under the terms of the GNU General Public License as published by
;	the Free Software Foundation; either version 2 of the License, or
;	(at your option) any later version.
;
;	This program is distributed in the hope that it will be useful,
;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;	GNU General Public License for more details.
;
;	You should have received a copy of the GNU General Public License
;	along with this program; if not, write to the Free Software
;	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 include "settings.inc"
 NOLIST
; include "ti83plus.inc"
 include "oldti83plus.inc"
 LIST
 include "equates.inc"
 include "extraequ.inc"
 include "header.asm"

 SEGMENT MAIN
 GLOBALS ON

 EXTERN IPutS,getAppNameLen,IGetKey,parserHook,VPutSAppCenter,getDataByte,parserHookChained
 EXTERN tempHookBlock,runProgram

 Var wProgSize,2
 Var wByteCount,2
 Var tempByte,1
 Var wDataPtr,2
 Var bDataPage,1
 Var wUserMemPtr,2
 Var wProgOffset,2
 Var wTempWord,2
 Var SPbackup,2
 Var flagByte,1
 Var oldHookBlock,4

wProgBytes           equ cmdShadow+60
bNameBackup          equ cmdShadow+62
mirTempByte          equ cmdShadow+70
libraryVectors       equ cmdShadow+80
randData             equ cmdShadow+104


_ParsePrgmName equ 4E82h

_GetKeyRetOff equ 500Bh

_DispDone equ 45B5h

AppBackUpScreen EQU 9872h

parserHookPtr equ 9BACh
_EnableParserHook   equ     5026h
_DisableParserHook  equ     5029h
parserHookFlag       equ     36h
parserBit       equ     1

appChangeHookPtr equ 9BB0h
_EnableAppChangeHook equ 502Ch
_DisableAppChangeHook equ 502Fh
appChangeHookFlag equ 36h
appChangeBit equ 2

menuHookPtr equ 9BC0h
_EnableMenuHook equ 5083h
_DisableMenuHook equ 5086h
menuHookFlag equ 36h
menuBit equ 6


rawKeyHookPtr equ 9B84h
_EnableRawKeyHook equ 4F66h
_DisableRawKeyHook equ 4F6Fh
rawKeyHookFlag equ 34h
rawKeyBit equ 5


homescreenHookPtr EQU 9B8Ch
_EnableHomescreenHook EQU 4FABh
_DisableHomescreenHook EQU 4FAEh
homescreenHookActive EQU 4
homescreenHookFlag EQU 34h

getKeyHookPtr equ 9B88h
_EnableGetKeyHook equ 4F7Bh
_DisableGetKeyHook equ 4F7Eh
getKeyHookFlag equ 34h
getKeyBit equ 0

parseVar equ 9652h

cmdexec equ 6
cmdFlags equ 12
newDispF equ 8
progExecuting equ 1

_ExecutePrgm equ 4E7Ch

_JError equ 44D7h
_ErrCustom1 equ 4D41h
_DispErrorScreen equ 49DEh

basic_pc equ 965Dh

basic_prog equ 9652h

_MonForceKey equ 4021h


_PowerOff equ 5008h


_BufClear		equ 4936h
_BufDelete		equ 4912h
_BufInsert		equ 4909h

rMov9ToOP1 equ rMOV9TOOP1


StartApp:
	b_call ClrLCDFull
	ld hl, 0
	ld (curRow), hl
	ld hl, Util
	set textInverse, (IY + textFlags)
	call PutSApp
	res textInverse, (IY + textFlags)
	b_call NewLine
	ld hl, One
	call PutSApp
	;res textInverse, (IY + textFlags)
	ld hl, Install
	call PutSApp
	b_call NewLine
	ld hl, Two
	call PutSApp
	ld hl, Uninstall
	call PutSApp
	b_call NewLine
	ld hl, Three
	call PutSApp
	ld hl, Settings
	call PutSApp
	b_call NewLine
	ld hl, Four
	call PutSApp
	ld hl, About
	call PutSApp
	b_call NewLine
	ld hl, Five
	call PutSApp
	ld hl, QuitText
	call PutSApp
	ld b, 1
MenuLoop:
	res indicOnly, (IY + indicFlags)
	b_call GetKeyRetOff
	res onInterrupt, (IY + onFlags)
	cp kQuit
	jr z, Quit
	cp kClear
	jr z, Quit
	;cp kDown
	;jr z, Down
	;cp kUp
	;jr z, Up
	;cp kEnter
	;jr z, Enter
	cp k1
	jp z, InstallHooks
	cp k2
	jr z, UninstallHooks
	cp k3
	jr z, SettingsMenu
	cp k4
	jr z, AboutScreen
	cp k5
	jr z, Quit
	jr MenuLoop
Quit:
	b_call ClrScrnFull
	ld hl, 0
	ld (curRow), hl
	res 7, (IY + 28h)
	B_JUMP JForceCmdNoChar
Down:
	;xor a
	;ld (curCol), a
	;ld a, b
	;ld (curRow), a
	;add a, 30h
	;b_call PutC
	;ld a, ':'
	;b_call PutC
	;inc b
	;ld a, 5
	;cp b
	;jr nc, UpdateMenu
	;ld b, 1
	;jr UpdateMenu
Up:
	;xor a
	;ld (curCol), a
	;ld a, b
	;ld (curRow), a
	;add a, 30h
	;b_call PutC
	;ld a, ':'
	;b_call PutC
	;dec b
	;ld a, 1
	;cp b
	;jr c, UpdateMenu
	;jr z, UpdateMenu
	;ld b, 5
	;jr UpdateMenu
UpdateMenu:
	;xor a
	;ld (curCol), a
	;ld a, b
	;ld (curRow), a
	;add a, 30h
	;set textInverse, (IY + textFlags)
	;b_call PutC
	;ld a, ':'
	;b_call PutC
	;res textInverse, (IY + textFlags)
	;jp MenuLoop
Enter:
	;ld a, b
	;cp 1
	;jp z, InstallHooks
	;cp 2
	;jr z, UninstallHooks
	;cp 3
	;jr z, SettingsMenu
	;cp 4
	;jr z, AboutScreen
	;cp 5
	;jr z, Quit
	;jr MenuLoop

InstallHooks:
   bit parserBit,(iy + parserHookFlag)
   jr nz, ShowWarning
	bit appChangeBit, (iy + appChangeHookFlag)
	jr nz, ShowWarning
	bit rawKeyBit, (iy + rawKeyHookFlag)
	jr nz, ShowWarning
	;bit getKeyBit, (iy + getKeyHookFlag)
	;jr nz, ShowWarning
	;bit menuBit, (iy + menuHookFlag)
	;jr nz, ShowWarning

InstallHooks2:

	ld hl, Offscrpt									;Set up the offscript variable
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, CreateOff
	b_call DelVarArc
CreateOff:
	ld hl, AppVarEnd - AppVarStart + 1
	b_call CreateAppVar
	inc de
	inc de
	ld hl, AppVarStart
	ld bc, AppVarEnd - AppVarStart + 1
	ldir

	set 1, (IY + 33h)





	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr nc, FoundIt
	call NewAppVar										;Delete and make a new UtilVar
	jr SizeTwoOk
FoundIt:
	xor a
	cp b
	jr z, ItsUnarchived
	b_call Arc_Unarc
	b_call ChkFindSym
ItsUnarchived:
	ld a, (de)
	cp 28h
	jr z, SizeOneOk
	call NewAppVar
	jr SizeTwoOk
SizeOneOk:
	inc de
	ld a, (de)
	cp 0
	jr z, SizeTwoOk
	call NewAppVar
SizeTwoOk:







	bit parserBit, (IY + parserHookFlag)		;Try to save a current parser hook
	jr z, NoParserHook
	ld hl, parserHookPtr + 2
	ld a, (hl)
	ld b, a
	in a, (6)
	cp b
	jr z, InstallParser



	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, parserHookPtr
	ld bc, 3
	inc de
	inc de
	ldir
	jr InstallParser

NoParserHook:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ex de, hl
	inc hl
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 0
InstallParser:
	ld hl, ParserHook
	in a, (6)
	b_call EnableParserHook
	xor a
	ld (parserHookPtr + 3), a








	bit rawKeyBit, (IY + rawKeyHookFlag)		;Try to save the current raw key hook
	jr z, NoRawKeyHook
	ld hl, rawKeyHookPtr + 2
	ld a, (hl)
	ld b, a
	in a, (6)
	cp b
	jr z, InstallRawKey



	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld bc, 3
	ld hl, 5
	add hl, de
	ex de, hl
	ld hl, rawKeyHookPtr
	ldir
	jr InstallRawKey

NoRawKeyHook:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ex de, hl
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 0
InstallRawKey:
	ld hl, RawKeyHook
	in a, (6)
	b_call EnableRawKeyHook
	xor a
	ld (rawKeyHookPtr + 3), a






	bit appChangeBit, (IY + appChangeHookFlag)		;Try to save the current app change hook
	jr z, NoAppChangeHook
	ld hl, appChangeHookPtr + 2
	ld a, (hl)
	ld b, a
	in a, (6)
	cp b
	jr z, InstallAppChange


	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld bc, 3
	ld hl, 31
	add hl, de
	ex de, hl
	ld hl, appChangeHookPtr
	ldir
	jr InstallAppChange

NoAppChangeHook:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 31
	add hl, de
	ld (hl), 0
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 0
InstallAppChange:
	ld hl, AppChangeHook
	in a, (6)
	b_call EnableAppChangeHook
	xor a
	ld (appChangeHookPtr + 3), a

leave:

	;b_call DisableGetKeyHook

	;ld hl, GetKeyHook
	;in a, (6)
	;b_call EnableGetKeyHook
	;xor a
	;ld (getKeyHookPtr + 3), a


	b_call ClrScrnFull
	ld hl, 0
	ld (curRow), hl
	ld hl, Installed
	call PutSApp
	ld hl, 6h
	ld (curRow), hl
	ld hl, AnyKey
	call PutSApp
	ld hl, 7h
	ld (curRow), hl
	ld hl, AnyKey2
	call PutSApp
	b_call GetKey
	jr StartApp													;Install finished!



UninstallHooks:											;Uninstall all those hooks
	b_call DisableParserHook
	b_call DisableAppChangeHook
	b_call DisableRawKeyHook
	;b_call DisableGetKeyHook
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, NoVar2
	xor a
	cp b
	jr z, NoArcVar
	b_call Arc_Unarc
	b_call ChkFindSym
NoArcVar:
	ex de, hl
	inc hl
	inc hl
	inc hl
	inc hl
	ld a, (hl)
	cp 0
	jr z, NoParser
	dec hl
	ld d, (hl)
	dec hl
	ld e, (hl)
	ex de, hl
	b_call EnableParserHook
	ex de, hl
	inc hl
	inc hl
NoParser:
	inc hl
	inc hl
	inc hl
	ld a, (hl)
	cp 0
	jr z, NoRawKey
	dec hl
	ld d, (hl)
	dec hl
	ld e, (hl)
	ex de, hl
	b_call EnableRawKeyHook
	ex de, hl
	inc hl
	inc hl
NoRawKey:
	ld de, 26
	add hl, de
	ld a, (hl)
	cp 0
	jr z, NoAppChange
	dec hl
	ld d, (hl)
	dec hl
	ld e, (hl)
	ex de, hl
	b_call EnableAppChangeHook
NoAppChange:
NoVar2:
	ld hl, Offscrpt
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, NoOffscrpt
	b_call DelVarArc										;Remove our offscript appvar b/c it can go wacky if it's run w/o our hooks to handle it
NoOffscrpt:
	b_call ClrScrnFull
	ld hl, 0
	ld (curRow), hl
	ld hl, Uninstalled
	call PutSApp
	ld hl, 6h
	ld (curRow), hl
	ld hl, AnyKey
	call PutSApp
	ld hl, 7h
	ld (curRow), hl
	ld hl, AnyKey2
	call PutSApp
	b_call GetKey
	jr StartApp




ConfigureNoshell:
SettingsMenu:
	ld a, 1
	ld (AppBackUpScreen), a
	b_call ClrLCDFull
	ld hl, 0
	ld (curRow), hl
	ld hl, Settings
	set textInverse, (IY + textFlags)
	call PutSApp
	res textInverse, (IY + textFlags)
	ld hl, 1
	ld (curRow), hl
	ld a, 5
	b_call PutC
	ld a, 2
	ld (curCol), a
	ld hl, Writeback
	call PutSApp
	ld hl, 0202h
	ld (curRow), hl
	ld hl, SaveScr
	call PutSApp
	ld hl, 0203h
	ld (curRow), hl
	ld hl, Token
	call PutSApp
;	set textInverse, (IY + textFlags)
	call CheckSettings
	call CheckSettings
	push af
	xor a
	cp e
	jr z, WritebackOff
;	res textInverse, (IY + textFlags)

	ld hl, 0101h
	ld (curRow), hl
	ld a, '*'
	b_call PutC
WritebackOff:
;	set textInverse, (IY + textFlags)
	xor a
	cp d
	jr z, SaveOff
;	res textInverse, (IY + textFlags)

	ld hl, 0102h
	ld (curRow), hl
	ld a, '*'
	b_call PutC
SaveOff:
	pop af
;	set textInverse, (IY + textFlags)
	or a
	jr z, TokenOff
;	res textInverse, (IY + textFlags)

	ld hl, 0103h
	ld (curRow), hl
	ld a, '*'
	b_call PutC
TokenOff:
MenuLoop2:
;	res textInverse, (IY + textFlags)
	res indicOnly, (IY + indicFlags)
	b_call GetKey
	res onInterrupt, (IY + onFlags)
	cp kQuit
	jr z, Quit
	cp kClear
	jr z, StartApp
	cp kEnter
	jr z, Toggle
	cp kUp
	jr z, GoUp
	cp kDown
	jr z, GoDown
	jr MenuLoop2

GoUp:
	xor a
	ld (curCol), a
	ld a, (AppBackUpScreen)
	ld (curRow), a
	ld a, ' '
	b_call PutC
	ld a, (curRow)
	dec a
	cp 0
	jr nz, NoLoopUp
	ld a, 3
NoLoopUp:
	ld (AppBackUpScreen), a
	ld (curRow), a
	xor a
	ld (curCol), a
	ld a, 5
	b_call PutC
	jr MenuLoop2

GoDown:
	xor a
	ld (curCol), a
	ld a, (AppBackUpScreen)
	ld (curRow), a
	ld a, ' '
	b_call PutC
	ld a, (curRow)
	inc a
	cp 4
	jr nz, NoLoopDown
	ld a, 1
NoLoopDown:
	ld (AppBackUpScreen), a
	ld (curRow), a
	xor a
	ld (curCol), a
	ld a, 5
	b_call PutC
	jr MenuLoop2




Toggle:
;	set textInverse, (IY + textFlags)
	ld a, (AppBackUpScreen)
	cp 1
	jr z, ToggleWriteback
	cp 2
	jr z, ToggleSave
ToggleToken:
	call CheckSettings
	cp 0
	jr nz, SetToken
	ld de, 24
	add hl, de
	ld (hl), 1
	ld hl, 0103h
	ld (curRow), hl
;	ld a, '_'
	ld a, '*'
	b_call PutC
	jr MenuLoop2
SetToken:
	ld de, 24
	add hl, de
	ld (hl), 0
	ld hl, 0103h
	ld (curRow), hl
;	res textInverse, (IY + textFlags)
;	ld a, '_'
	ld a, ' '
	b_call PutC
	jr MenuLoop2

ToggleWriteback:
	call CheckSettings
	dec hl
	ld a, e
	cp 0
	jr nz, SetWriteback
	ld (hl), 80h
	ld hl, 0101h
	ld (curRow), hl
;	ld a, '_'
	ld a, '*'
	b_call PutC
	jr MenuLoop2
SetWriteback:
	ld (hl), 0
	ld hl, 0101h
	ld (curRow), hl
;	res textInverse, (IY + textFlags)
;	ld a, '_'
	ld a, ' '
	b_call PutC
	jr MenuLoop2

ToggleSave:
	call CheckSettings
	ld a, d
	cp 0
	jr nz, SetSave
	ld (hl), 1
	ld hl, 0102h
	ld (curRow), hl
;	ld a, '_'
	ld a, '*'
	b_call PutC
	jr MenuLoop2
SetSave:
	ld (hl), 0
	ld hl, 0102h
	ld (curRow), hl
;	res textInverse, (IY + textFlags)
;	ld a, '_'
	ld a, ' '
	b_call PutC
	jr MenuLoop2

CheckSettings:						;E will hold writeback flag (80/0) and D will hold save flag (1/0) and A will hold token flag (1/0)
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, SettingsUnarc
	b_call Arc_Unarc
	b_call ChkFindSym
SettingsUnarc:
	ld hl, 9
	add hl, de
	ld e, (hl)
	inc hl
	ld d, (hl)
	push hl
	push de
	ld de, 24
	add hl, de
	pop de
	ld a, (hl)
	pop hl
	ret



AboutScreen:
	b_call ClrLCDFull
	ld hl, 0
	ld (penCol), hl
	ld hl, AboutText1
	call VPutSApp
	xor a
	ld (penCol), a
	ld a, 10
	ld (penRow), a
	ld hl, AboutText2
	call VPutSApp
	xor a
	ld (penCol), a
	ld a, 17
	ld (penRow), a
	ld hl, AboutText3
	call VPutSApp
	xor a
	ld (penCol), a
	ld a, 24
	ld (penRow), a
	ld hl, AboutText4
	call VPutSApp
	xor a
	ld (penCol), a
	ld a, 31
	ld (penRow), a
	ld hl, AboutText5
	call VPutSApp
	xor a
	ld (penCol), a
	ld a, 38
	ld (penRow), a
	ld hl, AboutText6
	call VPutSApp
	xor a
	ld (penCol), a
	ld a, 45
	ld (penRow), a
	ld hl, AboutText7
	call VPutSApp
	xor a
	ld (penCol), a
	ld a, 52
	ld (penRow), a
	ld hl, AboutText8
	call VPutSApp
	b_call GetKey
	jp StartApp


NewAppVar:
;	ld hl, 0
;	ld (AppBackUpScreen+10), hl
;	xor a
;	ld (AppBackUpScreen+12), a
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, Create									;If appvar exists, try to retain backup flag; otherwise, go to Create
;	xor a
;	cp b
;	jr z, NotArccc
;	b_call Arc_Unarc
;	b_call ChkFindSym
;NotArccc:
;	push hl
;	push de
;	ld hl, 9
;	add hl, de
;	ld a, (hl)
;	ld (AppBackUpScreen+10), a
;	inc hl
;	ld a, (hl)
;	ld (AppBackUpScreen+11), a
;	ld de, 24
;	add hl, de
;	ld a, (hl)
;	ld (AppBackUpScreen+12), a
;	pop de
;	pop hl
	b_call DelVarArc
	ld hl, AppVarName
	rst rMov9ToOP1
Create:
	ld hl, 40
	b_call CreateAppVar
;	push de	
	inc de
	inc de
	xor a
	ld b, 40
clearloop:
	ld (de), a
	djnz clearloop
;	ld a, (AppBackUpScreen+10)
;	pop de
;	ld hl, 9
;	add hl, de
;	ld (hl), a
;	ld a, (AppBackUpScreen+11)
;	inc hl
;	ld (hl), a
;	ld de, 24
;	add hl, de
;	ld a, (AppBackUpScreen+12)
;	ld (hl), a
	ret

AboutText1:
	db "About:", 0
AboutText2:
	db "This program was created", 0
AboutText3:
	db "by Daniel Weisz to add", 0
AboutText4:
	db "features to the TI-83+", 0
AboutText5:
	db "including additional", 0
AboutText6:
	db "archive abilities. Shell", 0
AboutText7:
	db "routines by BrandonW. See", 0
AboutText8:
	db "readme for details.", 0


AppVarName:
	db AppVarObj, "UtilVar", 0




RestoreHooks:
	push af
	push hl
	in a, (6)
	ld hl, AppChangeHook
	b_call EnableAppChangeHook
	in a, (6)
	ld hl, RawKeyHook
	b_call EnableRawKeyHook
	in a, (6)
	ld hl, ParserHook
	b_call EnableParserHook
	pop hl
	pop af
	ret




ShowWarning:	
	ld hl, 0
	ld (curRow), hl
	ld hl, WarningText
	call PutSApp
	b_call GetKey
	cp kAdd
	jp z, InstallHooks2
	cp kEnter
	jr nz, StartApp
	b_call DisableParserHook
	b_call DisableRawKeyHook
	b_call DisableAppChangeHook
	jp InstallHooks2


WarningText:
	db "  Hooks exist!  "
	db "Press ", LlBrack, "Enter] to"
	db " overwrite, ", LlBrack, "+] "
	db "   attempt to   "
	db " chain hooks, or"
	db "any other key to"
	db "     cancel.", 0

Installed:
	db "Installed!", 0
Uninstalled:
	db "Uninstalled!", 0
AnyKey:
	db "Press any key", 0
AnyKey2:
	db "to continue...", 0

Util:
	db "CalcUtil v2.03", 0
One:
	db "1:", 0
Install:
	db "Install", 0
Two:
	db "2:", 0
Uninstall:
	db "Uninstall", 0
Three:
	db "3:", 0
Settings:
	db "Settings", 0
Four:
	db "4:", 0
About:
	db "About", 0
Five:
	db "5:", 0
QuitText:
	db "Quit", 0


Writeback:
	db "Writeback", 0
SaveScr:
	db "Save Screen", 0
Token:
	db "Legacy Tokens", 0





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





AppChangeHook:
	db 83h
	;set 7,(iy+28h)
	cp 2
	jr z, TurnedOff
	push af
	push bc
	push de
	push hl

	call RestoreHooks

	cp kMode
	jr z, ChainAppChangeHook
	cp kFormat
	jr z, ChainAppChangeHook
	cp kTblSet
	jr z, ChainAppChangeHook





	ld (AppBackUpScreen), a
	ld a, b
	ld (AppBackUpScreen+1), a
;	b_call DisableGetKeyHook
;	b_call GetKey
;loop22:
;	b_call GetCSC
;	cp 0
;	jr z, loop22
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NotArc4
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc4:
	ld a, (AppBackUpScreen)
	cp 0
	jr nz, NotError
	ld a, (AppBackUpScreen+1)
	cp kError
	jr z, HandleGoto
	ld a, (AppBackUpScreen)
NotError:
	cp kPrgmEd
	jr nz, CheckHome								;If we're entering the editor, continue; otherwise, go to CheckHome



	set 7,(iy+28h)



	ld a, progobj
	ld (OP1), a
	ld hl, progToEdit
	ld de, OP1+1
	ld bc, 8
	ldir
UnarchivedGoto:
	b_call ChkFindSym
	jr c, ChainAppChangeHook						;If the program to edit doesn't exist, quit
	xor a
	cp b
	jr z, EditUnarchived				;If the program to edit is archived, continue; otherwise, go to EditUnarchived
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 8								;Set the archived flag
	add hl, de
	ld (hl), 1
	inc hl
	ld a, (hl)
	ld (AppBackUpScreen), a
	ld de, 4
	add hl, de
	ex de, hl
	ld hl, progToEdit					;Store original program name to appvar
	ld bc, 8
	ldir
	ld a, ProgObj
	ld (OP1), a
	ld hl, progToEdit					;Copy archived program to a temp ram copy
	ld de, OP1+1
	ld bc, 8
	ldir
	call CopyPrgmToRam
	ld hl, OP1+1
	ld de, progToEdit
	ld bc, 8
	ldir									;Set it to edit the ram copy instead of the original
	b_call PushRealO1
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 21
	add hl, de
	ex hl, de
	push de
	b_call PopRealO1
	pop de
	ld hl, OP1+1
	ld bc, 8
	ldir
	jr ReturnZPopAllEditor
;	jr LoadEditor


TurnedOff:
	push af
	push bc
	push de
	push hl
	call CleanUp
	pop hl
	pop de
	pop bc
	pop af
	ret

QuitError:
	cp kError
	jr nz, ReturnZPopAll
	ld a, (AppBackUpScreen)
	cp kQuit
	jr nz, ReturnZPopAll
	call CleanUp
	jr ReturnZPopAll
		

HandleGoto:
	set 7, (iy + 28h)
	ld a, ProgObj
	ld (OP1), a
	ld hl, progToEdit
	ld de, OP1+1
	ld bc, 8
	ldir
	ld hl, OP1
	ld de, tempProgName
	ld b, 8
	call CompareStrings
	jr nz, UnarchivedGoto
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 8								;Set the archived flag
	add hl, de
	ld (hl), 1
;	jr ReturnZPopAllEditor
;	jr LoadEditor


ReturnZPopAllEditor:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 29
	add hl, de
	xor a
	cp (hl)
	jr nz, TurnOffFromEditor
ReturnZPopAll:
	pop hl
	pop de
	pop bc
	pop af
	jr ReturnZ



TurnOffFromEditor:
	ld (hl), a
	pop hl
	pop de
	pop bc
	pop af
	b_call PowerOff

CleanReturnZPopAllEditor:
	call CleanUp
	jr ReturnZPopAllEditor


EditUnarchived:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 8								;Reset the archived flag
	add hl, de
	ld (hl), 0
	inc hl
	inc hl
	ld a, (hl)
	ld (AppBackUpScreen), a
	ld de, 3
	add hl, de
	ex de, hl
	ld hl, progToEdit					;Store original program name to appvar
	ld bc, 8
	ldir
	ld a, (AppBackUpScreen)
	cp 0
	jr z, ReturnZPopAll				;If save screen isn't selected, quit now
;	jr z, LoadEditor
SaveScreenUnarc:
	ld a, ProgObj
	ld (OP1), a
	ld de, -8
	add hl, de
	ld de, OP1+1
	ld bc, 8
	ldir
	call CopyPrgmToRam
	ld hl, OP1+1
	ld de, progToEdit
	ld bc, 8
	ldir
	b_call PushRealO1
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 21
	add hl, de
	ex hl, de
	push de
	b_call PopRealO1
	pop de
	ld hl, OP1+1
	ld bc, 8
	ldir
	jr ReturnZPopAll
;	jr LoadEditor




CheckHome:
	cp kPrgmCr
	jr z, CreatingProgram
	res 7,(iy+28h)
	ld a, (AppBackUpScreen+1)
	cp kPrgmEd
	jr nz, QuitError
	ld hl, 8
	add hl, de
	ld a, (hl)
	cp 2
	jr z, ReturnZPopAll
	cp 1
	jr z, QuittingArchived
	inc hl
	inc hl
	xor a
	cp (hl)
	call nz, SaveScreen
	cp 0
	jr z, CleanReturnZPopAllEditor
	cp 2
	jr z, DelTempNoSave
	cp 3
	jr z, ReturnToEditor
	call TempToOriginal
	jr ReturnZPopAllEditor
QuittingArchived:
	inc hl
	inc hl
	xor a
	cp (hl)
	call nz, SaveScreen
	cp 0
	jr z, ArcProgWriteback
	cp 2
	jr z, DelTempNoSave
	cp 3
	jr z, ReturnToEditor
FinishWriteback:
	call TempToOriginal
	b_call OP4ToOP1
	b_call ChkFindSym
	jr c, ReturnZPopAllEditor
	b_call Arc_Unarc
	jr ReturnZPopAllEditor
	


CreatingProgram:
	ld hl, 8
	add hl, de
	ld a, 2
	ld (hl), a
	jr ReturnZPopAll


ArcProgWriteback:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 13
	add hl, de
	ld de, OP1+1
	ld bc, 8
	ldir
	ld de, OP2+1
	ld bc, 8
	ldir
	ld a, ProgObj
	ld (OP1), a
	ld (OP2), a
	call SmartWriteback
	jr z, DelTempNoSave
	jr FinishWriteback

TempToOriginal:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 13
	add hl, de
	ld de, OP1+1
	ld bc, 8
	ldir
	ld de, OP3+1
	ld bc, 8
	ldir
	ld a, ProgObj
	ld (OP1), a
	ld (OP3), a
	b_call PushRealO1
	b_call ChkFindSym
	b_call DelVarArc
	b_call PopRealO1
	b_call PushRealO1
	b_call OP3ToOP1
	b_call ChkFindSym
	push hl
	b_call PopRealO1
	b_call PushRealO1
	pop hl
	ld a, (OP1)
	push af
	call renameprog
	b_call PopRealO1
	ld a, 5
	ld (OP1), a
	b_call ChkFindSym
	pop af
	ld (hl), a
	ret

;TempToOriginal:
;	ld hl, AppVarName
;	rst rMov9ToOP1
;	b_call ChkFindSym
;	ld hl, 13
;	add hl, de
;	ld de, OP1+1
;	ld bc, 8
;	ldir
;	ld de, OP3+1
;	ld bc, 8
;	ldir
;	ld a, ProgObj
;	ld (OP1), a
;	ld (OP3), a
;	b_call OP1ToOP4
;	b_call ChkFindSym
;	b_call DelVarArc
;	b_call OP3ToOP1
;	b_call ChkFindSym
;	ex de, hl
;	ld c, (hl)
;	inc hl
;	ld b, (hl)
;	inc hl
;	push hl
;	push bc
;	push bc
;	b_call OP4ToOP1
;	pop hl
;	b_call CreateProg
;	inc de
;	inc de
;	pop bc
;	pop hl
;	ld a, b
;	or c
;	jr z, ItsEmpty
;	ldir
;ItsEmpty:
;	b_call OP3ToOP1
;	b_call ChkFindSym
;	b_call DelVarArc
;	call CleanUp
;	ret

DelTempNoSave:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 21
	add hl, de
	ld de, OP1+1
	ld bc, 8
	ldir
	ld a, ProgObj
	ld (OP1), a
	b_call ChkFindSym
	b_call DelVarArc
	call CleanUp
	jr ReturnZPopAllEditor

ReturnToEditor:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 21
	add hl, de
	ld de, progToEdit
	ld bc, 8
	ldir
	xor a
	ld (hl), a

;	jr LoadEditor

	ld a, kPrgmEd
	b_call NewContext0
	set 7,(iy+28h)
	b_call Mon

SaveScreen:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 13
	add hl, de
	ld bc, 8
	ld de, OP1+1
	ldir
	b_call ClrLCDFull
	ld hl, 0
	ld (curRow), hl
	ld hl, Save1
	call PutSApp
	call PutSAppPrgmName
	ld hl, 3
	ld (curRow), hl
	ld hl, Save2
	call PutSApp
SaveLoop:
	b_call GetCSC
	cp 0
	jr z, SaveLoop
	cp skGraph
	jr z, ReturnDiscard
	cp skTrace
	jr z, ReturnDiscard
	cp skWindow
	jr z, ReturnSave
	cp skYEqu
	jr z, ReturnSave
	jr ReturnReturn
ReturnSave:
	ld a, 1
	ret
ReturnDiscard:
	ld a, 2
	ret
ReturnReturn:
	ld a, 3
	ret


ChainAppChangeHook:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, ReturnZPopAll
	xor a
	cp b
	jr z, NotArc333
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc333:
	ld hl, 31
	add hl, de
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	ld a, (hl)
	cp 0
	jr z, ReturnZPopAll
	ld b, a
	ex de, hl
	b_call LoadCIndPaged
	ld (AppBackUpScreen+20), a
	ld a, 83h
	cp c
	jr nz, ReturnZPopAll
	ld a, (AppBackUpScreen+20)
	inc hl
;	push hl
;	ld hl, JumpToOldHook
;	ld de, AppBackUpScreen
;	ld bc, EndJump - JumpToOldHook
;	ldir
;	pop de
;	pop hl
;	pop bc
;	pop af
;	ld (AppBackUpScreen+3), a
;	in a, (6)
;	ld (AppBackUpScreen+8), a
;	ld a, (AppBackUpScreen+20)
;	ld (AppBackUpScreen+5), de
;	call AppBackUpScreen
;Return:
;	ret

	ld (AppBackUpScreen), hl
	ld (AppBackUpScreen+2), a
	ld (appChangeHookPtr), hl
	ld (appChangeHookPtr+2), a
	pop hl
	pop de
	pop bc
	pop af
	rst 28h
	dw AppBackUpScreen+4000h
	call RestoreHooks
	ret

AppChangePop:
	pop hl
	pop de
	pop bc
	pop af
	ret


;ChainAppChangeHook:																		;Restores registers and passes onto the chained app's hook
;	ld hl, AppVarName
;	rst rMov9ToOP1
;	b_call ChkFindSym
;	jr c, ReturnZPopAll
;	xor a
;	cp b
;	jr z, NotArc333
;	b_call Arc_Unarc
;	b_call ChkFindSym
;NotArc333:
;	ld hl, 31
;	add hl, de
;	ld e, (hl)
;	inc hl
;	ld d, (hl)
;	xor a
;	cp d
;	jr z, ReturnZPopAll
;	inc hl
;	ld a, (hl)
;	push de
;	push bc
;	ld hl, JumpToOldHook3
;	ld de, AppBackUpScreen
;	ld bc, EndJump3 - JumpToOldHook3
;	ldir
;	jp AppBackUpScreen
;
;
;JumpToOldHook3:
;	out (6), a
;	pop bc
;	pop hl
;	ld a, (hl)
;	cp 83h
;	jr nz, RetZ3
;	inc hl
;	ex de, hl
;	ld ix, 0
;	add ix, de
;	pop hl
;	pop de
;	pop bc
;	pop af
;	jp (ix)
;RetZ3:
;	pop hl
;	pop de
;	pop bc
;	pop af
;	cp a
;	ret
;EndJump3:




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



ParserHook:
	db 83h
	call RestoreHooks
	or a
	jr nz, Function
	ld de, parseVar+1
	ld a, (de)
	cp '#'
	jr nz, ReturnZ
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NotArcc2
	b_call Arc_Unarc
	b_call ChkFindSym
NotArcc2:
	inc de
	inc de
	ex de, hl
	ld de, 0FFFDh
	ld bc, 3
	ldir
	ld de, 6
	add hl, de
	ld (hl), 0
	inc hl
	ld (hl), 0



	
	ld hl, parseVar
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, ReturnZ
	ld a, (de)
	ld c, a
	inc de
	ld a, (de)
	ld b, a
	dec bc
	inc de
	ld a, (de)
	cp tProg												;If the first token is prgm, continue
	jr z, FindName






	cp t2ByteTok
;	jr nz, ReturnZ
	jr nz, Run#
	inc de
	ld a, (de)
	cp tasm												;If the first tokens are Asm(prgm, skip the Asm and continue
;	jr nz, ReturnZ
	jr nz, Run#
	inc de
	ld a, (de)
	cp tProg
	jr z, FindName
	cp a
	ret




Run#:
	ld hl, PrgmPound
	rst rMov9ToOP1
	b_call ChkFindSym
	ret c
	call PreParser
	ld hl, SmallParserHook
	ld (parserHookPtr), hl
	in a, (6)
	ld (parserHookPtr+2), a
	b_call ParseInp
	b_call PushRealO1
	ld hl, PrgmPound
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, Huh?
	call PreParser_Restore
Huh?:
	res progExecuting, (iy + newDispF)
	res cmdexec, (iy + cmdFlags)
	b_call PopRealO1
;	b_call ZeroOP1
	or 1
	ret

SmallParserHook:
	db 83h
	call RestoreHooks
	cp a
	ret


TooManyPrograms:
	b_call ClrLCDFull
	res appTextSave, (IY + appFlags)
	ld a, E_Memory
	ld (errNo), a
	b_call DispErrorScreen

	b_call RunIndicOn

	call CleanUp


	ld a, E_Memory
	res 7, a
	b_jump JError




FindName:
	dec de
	ld (basic_pc), de
	b_call ParsePrgmName

ParserEntrypoint:
	b_call ChkFindSym
	jr nc, Good
	ld a, ProtProgObj
	ld (OP1), a
	b_call ChkFindSym
	jr nc, Good
	b_jump ErrUndefined
Good:
	b_call PushRealO1
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NotArcc3
	b_call Arc_Unarc
	b_call ChkFindSym
NotArcc3:
	ld hl, 11
	add hl, de
	ld a, (hl)
	cp 9
	jr z, TooManyPrograms
	inc a
	ld (hl), a
	push hl
	b_call PopRealO1
	call PushRealO1
	pop hl
	inc hl
	inc hl
	ex hl, de
	ld hl, OP1+1
	ld bc, 8
	ldir
	b_call ChkFindSym
	ret c
	xor a
	cp b
	jr nz, RunningArchived
	b_call OP1ToOP3
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 8
	add hl, de
	ld (hl), 0
	b_call OP3ToOP1
	call PushRealO1
	b_call ChkFindSym
	inc de
	inc de
	ld a, (de)
	cp t2ByteTok
	jr nz, NotAsm
	inc de
	ld a, (de)
	cp tasmPrgm
	jr z, Asm
	cp tasmCmp
	jr nz, NotAsm
Asm:
	ld hl, AsmErrors
	call 59h
	call runProgram
	call 5Ch
	set appTextSave, (iy + appFlags)
	ld hl, 0
	ld (curRow), hl
	set indicRun, (iy + indicFlags)
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 0FFFDh
	inc de
	inc de
	ld bc, 3
	ldir
	ld hl, 6
	add hl, de
	ld a, (hl)
	dec a
	ld (hl), a
	call PopRealO1
	call PopRealO1
	or 1
	ret
NotAsm:
	call PreParser
	set progExecuting, (iy + newDispF)
	set cmdexec, (iy + cmdFlags)
	ld hl, AsmErrors
	call 59h
	in a, (2)
	rla
	sbc a, a
	out (20h), a
	b_call ParseInp
	call 5Ch
	b_call OP1ToOP6
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 11
	add hl, de
	ld a, (hl)
	dec a
	ld (hl), a
	push af
	call PopRealO1
	call PopRealO1
	call PreParser_Restore
	pop af
	cp 0
	jr z, LastProgram
	or 1
	ret
RunningArchived:
	b_call OP1ToOP3
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 8
	add hl, de
	ld (hl), 1
	b_call OP3ToOP1
	b_call ChkFindSym
	ex de, hl
	call LoadCIndPaged_inc
	call LoadDEIndPaged_inc
	ld de, 6
	call BHL_Plus_DE
	call LoadCIndPaged_inc
	ld e, c
	call BHL_Plus_DE
	call LoadDEIndPaged_inc
	call LoadDEIndPaged_inc
	ld a, e
	cp t2ByteTok
	jr nz, ArcNotAsm
	ld a, d
	cp tasmPrgm
	jr z, ArcAsm
	cp tasmCmp
	jr nz, ArcNotAsm
ArcAsm:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 9
	add hl, de
	xor a
	cp (hl)
	jr z, NoWriteback
	call PopRealO1
	call PushRealO1
	call CopyPrgmToRam
	jr YesWriteback
NoWriteback:
	call PopRealO1
	call PushRealO1
YesWriteback:
	call PushRealO1
	ld hl, AsmErrors
	call 59h
	call runProgram
	call 5Ch
	set appTextSave, (iy + appFlags)
	ld hl, 0
	ld (curRow), hl
	set indicRun, (iy + indicFlags)
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 0FFFDh
	inc de
	inc de
	ld bc, 3
	ldir
	ld hl, 6
	add hl, de
	ld a, (hl)
	dec a
	ld (hl), a
	dec hl
	dec hl
	xor a
	cp (hl)
	jr z, NoWriteback2
	call PopRealO1
	b_call OP1ToOP2
	call PopRealO1
	b_call PushRealO1
	call SmartWriteback
	jr z, DeleteTemporary
	b_call PopRealO1
	b_call OP2ToOP3
	b_call PushRealO1
	call TempToOriginalAsm
	b_call PopRealO1
	b_call ChkFindSym
	b_call Arc_Unarc
	or 1
	ret
NoWriteback2:
	call PopRealO1
	call PopRealO1
	or 1
	ret
DeleteTemporary:
	b_call PopRealO1
	b_call OP2ToOP1
	b_call ChkFindSym
	b_call DelVarArc
	or 1
	ret

TempToOriginalAsm:
	b_call PushRealO1
	b_call ChkFindSym
	b_call DelVarArc
	b_call PopRealO1
	b_call PushRealO1
	b_call OP3ToOP1
	b_call ChkFindSym
	push hl
	b_call PopRealO1
	b_call PushRealO1
	pop hl
	ld a, (OP1)
	push af
	call renameprog
	b_call PopRealO1
	ld a, 5
	ld (OP1), a
	b_call ChkFindSym
	pop af
	ld (hl), a
	ret

;TempToOriginalAsm:
;	b_call OP1ToOP4
;	b_call ChkFindSym
;	b_call DelVarArc
;	b_call OP3ToOP1
;	b_call ChkFindSym
;	ld de, -7
;	add hl, de
;	push hl
;	b_call OP4ToOP1
;	pop hl
;	ld de, OP1+1
;	ld b, 0
;CopyNameLoop:
;	ld a, (de)
;	cp 0
;	jr z, EndCopyNameLoop
;	ld (hl), a
;	inc de
;	dec hl
;	inc b
;	jr CopyNameLoop
;EndCopyNameLoop:
;	ld a, 8
;	sub a, b
;	ret z
;	ld c, a
;	ld b, 0
;	scf
;	ccf
;	sbc hl, bc
;	ld e, c
;	ld d, b
;	push hl
;	push de
;	b_call DelMem
;	pop de
;	pop hl
;	add hl, de
;	ld (hl), e
;	ret

;TempToOriginalAsm:
;	b_call OP1ToOP4
;	b_call ChkFindSym
;	b_call DelVarArc
;	b_call OP3ToOP1
;	b_call ChkFindSym
;	ex de, hl
;	ld c, (hl)
;	inc hl
;	ld b, (hl)
;	inc hl
;	push hl
;	push bc
;	push bc
;	b_call OP4ToOP1
;	pop hl
;	b_call CreateProg
;	inc de
;	inc de
;	pop bc
;	pop hl
;	ld a, b
;	or c
;	jr z, ItsEmpty2
;	ldir
;ItsEmpty2:
;	b_call OP3ToOP1
;	b_call ChkFindSym
;	b_call DelVarArc
;	ret





ArcNotAsm:
;	ld hl, AppVarName
;	rst rMov9ToOP1
;	b_call ChkFindSym
;	ld hl, 12
;	add hl, de
;	ld a, (hl)
;	inc a
;	ld (hl), a

	b_call OP3ToOP1
	call CopyPrgmToRam

	call PushRealO1
	b_call PushRealO1
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 21
	add hl, de
	ex de, hl
	push de
	b_call PopRealO1
	pop de
	ld hl, OP1+1
	ld bc, 8
	ldir
;	ld hl, OP1
;	ld de, parseVar
;	ld bc, 8
;	ldir
	call PreParser
	set progExecuting, (iy + newDispF)
	set cmdexec, (iy + cmdFlags)
	ld hl, BasicErrors
	call 59h
	in a, (2)
	rla
	sbc a, a
	out (20h), a
	b_call ParseInp
	call 5Ch
	b_call OP1ToOP6
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 11
	add hl, de
	ld a, (hl)
	dec a
	ld (hl), a
	push af
	inc hl
	ld a, (hl)
	dec a
	ld (hl), a
	call PopRealO1
	b_call ChkFindSym
	jr c, ArcGone
	b_call DelVarArc
ArcGone:
	call PopRealO1
	pop af
	cp 0
	jr z, LastProgram
	or 1
	ret




LastProgram:
	res progExecuting, (iy + newDispF)
	res cmdexec, (iy + cmdFlags)
	b_call OP6ToOP1
	or 1
	ret




BasicErrors:
	call PopRealO1
	b_call OP1ToOP2
	call PopRealO1
	call PushRealO1
	b_call OP1ToOP6
	b_call OP2ToOP1
	call PushRealO1

;	call CleanUp



	call PreParser_Restore

;	b_call OP6ToOP1
;	call CopyPrgmToRam

	ld hl, OP1+1
	ld de, progToEdit
	ld bc, 8
	ldir

	b_call OP1ToOP5

	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 13
	add hl, de
	ex de, hl
	ld hl, OP6+1
	ld bc, 8
	ldir
	ld hl, OP5+1
	ld bc, 8
	ldir
	
	call FixGoto

	res progExecuting, (iy + newDispF)
	res cmdexec, (iy + cmdFlags)

	b_jump JErrorNo


basic_start equ 965Bh

_TokToKey equ 4A0Bh

_DispErrorScreen equ 49DEh

AsmErrors:
	call PopRealO1
	call PushRealO1

	ld hl, OP1+1
	ld de, progToEdit
	ld bc, 8
	ldir

;	call CleanUp



	call PreParser_Restore

	call FixGoto	


	res progExecuting, (iy + newDispF)
	res cmdexec, (iy + cmdFlags)

	b_jump JErrorNo





FixGoto:
	ld a, ProgObj
	ld (OP1), a
	ld hl, progToEdit
	ld de, OP1+1
	ld bc, 8
	ldir
	b_call ChkFindSym
	ld hl, (basic_pc)
	inc de
	push de
	scf
	ccf
	sbc hl, de
	ld b, h
	ld c, l
	pop hl
FixGotoLoop:
	inc hl
	ld a, b
	or c
	ret z
	dec bc
	ld a, (hl)
	cp t2ByteTok
	jr z, TwoBytes
	cp tString
	jr z, Quotes
	cp tProg
	jr nz, FixGotoLoop
	ld de, (basic_pc)
	dec de
	dec de
	dec de
	dec de
	dec de
	ld (basic_pc), de
	ld a, 5
	cp c
	ret nc
	dec bc
	dec bc
	dec bc
	dec bc
	dec bc
	jr FixGotoLoop
TwoBytes:
	inc hl
	ld a, b
	or c
	ret z
	dec bc
	ld a, (hl)
	cp tArchive
	jr z, SkipAByte
	cp tUnarchive
	jr z, SkipAByte
	cp tasm
	jr nz, FixGotoLoop
	inc hl
	ld a, b
	or c
	ret z
	dec bc
	ld a, (hl)
	cp tProg
	jr nz, FixGotoLoop
	ld de, (basic_pc)
	dec de
	dec de
	dec de
	ld (basic_pc), de
	ld a, 3
	cp c
	ret nc
	dec bc
	dec bc
	dec bc
	jr FixGotoLoop
Quotes:
	inc hl
	ld a, b
	or c
	ret z
	dec bc
	ld a, (hl)
	cp tString
	jr z, FixGotoLoop
	cp tEnter
	jr z, FixGotoLoop
	jr Quotes
SkipAByte:
	inc hl
	ld a, b
	or c
	ret z
	dec bc
	jr FixGotoLoop




CleanUp:
	ld hl, PrgmPound
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, WTFNo#?
	call PreParser_Restore
WTFNo#?:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ret c
	ld hl, 12
	add hl, de
	ld b, (hl)
	ld hl, tempProgName
	push bc
	rst rMov9ToOP1
CleanLoop:
	pop bc
	xor a
	cp b
	jr z, EndCleanLoop
	ld a, b
	ld (OP1+8), a
	dec bc
	push bc
	b_call ChkFindSym
	jr c, CleanLoop
	b_call DelVarArc
	jr CleanLoop
EndCleanLoop:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 12
	add hl, de
	ld (hl), 0
	dec hl
	ld b, (hl)
	push bc
FixTokenLoop:
	pop bc
	xor a
	cp b
	jr z, FinishCleanUp
	dec b
	push bc
	call PopRealO1
	b_call OP1ToOP2
	call PopRealO1
	ld hl, OP1
	ld de, OP2
	ld b, 9
	call CompareStrings
	jr nz, FixTokenLoop
	b_call ChkFindSym
	jr c, FixTokenLoop
	xor a
	cp b
	jr nz, FixTokenLoop
	inc de
	inc de
	ex de, hl
	ld a, t2ByteTok
	cp (hl)
	jr z, Check2ndByte
ItsBasic:
	call PreParser_Restore
	jr FixTokenLoop

Check2ndByte:
	inc hl
	ld a, (hl)
	cp tasmPrgm
	jr z, FixTokenLoop
	cp tasmCmp
	jr z, FixTokenLoop
	jr ItsBasic

FinishCleanUp:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 11
	add hl, de
	ld (hl), 0
	ret


Function:
	push af
	ld a, c
	cp 1
	jr z, XX01
	cp b
	jr nz, ChainParserNew
	cp 8Ah
	jr z, RealTok
	cp 0C0h
	jr nz, ChainParserNew
	
	pop af
	ld a, 2
	cp l
	jr nz, ReturnZ
	dec hl
	b_call PopOP3
	b_call PushOP3
	ld a, (OP3)
	cp cplxobj
	jr z, ComplexLog
	b_call LogX
	b_call OP1ExOP2
	b_call PopRealO1
	ld hl, 0
	b_call PushRealO2
	b_call LogX
	b_call PopRealO2
	b_call FPDiv
	or 1
	ret

XX01:
	ld a, b
	cp 5Bh
	jr z, ArcTok
	cp 5Ch
	jr z, UnarcTok
	cp 0Bh
	jr z, StopTok
	jr ChainParserNew

ComplexLog:
	b_call LogX
	b_call OP1ExOP6
	b_call PopMCplxO1
	b_call PushRealO6
	b_call CLog
	b_call PopRealO3
	b_call CDivByReal
	ld hl, 0
	or 1
	ret

_IncFetch equ 4B73h
basic_pc equ 965Dh

ArcTok:																				;Allows archiving programs from within a program
	push bc
	push de
	push hl
	ld hl, (basic_pc)
	push hl
	ld hl, ArcHandler
	call 59h
	b_call ParsePrgmName
	call 5Ch
	b_call ChkFindSym
	jr c, ArcHandler
	xor a
	cp b
	jr nz, PopAllReturnNZ
	b_call Arc_Unarc
	jr PopAllReturnNZ



ArcHandler:
	pop hl
	ld (basic_pc), hl
	pop hl
	pop de
	pop bc
	pop af
	jr ReturnZ

PopAllReturnNZ:
	pop hl
	pop hl
	pop de
	pop bc
	pop af
	xor a
	cp 1
	ret


UnarcTok:																	;Allows unarchiving programs from within a program
	push bc
	push de
	push hl
	ld hl, (basic_pc)
	push hl
	ld hl, ArcHandler
	call 59h
	b_call ParsePrgmName
	call 5Ch
	b_call ChkFindSym
	jr c, ArcHandler
	xor a
	cp b
	jr z, PopAllReturnNZ
	b_call Arc_Unarc
	jr PopAllReturnNZ



StopTok:																		;Handles Stop tokens without a ram clear
	pop af
	call CleanUp
	b_call DispDone
	B_JUMP JForceCmdNoChar


RealTok:																		;Handles prgm tokens converted to real(42," so that subprograms are handled properly
	push hl
	ld a, l
	cp 2
	jr nz, ChainParserAHL
	xor a
	cp h
	jr nz, ChainParserAHL
	push bc
	push de
	b_call OP1ToOP6
	b_call PopRealO1
	b_call OP1ToOP5
	b_call CkPosInt
	jr nz, ChainParserReloadNew
	b_call ConvOP1
	cp 42
	jr z, YesReal
	cp 43
	jr nz, ChainParserReloadNew
YesReal:
	pop de
	pop bc
	pop hl
	pop af
	ld hl, 0
	b_call OP6ToOP1

	b_call ChkFindSym
	ex de, hl
	ld c, (hl)
	inc hl
	ld b, 0
	inc hl

	push hl
	b_call ZeroOP1
	pop hl

	ld a, c
	cp 9
	jr c, EightCharsOrLess
	ld bc, 8
EightCharsOrLess:
	ld a, ProgObj
	ld (OP1), a
	ld de, OP1 + 1
	ldir

	ld hl, OP1
	ld b, 9
ParenLoop:
	inc hl
	dec b
	xor a
	cp b
	jr z, EndParenLoop
	ld a, (hl)
	cp tRParen
	jr nz, ParenLoop
	ld (hl), 0
EndParenLoop:
	b_call ChkFindSym
	jr nc, GoodProg
	ld a, ProtProgObj
	ld (OP1), a
	b_call ChkFindSym
	jr c, ThrowError
GoodProg:
	call ParserEntrypoint
	jr z, ThrowError
	res numOP1, (iy + ParsFlag2)
	xor a
	cp 1
	ret

ThrowError:
	ld a, E_Undefined
	b_call JError

ChainParserReloadNew:
	b_call PushRealO5
	b_call OP6ToOP1
	pop de
	pop bc
ChainParserAHL:
	pop hl
ChainParserNew:
	push bc
	push hl
	ld de, (0FFFDh)
	ld a, (0FFFFh)
	cp 0
	jr z, ParserPop
	ld b, a
	ex de, hl
	b_call LoadCIndPaged
	ld (AppBackUpScreen+20), a
	ld a, 83h
	cp c
	jr nz, ParserPop
	ld a, (AppBackUpScreen+20)
	inc hl
;	push hl
;	ld hl, JumpToOldHook
;	ld de, AppBackUpScreen
;	ld bc, EndJump - JumpToOldHook
;	ldir
;	pop de
;	pop hl
;	pop bc
;	pop af
;	ld (AppBackUpScreen+3), a
;	in a, (6)
;	ld (AppBackUpScreen+8), a
;	ld a, (AppBackUpScreen+20)
;	ld (AppBackUpScreen+5), de
;	call AppBackUpScreen
;Return:
;	ret

	ld (AppBackUpScreen), hl
	ld (AppBackUpScreen+2), a
	ld (parserHookPtr), hl
	ld (parserHookPtr+2), a
	pop hl
	pop bc
	pop af
	rst 28h
	dw AppBackUpScreen+4000h
	jr z, ParserZero
	call RestoreHooks
	or 1
	ret
ParserZero:
	call RestoreHooks
	cp a
	ret

ParserPop:
	pop hl
	pop bc
	pop af
	cp a
	ret



;JumpToOldHook:
;	out (6), a
;	ld a, 0FFh
;	call 0FFFFh
;	ld a, 0FFh
;	out (6), a
;	ret
;EndJump:



PrgmPound:
	db ProgObj, "#", 0



tempProgName:
	db ProgObj, "CalcUt", tGoto, 1

LoadCIndPaged_inc:
	b_call LoadCIndPaged
inc_BHL:
	inc hl
	bit 7, h
	ret z
	inc b
	res 7, h
	set 6, h
	ret

LoadDEIndPaged_inc:
	b_call LoadDEIndPaged
	jr inc_BHL
BHL_Plus_DE:
BHL_plus_DE:
	add hl, de
	bit 7, h
	ret z
	inc b
	res 7, h
	set 6, h
	ret







ReturnZ:
	cp a
	ret

CompareStrings:
	ld a, (de)
	cp (hl)
	ret nz
	inc de
	inc hl
	djnz CompareStrings
	cp a
	ret

Save1:
	db " Do you want to ", "  save changes  ", 0
Save2:
	db " Press any key  ", "  to return to  ", "  the editor.   ", "----------------", " Yes |    |  No", 0



RawKeyHook:
	db 83h
	call RestoreHooks
	cp kAppsMenu
	jr z, App2Fin
	cp kQuit
	jr z, Quitting
	cp kOff
	jr nz, CheckNum
	ld a, (cxCurApp)
	cp kPrgmEd
	jr nz, TurnOff
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 29
	add hl, de
	ld a, 1
	ld (hl), a
	ld a, kQuit
	cp 1
	ret
TurnOff:
	ld a, kOff
	cp 1
	ret
App2Fin:
	ld a, (cxCurApp)
	cp kPrgmEd
	jr z, NoApps
	ld a, kAppsMenu
	jr ChainRawKey
NoApps:
	ld a, kFin
	cp 1
	ret
	

QuitHook2:
	xor a
	cp a
	ret

Startup:
	ld a, tS + 5Eh
	jr NoOnNecessary


CheckNum:
	cp k1
	jr c, ProgList
	cp k9 + 1
	jr nc, ProgList

	ld (AppBackUpScreen), a

	push af
	in a, (4)
	bit 3, a
	jr nz, ChainRawKeyPop



;	ld hl, AppVarName
;	rst rMov9ToOP1
;	b_call ChkFindSym
;	jr c, QuitHook2
;	xor a
;	cp b
;	jr z, NotArc11
;	b_call Arc_Unarc
;	b_call ChkFindSym
;NotArc11:
;	ld hl, 20
;	add hl, de
;	ld (hl), 42

	;ld a, (cxCurApp)
	;cp kPrgmEd
	;jr z, PopAReturnZ

	ld a, kQuit
	b_call NewContext0



	pop af

NoOnNecessary:
	ld (AppBackUpScreen), a
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, QuitHook2
	xor a
	cp b
	jr z, NotArc12
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc12:
	ld hl, 12
	add hl, de
	ld (hl), 0
;	ld de, 8
;	add hl, de
;	ld (hl), 0

;	b_call DisableGetKeyHook
	ld hl, PROGLIST
	rst rMov9ToOP1
	b_call ChkFindSym
	ld a, (AppBackUpScreen)
	ret c
	ld a, (AppBackUpScreen)
	add -5Eh
	ld (AppBackUpScreen), a
	bit editOpen, (IY + editFlags)
	jr z, NoEdit
	b_call CloseEditBuf
NoEdit:
	ld hl, PROGLIST
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, NoMatch
	xor a
	cp b
	call nz, CopyPrgmToRam
	ex de, hl
	ld c, (hl)
	inc hl
	ld b, (hl)
	inc hl

NumLoop:
	ld a, (AppBackUpScreen)
	cp (hl)
	jr nz, IncLoop
	inc hl
	dec bc
	ld a, b
	or c
	jr z, NoMatch
	ld a, tcolon
	cp (hl)
	jr nz, NumLoop
	inc hl
	dec bc
	ld a, b
	or c
	jr z, NoMatch
	ld a, tprog
	cp (hl)
	jr nz, NumLoop
	dec hl
	inc bc
	ld (basic_pc), hl
FindEOL:
	ld a, tenter
	inc hl
	dec bc
	cp (hl)
	jr z, FoundEOL
	ld a, b
	or c
	jr nz, FindEOL



FoundEOL:
	dec hl
	ld (basic_end), hl
	b_call ParsePrgmName
	b_call OP1ToOP6

	ld hl, tempProgName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, NotAround2
	b_call DelVarArc
NotAround2:


	ld a, kOff
	b_call NewContext
	b_call BufClear
	ld a, kClear
	b_call cxMain
	ld e, 5Fh
	ld d, 0
	b_call TokToKey
	b_call cxMain
	;b_call Mon
	ld hl, OP6 + 1
	ld b, 8
loop:
	ld d, 0
	ld a, (hl)
	cp d
	jr z, FinishedInserting
	ld e, a
	push bc
	push hl
	b_call TokToKey
	b_call cxMain
	pop hl
	pop bc
	inc hl
	djnz loop
FinishedInserting:
;	call CleanUp
;	ld a, (AppBackUpScreen)
;	add 5Eh
	ld a, kEnter
	b_call cxMain
	xor a
	cp 0
	ret
;	b_call Mon




;	b_call RunIndicOn
;
;	call CheckArchivedRunAlreadyInOP1
;	ld hl, tempProgName
;	rst rMov9ToOP1
;	b_call ChkFindSym
;	jr c, QuitHook2
;	b_call DelVarArc
;	ld a, kQuit
;	cp a
;	ret




_TokToKey equ 4A0Bh
_cxMain			equ 4045h


IncLoop:
	inc hl
	dec bc
	ld a, b
	or c
	jr nz, NumLoop


NoMatch:
;	call CleanUp
	ld hl, tempProgName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, NotAround
	b_call DelVarArc
NotAround:
	ld a, (AppBackUpScreen)
	add 5Eh
	cp a
	ret





ProgList:
	push af
	ld a, (MenuCurrent)
	cp 3
	jr nz, ChainRawKeyPop
	ld a, (MenuCurrent+1)
	cp 2
	jr z, ChainRawKeyPop
	pop af
	cp kcapa
	jr z, Archive
	cp kadd
	jr z, Description
	cp kcapl
	jr nz, ChainRawKey
	ld hl, 85E7h
	rst rMov9ToOP1
	b_call ChkFindSym
	xor a
	cp b
	jr nz, ArchivedLock
	ld a, (hl)
	cp 5
	jr z, Lock
	ld (hl), 5
	xor a
	ret
Lock:
	ld (hl), 6
	xor a
	ret
ArchivedLock:
	b_call CloseEditBuf
	ld hl, 85E7h
	rst rMov9ToOP1
	b_call ChkFindSym
	b_call Arc_Unarc
	ld hl, 85E7h
	rst rMov9ToOP1
	b_call ChkFindSym
	ld a, (hl)
	cp 5
	jr z, ArcLock
	ld (hl), 5
	b_call Arc_Unarc
	xor a
	ret
ArcLock:
	ld (hl), 6
	b_call Arc_Unarc
	xor a
	ret
Archive:
	b_call CloseEditBuf
	ld a, 2
	ld (curCol), a
	ld hl, 85E7h
	rst rMov9ToOP1
	xor a
	ld (OP1+9), a
	b_call ChkFindSym
	xor a
	cp b
	jr z, NotArchived
	b_call Arc_Unarc
	ld hl, OP1+1
	xor a
	ld (OP1+9), a
FindEndLoop:
	inc hl
	ld b, (hl)
	inc b
	djnz FindEndLoop
	ld (hl), ' '
	inc hl
	ld (hl), b
	ld hl, OP1+1
	b_call PutS
	xor a
	ret
NotArchived:
	b_call Arc_Unarc
	xor a
	ld (OP1+9), a
	ld a, '*'
	ld (OP1), a
	ld hl, OP1
	b_call PutS
	xor a
	ret
Description:
	ld a, (85E8h)
	cp '@'
	jr nz, NotEmpty
	xor a
	ret
NotEmpty:
	b_call ClrLCDFull
	ld hl, 0
	ld (curRow), hl
	ld hl, Name
	call PutSApp
	ld hl, 85E7h
	rst rMov9ToOP1
	xor a
	ld (OP1+9), a
	ld hl, OP1+1
NameLengthLoop:
	inc hl
	inc a
	ld b, (hl)
	inc b
	djnz NameLengthLoop
	ld (AppBackUpScreen), a
	ld hl, OP1+1
	b_call PutS
	b_call NewLine
	ld hl, Locked
	call PutSApp
	ld hl, No
	ld a, (OP1)
	cp 5
	jr z, Unlocked
	ld hl, Yes
Unlocked:
	call PutSApp
	b_call NewLine
	ld hl, Archived
	call PutSApp
	b_call ChkFindSym
	xor a
	cp b
	jr z, Unarchived
	ld hl, Yes
	call PutSApp
	b_call NewLine
	ld hl, Size
	call PutSApp
	ex de, hl
	call LoadCIndPaged_inc
	call LoadDEIndPaged_inc
	ld de, 6
	call BHL_Plus_DE
	call LoadCIndPaged_inc
	ld e, c
	call BHL_Plus_DE
	call LoadDEIndPaged_inc
	push bc
	push hl
	push de
	ld hl, 9
	ld a, (AppBackUpScreen)
	ld e, a
	ld d, 0
	add hl, de
	pop de
	add hl, de
	b_call DispHL
	b_call NewLine
	ld hl, Type
	call PutSApp
	pop hl
	pop bc
	call LoadCIndPaged_inc
	ld a, 0BBh
	cp c
	jr nz, ArcBasic
	call LoadCIndPaged_inc
	ld a, 06Dh
	cp c
	jr nz, ArcBasic
	ld hl, Assembly
	call PutSApp
	jr Wait
ArcBasic:
	ld hl, Basic
	call PutSApp
	jr Wait
Unarchived:
	ld hl, No
	call PutSApp
	b_call NewLine
	ld hl, Size
	call PutSApp
	ex de, hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	push hl
	push de
	ld hl, 9
	ld a, (AppBackUpScreen)
	ld e, a
	ld d, 0
	add hl, de
	pop de
	add hl, de
	b_call DispHL
	b_call NewLine
	ld hl, Type
	call PutSApp
	pop hl
	inc hl
	ld a, 0BBh
	cp (hl)
	jr nz, UnarcBasic
	inc hl
	ld a, 06Dh
	cp (hl)
	jr nz, UnarcBasic
	ld hl, Assembly
	call PutSApp
	jr Wait
UnarcBasic:
	ld hl, Basic
	call PutSApp
Wait:
	b_call GetKey
	ld a, kPrgm
	ret



Name:
	db "Name: ", 0
Locked:
	db "Locked: ", 0
Archived:
	db "Archived: ", 0
Yes:
	db "Yes", 0
No:
	db "No", 0
Size:
	db "Size: ", 0
Type:
	db "Type: ", 0
Assembly:
	db "Assembly", 0
Basic:
	db "Basic", 0

ChainRawKeyPop:
	pop af
ChainRawKey:
	push af
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, PopAReturnZ
	xor a
	cp b
	jr z, NotArc13
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc13:
	ld hl, 5
	add hl, de
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	ld a, (hl)
	cp 0
	jr z, PopAReturnZ
	ex de, hl
	ld b, a
	b_call LoadCIndPaged
	ld a, 83h
	cp c
	jr nz, PopAReturnZ
	inc hl
	ld a, b
	ld (AppBackUpScreen), hl
	ld (AppBackUpScreen+2), a
	ld (rawKeyHookPtr), hl
	ld (rawKeyHookPtr+2), a
	pop af
	rst 28h
	dw AppBackUpScreen+4000h
	jr z, RawKeyZero
	push af
	call RestoreHooks
	pop af
	ret
RawKeyZero:
	call RestoreHooks
	cp a
	ret
;	push de
;	ld hl, JumpToOldHook2
;	ld de, AppBackUpScreen
;	ld bc, EndJump2 - JumpToOldHook2
;	ldir
;	jp AppBackUpScreen

Quitting:
	ld a, (cxCurApp)
	cp kPrgmInput
	ld a, kQuit
	jr nz, ReturnZ
	b_call CloseEditBuf
	call CleanUp
	ld a, kQuit
	cp a
	ret


PopAReturnZ:
	pop af
	jr ReturnZ

ChainReload:
	ld a, (AppBackUpScreen)
	jr ChainRawKey


JumpToOldHook2:
	out (6), a
	pop hl
	ld a, (hl)
	cp 83h
	jr nz, RetZ2
	inc hl
	pop af
	jp (hl)
RetZ2:
	pop af
	cp a
	ret
EndJump2:







basic_end equ 965Fh






PutSApp:
;Displays a string on the screen in large font at the current cursor position.
;INPUT:    HL->Zero terminated string.
;OUTPUT:   HL->Byte after zero terminator.
;DESTROYS: AF
;PLATFORM: 73,83P
       ld     a,(hl)
       inc    hl
       or     a
       ret    z
       B_CALL PutC
       jr     PutSApp

VPutSApp:
;Displays a string on the screen in small font at the current cursor position.
;INPUT:    Same as VPutS (HL->String)
;OUTPUT:   Same as VPutS (HL->Byte after zero terminator)
;DESTROYS: AF
;PLATFORM: 73,83P
        ld      a,(hl)
        inc     hl
        or      a
        ret     z
        push    de
        B_CALL  VPutMap
        pop     de
        ret     c
        jr      VPutSApp



PutSAppPrgmName:
	xor a
	ld (OP1+9), a
	ld hl, OP1+1
	b_call StrLength
	ld a, 8
	cp c
	jr nc, Just8
	ld c, 8
Just8:
	ld a, c
	push af
	cp 1
	jr z, OneChar
	cp 2
	jr z, OneChar
	cp 3
	jr z, ThreeChar
	cp 4
	jr z, ThreeChar
	cp 5
	jr z, FiveChar
	cp 6
	jr z, FiveChar
	xor a
	jr DispPrgm
OneChar:
	ld a, 3
	jr DispPrgm
ThreeChar:
	ld a, 2
	jr DispPrgm
FiveChar:
	ld a, 1
DispPrgm:
	ld (curCol), a
	ld hl, Prgm
	call PutSApp
	pop af
	ld hl, OP1+1
	ld b, a
PrgmLoop:
	xor a
	cp b
	jr z, QMark
   ld a,(hl)
   inc hl
   or a
   jr z, QMark
   B_CALL PutC
   jr PrgmLoop
QMark:
	ld a, '?'
	b_call PutC
	ld hl, 3
	ld (curRow), hl
	ret


Prgm:
	db "to prgm", 0

CopyPrgmToRam:
	ld a, (OP1)
	ld (AppBackUpScreen+42), a
	b_call ChkFindSym
	xor a
	cp b
	jr z, RamCopy
	ex de, hl
	call LoadCIndPaged_inc
	call LoadDEIndPaged_inc
	ld de, 6
	call BHL_Plus_DE
	call LoadCIndPaged_inc
	ld e, c
	call BHL_Plus_DE
	call LoadDEIndPaged_inc

ContCopy:
	push bc
	push hl
	push de
	push de
	b_call OP1ToOP3
	ld hl, tempProgName
	rst rMov9ToOP1
	ld a, (AppBackUpScreen+42)
	ld (OP1), a
	b_call PushRealO1

	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NotArc20
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc20:
	ld hl, 12
	add hl, de
	ld a, (hl)
	inc a
	ld (hl), a
	push af
;	push hl
	b_call PopRealO1
;	pop hl
	pop af
	ld (OP1 + 8), a
;	inc hl
;	ex de, hl
;	ld hl, OP3+1
;	ld bc, 8
;	ldir
	b_call OP1ToOP5
	b_call ChkFindSym
	jr c, notExist
	b_call DelVarArc
notExist:
	b_call OP5ToOP1
	pop hl
	b_call CreateProg
	ld a, (AppBackUpScreen+42)
	ld (OP1), a
	ld (hl), a
	ld a, (de)
	ld b, a
	inc de
	ld a, (de)
	or b
	jr nz, ContinueCopy
	b_call OP5ToOP1
	b_call ChkFindSym
	pop bc
	pop hl
	pop af
	ret
ContinueCopy:
	inc de
	pop bc
	pop hl
	pop af
	b_call FlashToRam
	b_call OP5ToOP1
	b_call ChkFindSym
	ret



RamCopy:
	ex de, hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	push hl
	push de
	push de
	b_call OP1ToOP3
	ld hl, tempProgName
	rst rMov9ToOP1
	b_call PushRealO1

	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NotArc202
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc202:
	ld hl, 12
	add hl, de
	ld a, (hl)
	inc a
	ld (hl), a
	push af
;	push hl
	b_call PopRealO1
;	pop hl
	pop af
	ld (OP1 + 8), a
;	inc hl
;	ex de, hl
;	ld hl, OP3+1
;	ld bc, 8
;	ldir
	b_call OP1ToOP5
	b_call ChkFindSym
	jr c, notExist2
	b_call DelVarArc
notExist2:
	b_call OP5ToOP1
	pop hl
	b_call CreateProg
	ld a, (de)
	ld b, a
	inc de
	ld a, (de)
	or b
	jr nz, ContinueCopy2
	b_call OP5ToOP1
	b_call ChkFindSym
	pop bc
	pop hl
	ret
ContinueCopy2:
	inc de
	pop bc
	pop hl
	ldir
	b_call OP5ToOP1
	b_call ChkFindSym
	ret





GetKeyHook:
	db 83h
	ld (AppBackUpScreen), a
	ld a, (0FFFCh)
	cp 0
	jr z, NoHook2
	ld hl, 0FFFCh
	ld de, getKeyHookPtr
	ld bc, 4
	ldir
	jr YesHook2
NoHook2:
	b_call DisableGetKeyHook
YesHook2:
;	call FixAllPrograms
;	call CleanNoFix
	ld hl, PROGLIST
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, GetKeyEnd
	b_call CursorOff
	ld a, (AppBackUpScreen)
	call Startup
	b_call CursorOn
	ld a, kQuit
	b_call NewContext0
GetKeyEnd:
	ld a, 1Ah
	cp 0
	ret

PROGLIST:
	db ProgObj, "PROGLIST"



AppVarStart:
;	ld hl, tempProgName2 - AppVarStart + 8001h
;	rst rMov9ToOP1
;	b_call ChkFindSym
;	jr c, QuitHook4
;	b_call DelVarArc
;	ret
;QuitHook4:
	bit 4,(IY+apdFlags)
	ret nz
	xor a
	ld (0FFFCh), a
	bit getKeyBit, (iy + getKeyHookFlag)
	jr z, NoHook
	ld hl, getKeyHookPtr
	ld de, 0FFFCh
	ld bc, 4
	ldir
NoHook:
	ld hl, AppName2 - AppVarStart + 8001h
	rst rMov9ToOP1
	b_call FindApp
	ret c
	out (6), a
	ld hl, GetKeyHook
	b_call EnableGetKeyHook
	ret

;tempProgName2:
;	db ProgObj, "CalcUtil"

AppName2:
	db appobj, "CalcUtil"

AppVarEnd:

Offscrpt:
	db AppVarObj, "OFFSCRPT"





						



_NewContext0 equ 4033h

_NewContext equ 4030h

_callMain equ 4045h

_MonReset equ 4C54h

_Mon equ 401Eh


   
;---------------------------------------------------------------------------------




getNoshellSettings:
       ld hl,AppVarName
       rst rMov9ToOP1
       b_call ChkFindSym
       jr nc, NotFound2
       call NewAppVar
NotFound2:
       ld hl, AppVarName
       rst rMov9ToOP1
       b_call ChkFindSym
       xor a
		 cp b
       jr z,inRAM
       b_call Arc_Unarc
inRAM:
       ld hl, 9
		 add hl, de
		 ld a, (hl)
       ld (flagByte),a
;       ld hl, 0
;       ld (oldHookBlock), hl
;       ld (oldHookBlock+2), hl
       ret

;--------------------------------------------------------------------

PushRealO1:
	ld hl, 9
	b_call EnoughMem
	ret c
	b_call PushRealO1
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ex de, hl
	ld c, (hl)
	inc hl
	ld b, (hl)
	push bc
	dec hl
	ex de, hl
	ld hl, 9
	add hl, bc
	ex de, hl
	ld (hl), e
	inc hl
	ld  (hl), d
	pop de
	add hl, de
	ex de, hl
	ld hl, 9
	b_call InsertMem
	push de
	b_call PopRealO1
	pop de
	b_call MovFrOP1
	xor a
	ret

PopRealO1:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ex de, hl
	ld c, (hl)
	inc hl
	ld b, (hl)
	push bc
	dec hl
	ex de, hl
	ld hl, -9
	add hl, bc
	ex de, hl
	ld (hl), e
	inc hl
	ld  (hl), d
	pop de
	add hl, de
	ex de, hl
	ld hl, -9
	add hl, de
	push hl
	b_call Mov9ToOP1
	pop hl
	ld de, 9
	b_call DelMem
	xor a
	ld (OP1+9), a
	ret



;---------------------------------------------------------------------------------


; Source code adapted from Omnicalc v1.10
; (C) 2002-2003 Michael Vincent.

;Parsing equates
bufferend EQU ramCode
sizechange EQU ramCode+2
sizebytes EQU ramCode+4
op1backup EQU ramCode+6	;9 bytes
oldtoken EQU ramCode+18
op1backup2 EQU ramCode+30
inQuotes EQU 0
parseFlags EQU asm_Flag2



PreParser:
;	ld a,(parserHookPtr+3)
;	dec a
;	jp z,PreParser_Recursive
	;OP1 is variable being parsed
	;We must change all tokens CFh and above to real(token-CF,arguments)
;	ld hl,OP1
;	ld de,op1backup
;	ld bc,9
;	ldir
	B_CALL ChkFindSym
	jp c,Hook_Parser_NotState
	xor a
	ld (parserHookPtr+3),a
	ex de,hl
	ld (sizebytes),hl
	ld c,(hl)
	inc hl
	ld b,(hl)	;Get size
	ld a,b
	or c
	ret z
	inc hl
	push hl
	add hl,bc
	ld (bufferend),hl
	pop hl
	;(bufferend)-1 is last token
	ld bc,0
	ld (sizechange),bc
	res inQuotes,(iy+parseFlags)
PreParser_Replace_Loop:
	ld a,(hl)
	cp tString
	call z,PreParser_QuoteFound
	cp tEnter
	call z,PreParser_ResetQuote
	bit inQuotes,(iy+parseFlags)
	jr nz,PreParser_Replace_NextToken
;	cp 0BBh
;	jr nz,PreParser_Replace_NextToken
;	inc hl
;	ld a,(hl)
;	cp 0CFh
	cp tProg
;	jr c,PreParser_Replace_NextToken
	jr nz,PreParser_CheckAsm

	inc hl
	push hl
	ld de,(bufferend)
	or a
	sbc hl,de
	pop hl
	jr z,PreParser_End
	ld a, (hl)
	cp 41h
	jr c, PreParser_Replace_NextToken
	cp 5Ch
	jr nc, PreParser_Replace_NextToken
	dec hl

	ld (hl),t2ByteTok
	push af
	;We found a program token...
	inc hl
	ex de,hl
	;DE = insertion address
	ld hl,5
	B_CALL InsertMem
	ld bc,(sizechange)
	inc bc
	inc bc
	inc bc
	inc bc
	inc bc
	ld (sizechange),bc
	ld bc,(bufferend)
	inc bc
	inc bc
	inc bc
	inc bc
	inc bc
	ld (bufferend),bc
	ex de,hl
	;HL now points to 5 bytes to store the real(##,"
	pop af
	ld (hl), tReal
	inc hl
	ld (hl), t4
	inc hl
	ld (hl), t2
PreParser_ReturnFromAsm:
	;Comma time
	inc hl
	ld (hl),tComma
	;Quotes
	inc hl
	ld (hl), tString
	;Done...Continue for more tokens
	ld a,1
	ld (parserHookPtr+3),a
PreParser_Replace_NextToken:
	inc hl
	push hl
	ld de,(bufferend)
	or a
	sbc hl,de
	pop hl
	jr nz,PreParser_Replace_Loop
	;Update size bytes now
PreParser_End:
	ld hl,(sizebytes)
	ld c,(hl)
	inc hl
	ld b,(hl)
	push hl
	ld hl,(sizechange)
	add hl,bc
	ex hl,de
	pop hl
	ld (hl),d
	dec hl
	ld (hl),e
	ret
;	ld a,(parserHookPtr+3)
;	or a
;	ret z
;	B_CALL PushRealO1
;	AppOnErr ParsingErrorOccured
;	B_CALL ParseInp
;	AppOffErr
;	;Change real(##,arguments) to token(arguments)
;	call PreParser_Restore
;	xor a
;	inc a
;	ret
SkipCompAsm:
	inc hl
	push hl
	ld de,(bufferend)
	or a
	sbc hl,de
	pop hl
	jr z,PreParser_End
	ld a, (hl)
	cp tColon
	jr z, PreParser_Replace_Loop
	cp tRParen
	jr z, PreParser_Replace_Loop
	cp tEnter
	jr z, PreParser_Replace_Loop
	jr SkipCompAsm
PreParser_QuoteFound:
	;Toggle flag
	push af
	ld a,(iy+parseFlags)
	xor 00000001b
	ld (iy+parseFlags),a
	pop af
	ret
PreParser_ResetQuote:
	res inQuotes,(iy+parseFlags)
	ret
ParsingErrorOccured:
	push af
	call PreParser_Restore
	pop af
	B_JUMP JError
PreParser_Recursive:
	xor a
	ld (parserHookPtr+3),a
	ret
PreParser_CheckAsm:
	cp t2ByteTok
	jr nz,PreParser_Replace_NextToken
	inc hl

	push hl
	ld de,(bufferend)
	or a
	sbc hl,de
	pop hl
	jr z,PreParser_End

	ld a, (hl)
	cp tasmComp
	jr z, SkipCompAsm

	ld a, (hl)
	cp tasm
	jr nz,PreParser_CheckArcUnarc
	inc hl

	push hl
	ld de,(bufferend)
	or a
	sbc hl,de
	pop hl
	jr z,PreParser_End

	ld a, (hl)
	cp tProg
	jr nz, PreParser_Replace_NextToken
	inc hl

	push hl
	ld de,(bufferend)
	or a
	sbc hl,de
	pop hl
	jr z,PreParser_End
	ld a, (hl)
	cp 41h
	jr c, PreParser_Replace_NextToken
	cp 5Ch
	jr nc, PreParser_Replace_NextToken

	dec hl
	dec hl
	ld (hl),tReal
	inc hl

	ld (hl),t4
	push af
	;We found a program token...
	inc hl
	ex de,hl
	;DE = insertion address
	ld hl,3
	B_CALL InsertMem
	ld bc,(sizechange)
	inc bc
	inc bc
	inc bc
	ld (sizechange),bc
	ld bc,(bufferend)
	inc bc
	inc bc
	inc bc
	ld (bufferend),bc
	ex de,hl
	;HL now points to 3 bytes to store the #,"
	pop af
	ld (hl), t3
	jr PreParser_ReturnFromAsm

PreParser_CheckArcUnarc:


	cp 0CFh
	jr c,PreParser_CheckArcUnarc2
	ld (hl),tReal
	push af
	;We found a BB,CF or greater token...
	inc hl
	ex de,hl
	;DE = insertion address
	ld hl,3
	B_CALL InsertMem
	ld bc,(sizechange)
	inc bc
	inc bc
	inc bc
	ld (sizechange),bc
	ld bc,(bufferend)
	inc bc
	inc bc
	inc bc
	ld (bufferend),bc
	ex de,hl
	;HL now points to 3 bytes to store the "##,"
	pop af
	sub 0CFh
	ld c,-1
PreParser_Get10:
	inc c
	sub 10
	jr nc,PreParser_Get10
	add a,10
	;C is number of 10s, A is remainder
	push af
	ld a,t0
	add a,c
	ld (hl),a
	pop af
	;Now second number
	inc hl
	add a,t0
	ld (hl),a
	;Comma time
	inc hl
	ld (hl),tComma
	;Done...Continue for more tokens
	ld a,1
	ld (parserHookPtr+3),a
	jr PreParser_Replace_NextToken




PreParser_CheckArcUnarc2:
	cp tArchive
	jr z, PreParser_SkipToken
	cp tUnarchive
	jr nz, PreParser_Replace_NextToken
PreParser_SkipToken:
	inc hl
	jr PreParser_Replace_NextToken


PreParser_Restore:
;	ld hl,OP1
;	ld de,op1backup2
;	ld bc,9
;	ldir
;	B_CALL PopRealO1
	b_call PushRealO1
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 34
	add hl, de
	ld a, (hl)
	ld (iy+asm_Flag1), a
	b_call PopRealO1
	B_CALL ChkFindSym
	ex de,hl
	ld (sizebytes),hl
	ld c,(hl)
	inc hl
	ld b,(hl)	;Get size
	inc hl
	push hl
	add hl,bc
	ld (bufferend),hl
	pop hl
	;(bufferend)-1 is last token
	ld bc,0
	ld (sizechange),bc
PreParser_Restore_Loop:
	ld a,(hl)
	cp 0BBh
	jr nz,PreParser_Restore_NextToken
	inc hl
	ld a,(hl)
	cp tReal
	jr nz,PreParser_Restore_NextToken
	;We found a Real token
	;Now check if it has "##," after it.



	inc hl
	ld a,(hl)
;	cp t0
;	jr c,PreParser_Restore_NextToken
	cp t4
	jr nz, CheckOmniBolicNoDec
;	cp t9+1
;	jr nc,PreParser_Restore_NextToken
	;Next # check
	inc hl
	ld a,(hl)
;	cp t0
;	jr c,PreParser_Restore_NextToken
;	cp t9+1
;	jr nc,PreParser_Restore_NextToken
	cp t2
	jr z,PreParser_Restore_Continue
	cp t3
	jr nz, CheckOmniBolic
PreParser_Restore_Continue:
	push af
	inc hl
	ld a,(hl)
	cp tComma
	jr nz,PreParser_Restore_NextToken
	inc hl
	ld a, (hl)
	cp tString
	jr nz,PreParser_Restore_NextToken
	;We have a real(##,"arguments)
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	ld (hl), tProg
	pop af
	cp t2
	jr z, PreParser_NotAsm
	ld (hl), t2ByteTok
	inc hl
	ld (hl), tasm
	inc hl
	ld (hl), tProg
	inc hl
	ld de,3
	push hl
	B_CALL DelMem
	pop hl
	dec hl
	ld bc,(sizechange)
	dec bc
	dec bc
	dec bc
	ld (sizechange),bc
	ld bc,(bufferend)
	dec bc
	dec bc
	dec bc
	ld (bufferend),bc
	;Done...Continue for more tokens
	jr PreParser_Restore_NextToken


CheckOmniBolic:
	dec hl
CheckOmniBolicNoDec:
	ld a, (iy+asm_Flag1)
	cp 0
	jr z, PreParser_Restore_NextToken
	ld a,(hl)
	cp t0
	jr c,PreParser_Restore_NextToken
	cp t9+1
	jr nc,PreParser_Restore_NextToken
	;Next # check
	inc hl
	ld a,(hl)
	cp t0
	jr c,PreParser_Restore_NextToken
	cp t9+1
	jr nc,PreParser_Restore_NextToken
	inc hl
	ld a,(hl)
	cp tComma
	jr nz,PreParser_Restore_NextToken
	;We have a real(##,arguments)
	dec hl
	dec hl
	ld a,(hl)
	sub t0
	or a
	jr z,PreParser_Restore_Add10_Skip
	ld b,a
	xor a
PreParser_Restore_Add10:
	add a,10
	djnz PreParser_Restore_Add10
PreParser_Restore_Add10_Skip:
	inc hl
	ld c,a
	ld a,(hl)
	sub t0
	add a,c
	dec hl
	ld (oldtoken),a
	ld de,3
	push hl
	B_CALL DelMem
	pop hl
	dec hl
	ld a,(oldtoken)
	add a,0CFh
	ld (hl),a
	ld bc,(sizechange)
	dec bc
	dec bc
	dec bc
	ld (sizechange),bc
	ld bc,(bufferend)
	dec bc
	dec bc
	dec bc
	ld (bufferend),bc
	;Done...Continue for more tokens




	jr PreParser_Restore_NextToken

PreParser_NotAsm:
	inc hl
	ld de,5
	push hl
	B_CALL DelMem
	pop hl
	dec hl
	ld bc,(sizechange)
	dec bc
	dec bc
	dec bc
	dec bc
	dec bc
	ld (sizechange),bc
	ld bc,(bufferend)
	dec bc
	dec bc
	dec bc
	dec bc
	dec bc
	ld (bufferend),bc
	;Done...Continue for more tokens
PreParser_Restore_NextToken:
	inc hl
	push hl
	ld de,(bufferend)
	or a
	sbc hl,de
	pop hl
	jr c,PreParser_Restore_Loop
	;Update size bytes now
	ld hl,(sizebytes)
	ld c,(hl)
	inc hl
	ld b,(hl)
	push hl
	ld hl,(sizechange)
	add hl,bc
	ex hl,de
	pop hl
	ld (hl),d
	dec hl
	ld (hl),e
;	ld hl,op1backup2
;	rst 20h
	ret

Hook_Parser_NotState:
	cp a
	ret



; End of code segment based on Omnicalc v1.10
; (C) 2002-2003 Michael Vincent.
;;---------------------------------------------------------
SmartWriteback:
	b_call ChkFindSym
	ex de, hl
	call LoadCIndPaged_inc
	call LoadDEIndPaged_inc
	ld de, 6
	call BHL_Plus_DE
	call LoadCIndPaged_inc
	ld e, c
	call BHL_Plus_DE
	call LoadDEIndPaged_inc
	ex de, hl	;DE holds the location of the program in archive
	;Flash page is in B
	b_call SetupPagedPtr    ;Start a flash read session
	ld b, h;size of the asm program
	ld c, l
	push bc
	b_call OP2ToOP1
	b_call ChkFindSym
	ex de, hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	pop bc
	ld a, b
	cp d
	ret nz
	ld a, c
	cp e
	ret nz
	;location of unarchived program in hl
	or b
	ret z
cmpProgByte:
	b_call PagedGet ;have a byte from flash in A and advance internal pointers
	cp (hl)   ;Compare it with corresponding RAM byte.
	ret nz
	inc hl
	dec bc
	ld a,b
	or c     ;BC=0?
	jr nz,cmpProgByte
	;All bytes are identical. It is safe to delete the RAM program
	ret
