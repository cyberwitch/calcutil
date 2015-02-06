; CalcUtil v1.08
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
 EXTERN tempHookBlock, runProgram

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
	bit getKeyBit, (iy + getKeyHookFlag)
	jr nz, ShowWarning
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








VarGone:
	call NewAppVar										;Delete and make a new UtilVar






	bit parserBit, (IY + parserHookFlag)		;Try to save a current parser hook
	jr z, NoParserHook
	ld hl, parserHookPtr + 2
	ld a, (hl)
	ld b, a
	in a, (6)
	cp b
	jr z, NoParserHook



	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, parserHookPtr
	ld bc, 3
	inc de
	inc de
	ldir
	jr NoParserHook


NoParserHook:										;Install our parser hook
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, ParserHook
	in a, (6)
	b_call EnableParserHook
	xor a
	ld (parserHookPtr + 3), a








NoOldRawKeyHook:
	bit rawKeyBit, (IY + rawKeyHookFlag)		;Try to save the current raw key hook
	jr z, NoRawKeyHook
	ld hl, rawKeyHookPtr + 2
	ld a, (hl)
	ld b, a
	in a, (6)
	cp b
	jr z, NoRawKeyHook



	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld bc, 3
	ld hl, 17
	add hl, de
	ex de, hl
	ld hl, rawKeyHookPtr
	ldir
	jr NoRawKeyHook


NoRawKeyHook:											;Install our raw key hook
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, RawKeyHook
	in a, (6)
	b_call EnableRawKeyHook
	xor a
	ld (rawKeyHookPtr + 3), a



	ld hl, AppChangeHook								;Install AppChangeHook
	in a, (6)
	b_call EnableAppChangeHook
	xor a
	ld (appChangeHookPtr + 3), a

	b_call DisableGetKeyHook

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

ShowWarning:	
	ld hl, 0
	ld (curRow), hl
	ld hl, WarningText
	call PutSApp
	b_call GetKey
	cp kEnter
	jp nz, StartApp
	jp InstallHooks2

UninstallHooks:											;Uninstall all those hooks
	b_call DisableParserHook
	b_call DisableAppChangeHook
	b_call DisableRawKeyHook
	b_call DisableGetKeyHook
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
	call CheckNoshellAppVar
	b_call ClrLCDFull
	ld hl, 0
	ld (curRow), hl
	ld hl, Settings
	set textInverse, (IY + textFlags)
	call PutSApp
	call CheckNoshellAppVar
	cp 0
	jr nz, WritebackOn
	res textInverse, (IY + textFlags)
WritebackOn:
	b_call NewLine
	ld a, '_'
	b_call PutC
	res textInverse, (IY + textFlags)
	ld hl, Writeback
	call PutSApp
MenuLoop2:
	res indicOnly, (IY + indicFlags)
	b_call GetCSC
	res onInterrupt, (IY + onFlags)
	cp skMode
	jr z, StartApp
	cp skClear
	jr z, StartApp
	cp skEnter
	jr z, ToggleWriteback
	cp sk2nd
	jr z, ToggleWriteback
	jr MenuLoop2

ToggleWriteback:
	call CheckNoshellAppVar
	cp 0
	jr nz, SetWriteback
	ld (hl), 80h
	jr SettingsMenu
SetWriteback:
	ld (hl), 0
	jr SettingsMenu


CheckNoshellAppVar:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NoshellUnarc
	b_call Arc_Unarc
NoshellUnarc:
	ld hl, 16
	add hl, de
	ld a, (hl)
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
	xor a
	ld (AppBackUpScreen+10), a
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, Create
	push hl
	push de
	ld hl, 16
	add hl, de
	ld a, (hl)
	ld (AppBackUpScreen+10), a
	pop de
	pop hl
	b_call DelVarArc
	ld hl, AppVarName
	rst rMov9ToOP1
Create:
	ld hl, 30
	b_call CreateAppVar
	push de	
	inc de
	inc de
	xor a
	ld b, 30
clearloop:
	ld (de), a
	djnz clearloop
	ld a, (AppBackUpScreen+10)
	pop de
	ld hl, 16
	add hl, de
	ld (hl), a
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
			
WarningText:
	db " Warning! Hooks "
	db "  exist! Press  "
	db "    Enter to    "
	db "   attempt to   "
	db " chain hooks or "
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
	db "CalcUtil v1.08", 0
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




ParserHook:
	db 83h
;	push af
;	push bc
;	push de
;	push hl


; in a,(2)
; rla
; sbc a,a
; out (20h),a

	;ld hl, AppVarName
	;rst rMov9ToOP1
	;b_call ChkFindSym
	;call c, NewAppVar
	;xor a
	;cp b
	;jr z, NotArc16
	;b_call Arc_Unarc
;NotArc16:
	;ld hl, 21
	;add hl, de
	;ld a, (hl)
	;cp 1
	;jr nz, ContParser
	;pop hl
	;pop de
	;pop bc
	;pop af
	;xor a
	;cp 1
	;ret
;ContParser:

;	pop hl
;	pop de
;	pop bc
;	pop af

	or a
	jr nz,CheckFn										;Handle functions separately from variables


	
	
	ld de, parseVar									;Only handle the prgm# variable (contains homescreen's input before execution, so no archived errors)
	inc de
	ld a, (de)
	cp '#'
	jr nz, ReturnZ	





	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NotArc15
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc15:
	ld hl, 15
	add hl, de
	ld (hl), 0
	ld de, 6
	add hl, de
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
	jr z, CheckArchivedRun






	cp t2ByteTok
	jr nz, ReturnZ
	inc de
	ld a, (de)
	cp tasm												;If the first tokens are Asm(prgm, skip the Asm and continue
	jr nz, ReturnZ
	inc de
	ld a, (de)
	cp tProg
	jr z, CheckArchivedRunAsm

ReturnZ:
	cp a
	ret

CheckFn:													;If log( w/ two parameters, do log(p1)/log(p2);  handles complex #s better than Omnicalc's log(
	push hl
	ld hl, 0C0C0h
	sbc hl, bc
	jr nz, ArcTok
	pop hl
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
	ld hl, 5B01h
	sbc hl, bc
	jr nz, UnarcTok
	pop hl
	push af
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
	scf
	ccf
	ld hl, 5C01h
	sbc hl, bc
	jr nz, StopTok
	pop hl
	push af
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
	scf
	ccf
	ld hl, 0B01h
	sbc hl, bc
	jr nz, RealTok
	pop hl
	call FixAllPrograms
	call CleanNoFix
	b_call DispDone
	B_JUMP JForceCmdNoChar


CleanNoFix:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, ReturnZPopAll
	xor a
	cp b
	jr z, NotArc33
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc33:
	ld hl, 5
	add hl, de
	xor a
	ld (hl), a																				;Sets appvar+5 to 0.  Flag of something...program running?
	ld de, 16
	add hl, de
	ld a, (hl)
	ld hl, tempProgName
	push af
	rst rMov9ToOP1
	pop af
DeleteLoop:
	ld (OP1+8), a
	b_call ChkFindSym
	jr c, LoopCont
	b_call DelVarArc
LoopCont:
	ld a, (OP1+8)
	cp 0
	jr z, QuitHook3
	dec a
	ld (OP1+8), a
	jr DeleteLoop
QuitHook3:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NotArccc
	b_call Arc_Unarc
	b_call ChkFindSym
NotArccc:
	ld hl, 21
	add hl, de
	ld (hl), 0
	res progExecuting, (iy + newDispF)
	res cmdexec, (iy + cmdFlags)
	ret

RealTok:																		;Handles prgm tokens converted to real(42," so that subprograms are handled properly
	scf
	ccf
	ld hl, 8A8Ah
	sbc hl, bc
	jr nz, ChainParser
	pop hl
	push hl
	push af
	ld a, l
	cp 2
	jr nz, ChainParserA
	xor a
	cp h
	jr nz, ChainParserA
	push bc
	push de
	b_call OP1ToOP6
	b_call PopRealO1
	b_call OP1ToOP5
	b_call CkPosInt
	jr nz, ChainParserReload
	b_call ConvOP1
	cp 42
	jr z, YesReal
	cp 43
	jr nz, ChainParserReload
YesReal:
	pop de
	pop bc
	pop af
	pop hl
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

	call CheckArchivedRunAlreadyInOP1
	jr z, ThrowError
	res numOP1, (iy + ParsFlag2)
	xor a
	cp 1
	ret

ThrowError:
	ld a, E_Undefined
	b_call JError

ChainParserReload:
	b_call OP5ToOP1
	b_call PushRealO1
	b_call OP6ToOP1
	pop de
	pop bc
ChainParserA:
	pop af
ChainParser:																		;Restores registers and passes onto the chained app's hook
	push af
	push bc
	b_call PushRealO1
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, PopLots
	xor a
	cp b
	jr z, NotArc
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc:
	push hl
	push de
	b_call PopRealO1
	pop de
	pop hl
	pop bc
	inc de
	inc de
	ex de, hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	xor a
	cp d
	jr z, PopAFReturnZ
	inc hl
	ld a, (hl)
	push de
	push bc
	ld hl, JumpToOldHook
	ld de, AppBackUpScreen
	ld bc, EndJump - JumpToOldHook
	ldir
	jp AppBackUpScreen

PopAFReturnZ
	pop af
	pop hl
	jr ReturnZ
	
PopLots:
	push hl
	push de
	b_call PopRealO1
	pop de
	pop hl
	pop bc
	pop af
	pop hl
	jr ReturnZ


JumpToOldHook:
	out (6), a
	pop bc
	pop hl
	ld a, (hl)
	cp 83h
	jr nz, RetZ
	inc hl
	ex de, hl
	ld ix, 0
	add ix, de
	pop af
	pop hl
	jp (ix)
RetZ:
	pop af
	pop hl
	cp a
	ret
EndJump:



CheckArchivedRunAsm:															;Deals with skipping the Asm( token
	dec bc
	dec bc
	push de
	pop hl
	add hl, bc
	ld a, (hl)
	cp tRParen
	jr nz, CheckArchivedRun
	dec bc
CheckArchivedRun:																;Copies name to OP1
	inc de
	b_call ZeroOP1
	ex de, hl
	ld a, ProgObj
	ld (op1), a
	ld de, op1 + 1
	ldir
CheckArchivedRunAlreadyInOP1:												;Backs up name to OP6
	b_call OP1ToOP6
	b_call ChkFindSym
	jr nc, UnarchiveCheck
	ld a, ProtProgObj															;Finds proper program name
	ld (op1), a
	b_call OP1ToOP6
	b_call ChkFindSym
	jr c, ReturnZ
UnarchiveCheck:
	xor a
	ld (appBackUpScreen), a
	cp b
	jr z, RunPrgm																;If program is in RAM, go to RunPrgm

	ex de, hl																	;Otherwise, unarchive
	call LoadCIndPaged_inc
	call LoadDEIndPaged_inc
	ld de, 6
	call BHL_Plus_DE
	call LoadCIndPaged_inc
	ld e, c
	call BHL_Plus_DE
	call LoadDEIndPaged_inc

	xor a
	cp d
	jr nz, ContinueCopying
	cp e
	jr nz, ContinueCopying
	cp 1
	ret
ContinueCopying:
	b_call ChkFindSym
	call CopyPrgmToRam
	b_call OP1ToOP6															;Backs up new temp program name to OP6
	ld a, 1																		;1 at AppBackUpScreen means program was archived
	ld (appBackUpScreen), a

RunPrgm:																			;Determines if program is asm or not, and passes to the right routine
	b_call ChkFindSym
	inc de
	inc de
	ld a, (de)
	cp t2ByteTok
	jr nz, RunBasicPrgm
	inc de
	ld a, (de)
	cp tasmPrgm
	jr z, RunAsmPrgm
	cp tasmCmp
	jr z, RunAsmPrgm

RunBasicPrgm:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NotArc2
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc2:
	ld hl, 5
	add hl, de
	ld a, 1
	ld (hl), a																;Sets appvar+5 to 1.  Flag of something...program running?
	ld de, 10
	add hl, de
	ld a, (hl)
	inc a
	ld (hl), a
	ld de, -9
	add hl, de


	ex de, hl
	push de
	b_call OP3ToOP1																;Copies program name to the appvar+6
	pop de
	ld hl, OP1+1
	ld bc, 8
	ldir

	b_call OP6ToOP1

	call PreParser



	;set 7,(iy+28h)
	set progExecuting, (iy + newDispF)
	set cmdexec, (iy + cmdFlags)
;;;;;;;;;;;;;;;;;;;	b_call PushRealO1
	call PushRealO1
	jr c, ErrorHandler
	ld hl, ErrorHandler
	call 59h

	in a,(2)
	rla
	sbc a,a
	out (20h),a

	b_call ParseInp																		;Run program.  No error handlers so the default handler will allow goto
	call 5Ch
;;;;;;;;;;;;;;;;;;;	b_call PopRealO1
	call PopRealO1
	b_call ChkFindSym
	ld hl, OP1
	ld de, tempProgName
	ld b, 8
	call CompareStrings
	jr nz, NotArchivedCopy2
	b_call ChkFindSym
	b_call DelVarArc
	jr ResetFlags
NotArchivedCopy2:
	call PreParser_Restore
ResetFlags:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, ReturnZPopAll
	xor a
	cp b
	jr z, NotArc3
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc3:
	ld hl, 5
	add hl, de
	xor a
	ld (hl), a																				;Sets appvar+5 to 0.  Flag of something...program running?
	ld de, 10
	add hl, de
	ld a, (hl)
	dec a
	ld (hl), a
	ld de, 6
	add hl, de
	ld a, (hl)
	cp 1
	jr nz, NotFirstPrgm
	res progExecuting, (iy + newDispF)
	res cmdexec, (iy + cmdFlags)
NotFirstPrgm:
	ld hl, tempProgName
	push af
	rst rMov9ToOP1
	pop af
	ld (OP1+8), a
	b_call ChkFindSym
	jr c, NotFound
	b_call DelVarArc
NotFound:
	ld a, (OP1+8)
	dec a
	push af
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	call c, NewAppVar
	xor a
	cp b
	jr z, NotArcc
	b_call Arc_Unarc
	b_call ChkFindSym
NotArcc:
	ld hl, 21
	add hl, de
	pop af
	ld (hl), a
QuitHook2:
	xor a
	cp 1
	ret







PrgmPound:
	db ProgObj, "#", 0

Noshell:
	db appobj, "Noshell "

RunAsmPrgm:
	ld hl, OP1
	ld de, tempProgName
	ld b, 8
	call CompareStrings
	jr nz, NotArchivedCopy3
	b_call ChkFindSym
	b_call DelVarArc
	b_call OP3ToOP1
NotArchivedCopy3:
	ld hl, basic_prog
	b_call Mov9ToOP2
	b_call PushRealO2
	ld de, basic_prog
	b_call MovFrOP1
	call runProgram
	b_call PopRealO1
	ld de, basic_prog
	b_call MovFrOP1
	xor a
	cp 1
	ret






ErrorHandler:
	call FixAllPrograms
	b_call JErrorNo



FixAllPrograms:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, ReturnZPopAll
	xor a
	cp b
	jr z, NotArc5
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc5:
	ld hl, 15
	add hl, de
	ld b, (hl)
	xor a
	cp b
	ret z
	ld (hl), a
FixLoop:
	push bc
;;;;;;;;;;;;;;;;;;	b_call PopRealO1
	call PopRealO1
	call PreParser_Restore
	pop bc
	djnz FixLoop
	ret


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





AppChangeHook:
	db 83h
	;set 7,(iy+28h)
	push af
	push bc
	push de
	push hl
	b_call DisableGetKeyHook
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, ContAppChange
	xor a
	cp b
	jr z, NotArc4
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc4:
	ld hl, 20
	add hl, de
	ld a, (hl)
	cp 42
	jr z, ContCheckHome
ContAppChange:
	pop hl
	pop de
	pop bc
	pop af
	cp kPrgmEd
	jr nz, CheckHome
	set 7,(iy+28h)
	push af
	push bc
	push de
	push hl
	ld a, progobj
	ld (OP1), a
	ld hl, progToEdit
	ld de, OP1+1
	ld bc, 8
	ldir
	b_call PushRealO1
	b_call ChkFindSym
	jr c, ReturnZPopAllO1
	xor a
	cp b
	jr z, ReturnZPopAllO1
	call CopyPrgmToRam
	ld hl, OP1+1
	ld de, progToEdit
	ld bc, 8
	ldir
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 5
	add hl, de
	ld a, 1
	ld (hl), a
	inc hl
	ex de, hl
	push de
	b_call PopRealO1
	pop de
	ld hl, OP1+1
	ld bc, 8
	ldir
	jr ReturnZPopAll

ReturnZPopAllO1:
	b_call PopRealO1
ReturnZPopAll:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, ReturnZPopAllNoAppVar
	xor a
	cp b
	jr z, NotArc6
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc6:
	ld hl, 14
	add hl, de
	ld a, (hl)
	cp 42
	jr nz, ReturnZPopAllNoAppVar
	xor a
	ld (hl), a
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	b_call PowerOff
ReturnZPopAllNoAppVar:
	pop hl
	pop de
	pop bc
	pop af
	jr ReturnZ

_PowerOff equ 5008h



TurningOff:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	ld hl, 5
	add hl, de
	xor a
	cp (hl)
	ret z
	ld (hl), a
	call FixAllPrograms
	call CleanNoFix
	jr ReturnZ



CheckHome:
	cp kQuit
	jr z, CheckHomeYes
	cp kYequ
	jr z, CheckHomeYes
	cp kWindow
	jr z, CheckHomeYes
	cp kGraph
	jr z, CheckHomeYes
	cp kTable
	jr z, CheckHomeYes
	cp kLinkIO
	jr z, CheckHomeYes
	cp kAppsMenu
	jr z, CheckHomeYes
	jr ReturnZ
CheckHomeYes:
	push af
	push bc
	push de
	push hl
	ld a, b
	cp kPrgmEd
	jr nz, CleanStuff
	ld hl, textShadow+8
	ld a, ProgObj
	ld (OP6), a
	ld de, OP6+1
	ld bc,8
	ldir
	ld hl, textShadow+8
	ld de, tempProgName+1
	ld b, 7
	call CompareStrings
	jr nz, ContCheckHome
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, CleanStuff
	xor a
	cp b
	jr z, NotArc7
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc7:
	ld hl, 5
	add hl, de
	ld a, 1
	ld (hl), a
ContCheckHome:
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, CleanStuff
	xor a
	cp b
	jr z, NotArc8
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc8:
	ld hl, 5
	add hl, de
	ld a, (hl)
	cp 1
	jr nz, CleanStuff




	xor a
	ld (hl), a
	ld a, progobj
	ld (OP1), a
	inc hl
	ld de, OP1+1
	ld bc, 8
	ldir




;	ld hl, AppVarName
;	rst rMov9ToOP1
;	b_call ChkFindSym
;	ld hl, 22
;	add hl, de
;	ld de, OP1+1
;	ld b, 8
;	ldir





Continue:

	b_call ChkFindSym
	jr c, CleanStuff
	xor a
	cp b
	jr z, CleanStuff
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
	jr z, CleanStuff
	cp skTrace
	jr z, CleanStuff
	cp skWindow
	jr z, QuitSaveLoop
	cp skYEqu
	jr z, QuitSaveLoop
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, ContReturnToEd
	xor a
	cp b
	jr z, NotArc9
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc9:
	ld hl, 14
	add hl, de
	xor a
	ld (hl), a
ContReturnToEd:
	ld hl, OP6+1
	ld de, progToEdit
	ld bc, 8
	ldir

	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, NoVar
	xor a
	cp b
	jr z, NotArcb
	b_call Arc_Unarc
	b_call ChkFindSym
NotArcb:
	ld hl, 5
	add hl, de
	ld (hl), 1
	ld de, 15
	add hl, de
	ld (hl), 0
NoVar:
	ld a, kPrgmEd
	b_call NewContext0

	set 7, (iy + 28h)
	b_call Mon
QuitSaveLoop:
	;b_call PopRealO1
	;b_call PushRealO1
	b_call ChkFindSym
	jr c, CleanStuff
	xor a
	cp b
	jr z, CleanStuff
	;b_call ChkFindSym
	push af
	push bc
	push de
	push hl
	b_call OP1ToOP3
	pop hl
	pop de
	pop bc
	pop af
	b_call DelVarArc
	ld hl, OP6
	rst rMov9ToOP1
	b_call ChkFindSym
	ld a, (de)
	ld l, a
	inc de
	ld a, (de)
	ld h, a
	push hl
	b_call OP3ToOP1
	pop hl
	b_call CreateProg
	push de
	b_call OP6ToOP1
	b_call ChkFindSym
	ld a, (de)
	ld c, a
	inc de
	ld a, (de)
	ld b, a
	pop hl
	inc hl
	inc hl
	inc de
	ex de, hl
	xor a
	cp c
	jr nz, ContinueNewProg
	cp b
	jr z, SkipNewProgData
ContinueNewProg:
	ldir
SkipNewProgData:
	b_call OP3ToOP1
	b_call Arc_Unarc
CleanStuff:
	call CleanNoFix
	jr ReturnZPopAll


ReturnZPopAllPlus:
	pop de
	b_call PopRealO1
	jr ReturnZPopAll

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
	push af
	push bc
	push de
	push hl

;	ld hl, AppVarName
;	rst rMov9ToOP1
;	b_call ChkFindSym
;	call c, NewAppVar
;	xor a
;	cp b
;	jr z, NotArc17
;	b_call Arc_Unarc
NotArc17:
;	ld hl, 21
;	add hl, de
;	ld (hl), 0


	b_call DisableGetKeyHook
	pop hl
	pop de
	pop bc
	pop af
	
	;set 7,(iy+28h)
	ld (AppBackUpScreen), a
;	ld a, (MenuCurrent)
;	cp 31h
;	jr z, Transmit
;	ld a, (AppBackUpScreen)
	cp kOff
	jr z, ContRawKey
	cp kQuit
	jr z, Quitting
	cp kAppsMenu
	jr z, App2Fin
	cp kMem
	jr nz, CheckNum
BlockKey:
	ld a, (cxCurApp)
	cp kPrgmEd
	jr nz, ReplaceARetNZ
	xor a
	ret
App2Fin:
	ld a, (cxCurApp)
	cp kPrgmEd
	jr nz, ChainReload
	ld a, kFin
	cp 1
	ret
ReplaceARetNZ:
	ld a, (AppBackUpScreen)
	cp a
	ret
ContRawKey:
	ld a, (cxCurApp)
	cp kPrgmEd
;	jr nz, ChainReload
	jr nz, TurningOff
	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, ContRawKey2
	xor a
	cp b
	jr z, NotArc10
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc10:
	ld hl, 14
	add hl, de
	ld a, 42
	ld (hl), a
ContRawKey2:
	ld a, kQuit
	cp 1
	;res 7,(iy+28h)
	ret



Startup:
	ld a, tS + 5Eh
	jr NoOnNecessary


CheckNum:
	cp k1
	jr c, ChainRawKey
	cp k9 + 1
	jr nc, ChainRawKey

	ld (AppBackUpScreen), a

	push af
	in a, (4)
	bit 3, a
	jr nz, ChainRawKeyPop


	ld hl, AppVarName
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, QuitHook2
	xor a
	cp b
	jr z, NotArc11
	b_call Arc_Unarc
	b_call ChkFindSym
NotArc11:
	ld hl, 20
	add hl, de
	ld (hl), 42

	;ld a, (cxCurApp)
	;cp kPrgmEd
	;jr z, PopAReturnZ

	ld a, kQuit
	b_call NewContext0

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
	ld hl, 20
	add hl, de
	ld (hl), 0

	pop af

NoOnNecessary:
	ld (AppBackUpScreen), a
	b_call DisableGetKeyHook
	ld hl, PROGLIST
	rst rMov9ToOP1
	b_call ChkFindSym
	jr c, NoMatch
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
	call CleanNoFix
	ld a, kEnter
	b_call cxMain
	b_call Mon




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
	call CleanNoFix
;	ld a, (AppBackUpScreen)
;	add 5Eh
	cp a
	ret





PROGLIST:
	db progobj, "PROGLIST"


basic_end equ 965Fh

Quitting:
	ld a, (cxCurApp)
	cp kPrgmEd
	call nz, TurningOff
	jr ChainReload



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
	ld hl, 17
	add hl, de
	ld e, (hl)
	inc hl
	ld d, (hl)
	xor a
	cp d
	jr z, PopAReturnZ
	inc hl
	ld a, (hl)
	push de
	ld hl, JumpToOldHook2
	ld de, AppBackUpScreen
	ld bc, EndJump2 - JumpToOldHook2
	ldir
	jp AppBackUpScreen

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


Transmit:
	ld a, (AppBackUpScreen)
	cp k1
	jr z, ContTransmit
	cp kEnter
	jr nz, ReturnZ
ContTransmit:
	res 0, (iy+1bh)
	res 1, (iy+1bh)
	res 2, (iy+1bh)
	res 3, (iy+1bh)
	ld a, 0Ah
	ld (sndRecState), a
	ld hl, PROGLIST
	rst rMov9ToOP1
	b_call SendVariable
	xor a
	cp 1
	ret

	ld a, kLinkIO
	call SendGetKeyPress
	b_call Get4Bytes
	b_call Get4Bytes
	ld a, kRight
	call SendGetKeyPress
	b_call Get4Bytes
 	b_call Get4Bytes
	ld a, k1
	call SendGetKeyPress
	b_call Get4Bytes
	b_call SendVariable
	ld a, (AppBackUpScreen)
	cp 0
	ret

_SendVariable equ 4F15h

SendGetKeyPress:
	push af
	ld a, 83h
	b_call SendAByte
	ld a, 87h
	b_call SendAByte
	pop af
	b_call SendAByte
	xor a
	b_call SendAByte
	ret



_Get4Bytes equ 4EF4h




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
	ld hl, 21
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









GetKeyHook:
	db 83h
	ld (AppBackUpScreen), a
	b_call DisableGetKeyHook
	call FixAllPrograms
	call CleanNoFix
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





AppVarStart:
;	ld hl, tempProgName2 - AppVarStart + 8001h
;	rst rMov9ToOP1
;	b_call ChkFindSym
;	jr c, QuitHook4
;	b_call DelVarArc
;	ret
;QuitHook4:
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
       ld hl, 16
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
	jr nz,PreParser_Restore_NextToken
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
	jr nz,PreParser_Restore_NextToken
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
	jr z, NotAsm
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
NotAsm:
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