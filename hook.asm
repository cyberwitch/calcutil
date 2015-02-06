; Source code in this file from Noshell v1.2
; (C) 2007 by Brandon Wilson. All rights reserved.

 include "settings.inc"
 include "ti83plus.inc"
 include "equates.inc" 
 SEGMENT Main
 GLOBALS ON

 EXTERN BHL_plus_DE,libraryVectors,bNameBackup,iversion,irandom,isprite,ilsprite,igetpix,ifastcopy,idetect,idecomp,getNoshellSettings
 EXTERN wProgSize,wByteCount,wProgBytes,wProgOffset,wDataPtr,bDataPage,wTempWord,SPbackup,wUserMemPtr,tempByte,inc_BHL,flagByte,oldHookBlock

Abackup              equ    0FE72h
HLbackup             equ    Abackup+1
DEbackup             equ    HLbackup+3
BCbackup             equ    DEbackup+5
hOP1backup           equ    BCbackup+7
basicProgBackup      equ    hOP1backup+11
tempHookBlock        equ    0FFFAh


runProgram:
       ld a,(basic_prog)
       cp ProgObj
       jr z,$F
       cp ProtProgObj
       jr nz,exitParserHook
$$:    B_CALL ChkFindSym
       ex de,hl
;       ld a,(basic_prog+1)
;       cp '#'
;       jr z,homescreenProg
       ld a,b
       or a
       jr z,readProgramData
       ld de,9
       call BHL_plus_DE
       call getDataByte
       ld c,a
$$:    call getDataByte
       dec c
       jr nz,$B
readProgramData:
       call getDataByte
       ld e,a
       call getDataByte
       ld d,a
       ld (wProgSize),de
       ld (wByteCount),de
$$:    ld (wDataPtr),hl
       ld a,b
       ld (bDataPage),a
       call getDataByte
       jr c,exitParserHook
       cp 0BBh
       jr nz,basicProgram
       call getDataByte
       jr c,exitParserHook
       cp 6Ch
       jr z,unsquishedProgram
       cp 6Dh
       jr nz,basicProgram
;assembly program of some sort
       call getDataByte
       jr c,exitParserHook
       cp 0AFh
       jr z,ionProgramNoLibs
       cp 0C9h
       jr nz,squishedAssemblyProgram
       call getDataByte
       jr c,exitParserHook
       cp 30h
       jr z,ionProgramLibs
       cp 3
       jr z,mirageOSWithQuit
       dec a
       jr z,mirageOSNormal
squishedAssemblyProgram:
       ld a,b
       or a
       jr nz,archivedSquishedAssemblyProgram
;*** SQUISHED ASSEMBLY PROGRAM ***
       ld hl,progExecErrorHandler
       call APP_PUSH_ERRORH
       B_CALL ExecutePrgm
       call APP_POP_ERRORH
       or 1
       ret
;*** ARCHIVED SQUISHED ASSEMBLY PROGRAM ***
archivedSquishedAssemblyProgram:
       ld hl,progExecErrorHandler
       call APP_PUSH_ERRORH
       call allocateTempProg
       ld hl,(wProgSize)
       B_CALL CreateProtProg
       inc de
       inc de
       ld hl,(wDataPtr)
       ld a,(bDataPage)
       ld bc,(wProgSize)
       B_CALL FlashToRam
       B_CALL OP4ToOP1
       B_CALL ExecutePrgm
       call deleteMostRecentTempProg
       call APP_POP_ERRORH
       or 1
       ret
exitParserHook:
       xor a
       ret
progExecErrorHandler:
       res 7,a
       push af
       call deleteAllTempProgs
       pop af
       B_JUMP JError
throwMemErr:
       B_JUMP ErrMemory
basicProgram:
       ld a,b
       or a
       jr nz,archivedBasicProgram
       set 1,(iy+8)
       xor a
       ret
archivedBasicProgram:
;*** ARCHIVED BASIC PROGRAM ***
       set 1,(iy+8) ;This is necessary to stop the parser from throwing ERR:INVALID sometimes
                    ;It might be to tell the parser we're in a program and it's okay to handle programming tokens like "Disp "
       ld hl,progExecErrorHandler
       call APP_PUSH_ERRORH
       call allocateTempProg
       ld hl,(wProgSize)
       B_CALL CreateProtProg
       jr c,throwMemErr
       inc de
       inc de
       xor a
       ld (basic_prog),a
       ld (basic_prog+1),a
       ld hl,(wDataPtr)
       ld a,(bDataPage)
       ld bc,(wProgSize)
       B_CALL FlashToRam
       B_CALL OP4ToOP1
       B_CALL ParseInp
       call deleteMostRecentTempProg
       call APP_POP_ERRORH
       or 1
       ret
;*** UNSQUISHED ASSEMBLY PROGRAM ***
unsquishedProgram:
       jr squishedAssemblyProgram
;       ld a,b
;       or a
;       jr nz,archivedUnsquishedProgram
;       or 1
;       ret
;*** ARCHIVED UNSQUISHED ASSEMBLY PROGRAM ***
;archivedUnsquishedProgram:
;       or 1
;       ret
ionProgramNoLibs:
       jr ionProgramLibs
;       ld a,b
;       or a
;       jr nz,archivedIonProgramNoLibs
;*** ION PROGRAM WITHOUT LIBRARIES ***
;       B_CALL RunIndicOff
;       ld hl,ionLibraryVectors
;       ld de,libraryVectors ;some people are dumb and put this header anyway
;       ld bc,24
;       ldir
;       ld de,bNameBackup
;       B_CALL MovFrOP1
;       ld hl,progExecErrorHandler
;       call APP_PUSH_ERRORH
;       call loadProgram
;       xor a
;       call userMem+1
;       call unloadProgram
;       call APP_POP_ERRORH
;       call cleanUpIonMirageProg
;       or 1
;       ret
;*** ARCHIVED ION PROGRAM WITHOUT LIBRARIES ***
;archivedIonProgramNoLibs:
;       jr ionProgramLibs
ionProgramLibs:
       ld a,b
       or a
       jr nz,archivedIonProgramLibs
;*** ION PROGRAM WITH LIBRARIES ***
       call prepareForIonMirageProg
       ld de,bNameBackup
       B_CALL MovFrOP1
       ld hl,progExecErrorHandler
       call APP_PUSH_ERRORH
       call loadProgram
       xor a
       call userMem+1
       call unloadProgram
       call APP_POP_ERRORH
       call cleanUpIonMirageProg
       or 1
       ret
;*** ARCHIVED ION PROGRAM WITH LIBRARIES ***
archivedIonProgramLibs:
       ld hl,progExecErrorHandler
       call APP_PUSH_ERRORH
       ld de,bNameBackup
       B_CALL MovFrOP1
       call prepareForIonMirageProg
       call getNoshellSettings
       ld a,(flagByte)
       and 80h
       jr nz,archivedIonWithWriteback
       ld hl,(wProgSize)
       dec hl
       dec hl
       ld (wProgBytes),hl
       B_CALL EnoughMem
       jr c,throwMemErr
       ld hl,(wProgBytes)
       ld de,userMem
       B_CALL InsertMem
       ld hl,(wDataPtr)
       ld a,(bDataPage)
       ld b,a
       ld de,2
       call BHL_plus_DE
       ld a,b
       ld de,userMem
       ld bc,(wProgBytes)
       B_CALL FlashToRam
       xor a
       call userMem+1
       ld de,(wProgBytes)
       ld hl,userMem
       B_CALL DelMem
       call APP_POP_ERRORH
       call cleanUpIonMirageProg
       or 1
       ret
;*** ARCHIVED ION PROGRAM WITH WRITEBACK ***
archivedIonWithWriteback:
       call APP_POP_ERRORH
       ld hl,archivedErrorHandler
       call APP_PUSH_ERRORH
       ld hl,bNameBackup
       rst 20h
       B_CALL ChkFindSym
       B_CALL Arc_Unarc
       ld hl,bNameBackup
       rst 20h
       B_CALL ChkFindSym
       call loadProgram
       xor a
       call userMem+1
       call unloadProgram
       call cleanUpIonMirageProg
       ld hl,bNameBackup
       rst 20h
       B_CALL ChkFindSym
       B_CALL Arc_Unarc
       call APP_POP_ERRORH
       or 1
       ret
archivedErrorHandler:
       res 7,a
       push af
       ld hl,bNameBackup
       rst 20h
       B_CALL ChkFindSym
       ld a,b
       or a
       jr z,$F
       B_CALL Arc_Unarc
$$:    pop af
       B_JUMP JError
;*** MIRAGEOS PROGRAM WITH QUIT ROUTINE ***
mirageOSWithQuit:
       ld de,34
       ld (wTempWord),de
       jr mirageOSRun
;       ld a,b
;       or a
;       jr nz,archivedMirageOSWithQuit
;       or 1
;       ret
;*** ARCHIVED MIRAGEOS PROGRAM WITH QUIT ROUTINE ***
;archivedMirageOSWithQuit:
;       or 1
;       ret
;*** MIRAGEOS PROGRAM WITHOUT QUIT ROUTINE ***
mirageOSNormal:
       ld de,32
       ld (wTempWord),de
mirageOSRun:
       ld a,b
       or a
       jr nz,archivedMirageOSNormal
       ld de,bNameBackup
       B_CALL MovFrOP1
       call prepareForIonMirageProg
       ld hl,progExecErrorHandler
       call APP_PUSH_ERRORH
       call loadProgram
       ld (SPbackup),sp
       ld hl,userMem
       ld de,(wTempWord)
       add hl,de
       ld bc,128
       xor a
       cpir
       ld de,mirReturnPoint
       push de
       jp (hl)
mirReturnPoint:
       im 1
       call unloadProgram
       call APP_POP_ERRORH
       call cleanUpIonMirageProg
       or 1
       ret
;*** ARCHIVED MIRAGEOS PROGRAM WITHOUT QUIT ROUTINE ***
archivedMirageOSNormal:
       ld hl,progExecErrorHandler
       call APP_PUSH_ERRORH
       ld de,bNameBackup
       B_CALL MovFrOP1
       call getNoshellSettings
       call prepareForIonMirageProg
       ld a,(flagByte)
       and 80h
       jr nz,archivedMirageOSWithWriteback
       ld hl,(wProgSize)
       dec hl
       dec hl
       ld (wProgBytes),hl
       B_CALL EnoughMem
       jr c,throwMemErr
       ld hl,(wProgBytes)
       ld de,userMem
       B_CALL InsertMem
       ld hl,(wDataPtr)
       ld a,(bDataPage)
       ld b,a
       ld de,2
       call BHL_plus_DE
       ld a,b
       ld de,userMem
       ld bc,(wProgBytes)
       B_CALL FlashToRam
       ld (SPbackup),sp
       ld hl,userMem
       ld de,(wTempWord)
       add hl,de
       ld bc,128
       xor a
       cpir
       ld de,mirArcReturnPoint
       push de
       jp (hl)
mirArcReturnPoint:
       im 1
       ld de,(wProgBytes)
       ld hl,userMem
       B_CALL DelMem
       call APP_POP_ERRORH
       call cleanUpIonMirageProg
       or 1
       ret
archivedMirageOSWithWriteback:
       call APP_POP_ERRORH
       ld hl,archivedErrorHandler
       call APP_PUSH_ERRORH
       ld hl,bNameBackup
       rst 20h
       B_CALL ChkFindSym
       B_CALL Arc_Unarc
       ld hl,bNameBackup
       rst 20h
       B_CALL ChkFindSym
       call loadProgram
       ld (SPbackup),sp
       ld hl,userMem
       ld de,(wTempWord)
       add hl,de
       ld bc,128
       xor a
       cpir
       ld de,mirArcWritebackReturnPoint
       push de
       jp (hl)
mirArcWritebackReturnPoint:
       im 1
       call unloadProgram
       call cleanUpIonMirageProg
       ld hl,bNameBackup
       rst 20h
       B_CALL ChkFindSym
       B_CALL Arc_Unarc
       call APP_POP_ERRORH
       or 1
       ret
prepareForIonMirageProg:
       B_CALL ClrLCDFull
       B_CALL GrBufClr
       B_CALL RunIndicOff
       set graphDraw,(iy+graphFlags)
       res textEraseBelow,(iy+textFlags)
       res textWrite,(iy+sGrFlags)
       res appTextSave,(iy+appFlags)
;       set write_on_graph,(iy+sGrFlags)
       ld hl,4083h
       ld de,libraryVectors
       ld bc,24
       ldir
       im 1
       ret
cleanUpIonMirageProg:
       im 1
       ld a,6
       out (4),a
       set graphDraw,(iy+graphFlags)
       B_CALL GrBufClr
       B_CALL ClrScrn
       ld hl,textShadow
       ld (hl),' '
       ld de,textShadow+1
       ld bc,127
       ldir
       ld (iy+sGrFlags),0
       ret



getDataByte:
       ld a,b
       or a
       ld a,(hl)
       inc hl
       call nz,$F
       ld (tempByte),a
       push de
       ld de,(wByteCount)
       dec de
       ld (wByteCount),de
       inc de
       ld a,d
       or e
       pop de
       ld a,(tempByte)
       ret nz
       scf
       ret
$$:    dec hl
       push bc
       ld a,b
       B_CALL GetBytePaged
       ld a,b
       pop bc
       call inc_BHL
       ret

allocateTempProg:
       ld hl,sTempProgName
       rst 20h
$$:    B_CALL ChkFindSym
       ccf
       ret nc
       ld a,(OP1+5)
       dec a
       scf
       ret z
       ld (OP1+5),a
       jr $B
deleteMostRecentTempProg:
       ld hl,sTempProgName
       rst 20h
       ld a,1
       ld (OP1+5),a
$$:    B_CALL ChkFindSym
       jr nc,$F
       ld a,(OP1+5)
       inc a
       scf
       ret z
       ld (OP1+5),a
       jr $B
$$:    B_CALL DelVarArc
       xor a
       ret
deleteAllTempProgs:
       call deleteMostRecentTempProg
       ret c
       jr deleteAllTempProgs
sTempProgName:
       DB ProtProgObj,12h,34h,56h,78h,0FFh,0

loadProgram:
       ld de,userMem
       ld (wUserMemPtr),de
       ld hl,(wProgSize)
       dec hl
       dec hl
       ld (wProgSize),hl
       ld (wProgBytes),hl
       set swapMax,(iy+noshellFlags)
loadProgramLoop:
       ld bc,768
       ld hl,(wProgSize)
       or a
       sbc hl,bc
       ret z
       jr nc,$F
       ld bc,(wProgSize)
       res swapMax,(iy+noshellFlags)
$$:    ld (wProgSize),hl
       push bc
       ld hl,bNameBackup
       rst 20h
       B_CALL ChkFindSym
       inc de
       inc de
       inc de
       inc de
       ex de,hl
       pop bc
       push hl
       ld de,appBackUpScreen
       push bc
       ldir
       pop de
       pop hl
       push de
       B_CALL DelMem
       pop hl
       push hl
       ld de,(wUserMemPtr)
       B_CALL InsertMem
       pop bc
       ld de,(wUserMemPtr)
       ld hl,appBackUpScreen
       ldir
       ld (wUserMemPtr),de
       bit swapMax,(iy+noshellFlags)
       ret z
       jr loadProgramLoop
unloadProgram:
       set swapMax,(iy+noshellFlags)
       ld hl,(wProgBytes)
       ld (wProgSize),hl
       ld hl,0
       ld (wProgOffset),hl
unloadProgramLoop:
       ld bc,768
       ld hl,(wProgSize)
       or a
       sbc hl,bc
       ret z
       jr nc,$F
       ld bc,(wProgSize)
       res swapMax,(iy+noshellFlags)
$$:    ld (wProgSize),hl
       ld hl,userMem
       ld de,appBackUpScreen
       push bc
       ldir
       pop de
       ld hl,userMem
       push de
       B_CALL DelMem
       ld hl,bNameBackup
       rst 20h
       B_CALL ChkFindSym
       inc de
       inc de
       inc de
       inc de
       ld hl,(wProgOffset)
       add hl,de
       ex de,hl
       pop hl
       push hl
       push de
       B_CALL InsertMem
       pop de
       pop bc
       ld hl,appBackUpScreen
       push bc
       ldir
       pop bc
       ld hl,(wProgOffset)
       add hl,bc
       ld (wProgOffset),hl
       bit swapMax,(iy+noshellFlags)
       ret z
       jr unloadProgramLoop
