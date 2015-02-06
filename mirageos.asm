; Source code in this file from Noshell v1.2
; (C) 2007 by Brandon Wilson. All rights reserved.

;MirageOS routines

 include "settings.inc"
 include "ti83plus.inc"
 include "equates.inc" 
 SEGMENT Main
 GLOBALS ON

 EXTERN igetpix,ifastcopy,ifastcopy_start,isprite,irandom,ilsprite,ConfigureNoshell,runProgram
 EXTERN mirTempByte,SPbackup

xdir                 equ    iMathPtr5
ydir                 equ    iMathPtr5+1
wMirTempWord         equ    appBackUpScreen+100
timer1               equ    8A3Ah
custintaddr          equ    966Fh
curfold              equ    9683h
savesort1            equ    968Ch
savesort2            equ    968Dh
savesort3            equ    968Eh
numfolds             equ    9699h
curgoodprog          equ    969Ch
curprognum           equ    96A2h
numscrollpixels      equ    96D6h
scrollbarcoords      equ    96D7h
old_element          equ    96D9h
cur_element          equ    96DBh
old_max_elem         equ    96DDh
max_elements         equ    96DFh
rambutton            equ    9872h
ramdesc              equ    9890h
copyBuffer           equ    98B8h
bTempBuffer1         equ    98C8h
bTempBuffer2         equ    98D9h
bTempByte1           equ    98DAh
bTempByte2           equ    98DBh
wTempWord1           equ    98DCh
wTempWord2           equ    98DEh
wTempWord3           equ    98E0h
progSize             equ    993Ah
lastInsertFlag       equ    993Ch
ramsize              equ    9958h
tempVATptr           equ    995Eh
varSize              equ    9974h
numgoodprogs         equ    9991h
linkBuffer           equ    999Eh
packetData           equ    99A2h
OP1backup            equ    99A4h
checksum             equ    99ADh

directin:
       push af
       ld a,0FFh
       out (1),a
       pop af
       out (1),a
       in a,(1)
       cp 0FFh
       ret

sendbytetios:
       ld hl,$F
       call APP_PUSH_ERRORH
       B_CALL SendAByte
       call APP_POP_ERRORH
       cp a
       ret
getbytetiosw:
       ld hl,$F
       call APP_PUSH_ERRORH
       B_CALL Rec1stByteNC
       ld (mirTempByte),a
       call APP_POP_ERRORH
       ld a,(mirTempByte)
       cp a
       ret
getbytetios:
       ld hl,$F
       call APP_PUSH_ERRORH
       B_CALL RecAByteIO
       ld (mirTempByte),a
       call APP_POP_ERRORH
       ld a,(mirTempByte)
       cp a
       ret
$$:    or a
       ret

version:
       ld a,1
       ld hl,0103h
       ret

setvputs:
       ld (penCol),de
       B_CALL VPutS
       ret

pixelonhl:
       ld a,h
       ld e,l
setpixel:
       push hl
       call igetpix
       or (hl)
       ld (hl),a
       pop hl
       ret

fastcopys:
       push hl
       push de
       push bc
       push af
       call ifastcopy
       pop af
       pop bc
       pop de
       pop hl
       ret

delayb:
       ei
$$:    halt
       djnz $B
       ret

multhl:
       ld e,l
multhe:
       ld l,0
       ld d,l
       ld b,8
multheLoop:
       add hl,hl
       jr nc,$F
       add hl,de
$$:    djnz multheLoop
       ret

quittoshell:
       ld hl,(SPbackup)
       dec hl
       dec hl
       ld sp,hl
       ret

fastlinew:
       xor a
       jr fastline
fastlinex:
       ld a,2
       jr fastline
fastlineb:
       ld a,1
fastline:
       di
       cp 1
       ex af,af'
       ld a,1
       ld (xdir),a
       ld (ydir),a
       ld a,d
       sub h
       ld b,a
       jr nc,$F
       neg
       ld b,a
       ld a,0FFh
       ld (xdir),a
$$:    ld a,e
       sub l
       ld c,a
       jr nc,$F
       neg
       ld c,a
       ld a,0FFh
       ld (ydir),a
$$:    ld e,0
       ld a,b
       cp c
       jr c,fastline_1
       ld d,b
       inc b
fastline_l_1:
       call fastline_sub
       ld a,(xdir)
       add a,h
       ld h,a
       ld a,e
       add a,c
       ld e,a
       cp d
       jr c,$F
       sub d
       ld e,a
       ld a,(ydir)
       add a,l
       ld l,a
$$:    djnz fastline_l_1
       ret
fastline_1:
       ld a,b
       ld b,c
       ld c,a
       ld d,b
       inc b
fastline_l_2:
       call fastline_sub
       ld a,(ydir)
       add a,l
       ld l,a
       ld a,e
       add a,c
       ld e,a
       cp d
       jr c,$F
       sub d
       ld e,a
       ld a,(xdir)
       add a,h
       ld h,a
$$:    djnz fastline_l_2
       ret
fastline_sub:
       push hl
       exx
       ex af,af'
       jr z,fastline_sub_1
       jr c,fastline_sub_2
       push af
       ex af,af'
       pop af
       pop de
       cp 2
       jr nz,fastline_sub_3
       ld a,d
       call igetpix
       xor (hl)
       ld (hl),a
       exx
       ret
fastline_sub_3:
       ld hl,0FE70h
       rlc (hl)
       jr c,$F
       jr fastline_sub_4
fastline_sub_1:
       ex af,af'
       pop de
$$:    ld a,d
       call igetpix
       or (hl)
       ld (hl),a
       exx
       ret
fastline_sub_2:
       ex af,af'
       pop de
fastline_sub_4:
       ld a,d
       call igetpix
       cpl
       and (hl)
       ld (hl),a
       exx
       ret

pixeloffhl:
       ld a,h
       ld e,l
pixeloff:
       push hl
       call igetpix
       cpl
       and (hl)
       ld (hl),a
       pop hl
       ret

pixelxorhl:
       ld a,h
       ld e,l
pixelxor:
       push hl
       call igetpix
       xor (hl)
       ld (hl),a
       pop hl
       ret

pixeltesthl:
       ld a,h
       ld e,l
pixeltest:
       push hl
       call igetpix
       and (hl)
       pop hl
       ret

pointonc:
       cp 96
       ret nc
       bit 6,e
       ret nz
       jr setpixel

pointoffc:
       cp 96
       ret nc
       bit 6,e
       ret nz
       jr pixeloff

pointxorc:
       cp 96
       ret nc
       bit 6,e
       ret nz
       jr pixelxor

centertext:
       ld (penRow),a
       ld de,OP1+1
       push de
       B_CALL strcopy
       pop hl
       B_CALL StrLength
       dec hl
       ld (hl),c
       B_CALL SStringLength
       sra b
       ld a,48
       sub b
       ld (penCol),a
       inc hl
       B_CALL VPutS
       ret

cphlbc:
       push hl
       or a
       sbc hl,bc
       pop hl
       ret

putsprite8:
       ld b,8
       jr isprite

fastcopyb:
       ld de,02F3h
       add hl,de
       ld a,80h
       out (10h),a
       jr ifastcopy_start

vputsc:
       push hl
       ld de,(penCol)
       push de
       B_CALL VPutS
       pop de
       ld (penCol),de
       ld a,80h
       ld hl,flags+14h
       xor (hl)
       ld (hl),a
       pop hl
       B_CALL VPutS
       ld a,80h
       ld hl,flags+14h
       xor (hl)
       ld (hl),a
       ret

scrolld7:
       ld de,plotSScreen
       ld hl,9394h
       ld bc,684
       ldir
       ld hl,95ECh
       ld bc,84
       B_CALL MemClear
       jr ifastcopy

vnewline:
       xor a
       ld (penCol),a
       ld a,(penRow)
       add a,7
       cp 63
       jr z,scrolld7
       ld (penRow),a
       ret

rand127:
       push bc
       ld b,128
       call irandom
       pop bc
       ret

disprle:
       ld bc,768
disprlel:
       ld a,(hl)
       cp 145
       jr z,disprle_1
       ldi
disprle_loop:
       ret po
       jr disprlel
disprle_1:
       inc hl
       inc hl
       ld a,(hl)
$$:    dec hl
       dec a
       ldi
       jr nz,$B
       inc hl
       jr disprle_loop
       ret

cphlde:
       push hl
       or a
       sbc hl,de
       pop hl
       ret

screentopic:
       ld b,63
       ld a,7
       out (10h),a
       ld a,7Fh
screentopic_1:
       push bc
       inc a
       ld (0FE71h),a
       push af
       ld a,7
       out (10h),a
       pop af
       B_CALL lcd_busy
       out (10h),a
       ld a,20h
       B_CALL lcd_busy
       out (10h),a
       ld b,12
       B_CALL lcd_busy
       in a,(11h)
screentopic_2:
       B_CALL lcd_busy
       in a,(11h)
       ld (de),a
       inc de
       djnz screentopic_2
       pop bc
       ld a,(0FE71h)
       djnz screentopic_1
       ld a,5
       out (10h),a
       ret

fastlined:
       ld a,0AAh
       ld (0FE70h),a
       jr fastline

getnextgoodprog:
       ld (iMathPtr4),hl
$$:    call getnext
       jr z,$F
       call isgoodprog
       ret z
       jr $B
$$:    ld hl,(iMathPtr4)
       xor a
       inc a
       ret
getprevgoodprog:
       ld (iMathPtr4),hl
getprevgoodprog_loop:
       call getprev
       jr z,$B
       call isgoodprog
       ret z
       jr getprevgoodprog_loop

getnext:
       push de
       ld de,0
       call cphlde
       jr nz,$F
       ld hl,(progPtr)
       jr getnext_1
$$:    ld de,6
       sbc hl,de
       ld e,(hl)
       sbc hl,de
       dec hl
       ld de,(pTemp)
getnext_1:
       call cphlde
       pop de
       ret

getprev:
       ex de,hl
       ld hl,(progPtr)
       call cphlde
       ret z
$$:    ld (wMirTempWord),hl
       call getnext
       call cphlde
       jr nz,$B
       ld hl,(wMirTempWord)
       inc d
       ret

compstrs:
       ld a,(de)
       cp (hl)
       jr nz,nextstr
       inc hl
       inc de
       or a
       ret z
       jr compstrs

nextstr:
       push af
       push bc
       xor a
       ld b,1
       cpir
       pop bc
       pop af
       ret

FastRectangle_Save:
       push af
       push de
       push hl
       call fastrectangle
       pop hl
       pop de
       pop af
       ret

fastrectangle:
       ld c,a
       push de
       ld e,l
       call fastrectangle_sub
       pop de
       push de
       ld d,h
       call fastrectangle_sub
       pop de
       push hl
       ld h,d
       call fastrectangle_sub
       pop hl
       ld l,e
fastrectangle_sub:
       push bc
       push hl
       push de
       ld a,c
       call fastline
       pop de
       pop hl
       pop bc
       ret

vputa:
       push af
       push hl
       push af
       ld hl,(penCol)
       push hl
       B_CALL VPutMap
       pop hl
       ld (penCol),hl
       ld hl,flags+14h
       ld a,80h
       xor (hl)
       ld (hl),a
       pop af
       B_CALL VPutMap
       ld hl,flags+14h
       ld a,80h
       xor (hl)
       ld (hl),a
       pop hl
       pop af
       ret

isgoodprog:
       ld a,(hl)
       cp 6
       jr z,$F
       cp 5
       ret nz
$$:    push bc
       call dataPtrToDE
       call copy100ToMem
       pop bc
       ld de,copyBuffer+2
       ld a,(de)
       cp 0BBh
       jr nz,igp_notAsm
       inc de
       ld a,(de)
       cp 6Dh
       ret nz
       inc de
       ld a,(de)
       cp 0C9h
       jr nz,igp_noRet
       inc de
       ld a,(de)
       cp 30h
       ret z
       cp 3
       jr nz,$F
       ld a,1
$$:    or a
       ret z
       cp 1
       ret
igp_noRet:
       cp 0AFh
       ret nz
       inc de
       ld a,(de)
       cp 30h
       ret
igp_notAsm:
       cp 3Eh
       ret z
       cp 0D9h
       jr z,$F
       cp 0D5h
       ret nz
$$:    inc de
       ld a,(de)
       or a
       ret nz
       inc de
       ld a,(de)
       cp 30h
       jr z,$F
igp_end:
       cp 11h
       ret
$$:    ld a,11h
       jr igp_end
dataPtrToDE:
       push hl
       dec hl
       dec hl
       dec hl
       ld e,(hl)
       dec hl
       ld d,(hl)
       pop hl
       call getVarPage
       or a
       ret z
       push bc
       push hl
       ld hl,9
       add hl,de
       ld b,a
       B_CALL LoadCIndPaged
       ld b,0
       inc c
       add hl,bc
       ex de,hl
       pop hl
       pop bc
       ret
getVarPage:
       push hl
       dec hl
       dec hl
       dec hl
       dec hl
       dec hl
       ld a,(hl)
       pop hl
       ret
copy100ToMem:
       ld bc,100
       call getVarPage
       or a
       jr z,$F
       push hl
       ex de,hl
       ld de,copyBuffer
       B_CALL FlashToRam
       pop hl
       ret
$$:    push hl
       ex de,hl
       ld de,copyBuffer
       ldir
       pop hl
       ret

getchecksum:
       ex de,hl
       ld hl,0
$$:    ld a,(de)
       push de
       ld e,a
       ld d,0
       add hl,de
       pop de
       inc de
       dec bc
       ld a,b
       or c
       ret z
       jr $B

hideall:
       ld hl,appData
       di
       ld a,(hl)
       ld (hl),0
       res kbdSCR,(iy+kbdFlags)
       ret

delprog:
       call VATnameToOP1
       B_CALL ChkFindSym
       B_CALL DelVarArc
       ret
VATnameToOP1:
       push bc
       push de
       push hl
       ld a,(hl)
       ld (OP1),a
       or a
       ld de,6
       sbc hl,de
       ld b,(hl)
       dec hl
       ld a,5Dh
       cp (hl)
       jr z,vnto_isList
vnto_continue:
       ld de,OP1+1
$$:    ld a,(hl)
       ld (de),a
       inc de
       dec hl
       djnz $B
       xor a
       ld (de),a
       ld hl,OP1+1
       ld de,OP2
       B_CALL strcopy
       pop hl
       pop de
       pop bc
       ret
vnto_isList:
       dec b
       dec b
       dec hl
       jr vnto_continue

compstrsn:
       ld a,(de)
       cp (hl)
       ret nz
       inc hl
       inc de
       djnz compstrsn
       ret

moveall:
       ld a,(curfold)
       push af
       ld a,b
       ld (curfold),a
       ld hl,0
moveall_1:
       push bc
       call getnextgoodprog
       pop bc
       jr z,$F
       pop af
       ld (curfold),a
       ret
$$:    dec hl
       ld a,(hl)
       and 80h
       or c
       ld (hl),a
       inc hl
       jr moveall_1

Skip_Forward_B_From_Top:
       ld hl,0
$$:    push bc
       call getnextgoodprog
       pop bc
       ret nz
       djnz $B
       ret

Get_Curgoodprog_Ptr:
       ld a,(curprognum)
       ld b,a
       call Skip_Forward_B_From_Top
       ld (curgoodprog),hl
       ret

Increase_Cur_Element:
       ld a,1
Add_A_To_Cur_Element:
       ld hl,(cur_element)
       ld de,(max_elements)
       call cphlde
       ret z
       ld d,0
       ld e,a
       add hl,de
element_routine_end:
       ld (cur_element),hl
       ret
Decrease_Cur_Element:
       ld a,1
Sub_A_From_Cur_Element:
       ld hl,(cur_element)
       call HLminusA
       ret z
       jr element_routine_end
HLminusA:
       ld d,0
       ld e,a
       or a
       sbc hl,de
       ret

Increase_Max_Elements:
       ld a,1
Add_A_To_Max_Elements:
       ld hl,(max_elements)
       ld d,0
       ld e,a
       add hl,de
store_elements:
       ld (max_elements),hl
       ret
Decrease_Max_Elements:
       ld a,1
Sub_A_From_Max_Elements:
       ld hl,(max_elements)
       call HLminusA
       jr store_elements

filledrectangle_save:
       push af
       push de
       push hl
       call filledrectangle
       pop hl
       pop de
       pop af
       ret
filledrectangle_1:
       pop af
       push de
       push hl
       ld c,a
       ld a,d
       sub h
       inc a
       ld b,a
       ld d,h
$$:    push bc
       push hl
       push de
       ld a,c
       call fastline
       pop de
       pop hl
       inc d
       inc h
       pop bc
       djnz $B
       pop hl
       pop de
       ret
filledrectangle:
       push af
       ld a,e
       sub l
       inc a
       ld b,a
       ld a,d
       sub h
       inc a
       cp 10
       jr c,filledrectangle_1
       cp 16
       jr nc,filledrectangle_2
       set 7,c
       ld a,d
       and 7
       cp 7
       jr nz,filledrectangle_3
filledrectangle_2:
       ld a,h
       and 0F8h
       ld c,a
       ld a,d
       sub c
       srl a
       srl a
       srl a
       dec a
       ld c,a
filledrectangle_3:
       push de
       ld a,h
       ld e,l
       ld h,0
       ld d,h
       add hl,de
       add hl,de
       add hl,hl
       add hl,hl
       ld e,a
       srl e
       srl e
       srl e
       add hl,de
       ld de,plotSScreen
       add hl,de
       pop de
       push hl
       push bc
       ld c,d
       and 7
       ld l,a
       ld h,0
       ld de,filledrectangle_table
       add hl,de
       ld a,(hl)
       cpl
       ld b,a
       ld a,c
       and 7
       ld l,a
       ld h,0
       ld de,filledrectangle_table_2
       add hl,de
       ld a,(hl)
       ld c,a
       push bc
       pop de
       pop bc
       pop hl
       pop af
       or a
       jr z,filledrectangle_4
       dec a
       jr z,filledrectangle_5
filledrectangle_loop:
       push bc
       push hl
       ld a,(hl)
       xor d
       ld (hl),a
       inc hl
       bit 7,c
       jr nz,$F
       ld b,c
filledrectangle_loop_2:
       ld a,(hl)
       xor 0FFh
       ld (hl),a
       inc hl
       djnz filledrectangle_loop_2
$$:    ld a,(hl)
       xor e
       ld (hl),a
       pop hl
       push de
       ld de,12
       add hl,de
       pop de
       pop bc
       djnz filledrectangle_loop
       ret
filledrectangle_4:
       push bc
       push hl
       ld a,(hl)
       or d
       xor d
       ld (hl),a
       inc hl
       bit 7,c
       jr nz,$F
       ld b,c
filledrectangle_loop_3:
       ld a,(hl)
       or 0FFh
       xor 0FFh
       ld (hl),a
       inc hl
       djnz filledrectangle_loop_3
$$:    ld a,(hl)
       or e
       xor e
       ld (hl),a
       pop hl
       push de
       ld de,12
       add hl,de
       pop de
       pop bc
       djnz filledrectangle_4
       ret
filledrectangle_5:
       push bc
       push hl
       ld a,(hl)
       or d
       ld (hl),a
       inc hl
       bit 7,c
       jr nz,$F
       ld b,c
filledrectangle_loop_4:
       ld a,(hl)
       or 0FFh
       ld (hl),a
       inc hl
       djnz filledrectangle_loop_4
$$:    ld a,(hl)
       or e
       ld (hl),a
       pop hl
       push de
       ld de,12
       add hl,de
       pop de
       pop bc
       djnz filledrectangle_5
       ret
filledrectangle_table:
       DB 0
filledrectangle_table_2:
       DB 80h,0C0h,0E0h,0F0h,0F8h,0FCh,0FEh,0FFh

swapram:
       ld (0FE70h),hl
       ld (0FE72h),de
       push bc
       pop hl
swapram_loop:
       or a
       ld bc,200
       push hl
       sbc hl,bc
       jr nc,$F
       pop bc
       push bc
$$:    pop de
       push hl
       push bc
       ld hl,(0FE70h)
       ld de,0FE74h
       push bc
       ldir
       pop bc
       ld de,(0FE70h)
       ld hl,(0FE72h)
       push bc
       ldir
       pop bc
       ld (0FE70h),de
       ld de,(0FE72h)
       ld hl,0FE74h
       ldir
       ld (0FE72h),de
       pop hl
       ld bc,200
       or a
       sbc hl,bc
       pop hl
       ret nz
       jr swapram_loop

freearc:
       B_CALL ArcChk
       ld bc,(839Fh)
       ld (OP1),bc
       ld bc,(83A1h)
       ld (OP1+2),bc
       ld hl,OP3+5
       ld b,6
$$:    ld de,10
       push hl
       push bc
       B_CALL Div32By16
       pop bc
       pop hl
       ld a,(OP2+3)
       add a,30h
       ld (hl),a
       dec hl
       djnz $B
       ret

curfoldname:
       push bc
       push hl
       ld a,(curfold)
       or a
       jr z,curfoldname_1
       ld d,a
       xor a
$$:    ld b,1
       cpir
       dec d
       jr nz,$B
curfoldname_1:
       ld de,OP1
       B_CALL strcopy
       pop hl
       pop bc
       ret

curfoldnamea:
       ld b,a
       ld a,(curfold)
       push af
       ld a,b
       ld (curfold),a
       call curfoldname
       pop af
       ld (curfold),a
       ret

put_mirageos_header:
       ld hl,0202h
       ld bc,0506h
       ld ix,MirageHeaderSprite
       call largespritehl
       ld b,9
       ld hl,plotSScreen
invert_lines:
       ld c,12
$$:    ld a,(hl)
       cpl
       ld (hl),a
       inc hl
       dec c
       jr nz,$B
       djnz invert_lines
       ret
invert_1_line:
       ld b,1
       jr invert_lines
MirageHeaderSprite:
       DB 0C6h,0DEh,38h,0F7h,0C3h,8Fh,0EEh,0DBh,6Dh,86h,04h,50h,0FEh,0DEh,7Dh,0B7h
       DB 84h,4Eh,0D6h,0DBh,6Dh,0B6h,04h,41h,0C6h,0DBh,6Ch,0F7h,0C3h,9Eh,41h,49h
       DB 5Dh,49h,49h,41h,7Fh,41h,49h,49h,5Dh,49h,41h,2Ah,2Ah,14h,2Ah,2Ah,14h,14h
       DB 2Ah,14h,14h,55h,6Bh,60h,9Eh,82h,82h,82h,0FEh,60h,0FCh,0CEh,82h,82h,0FEh
       DB 82h,82h,82h,82h,82h,0FEh,80h,0C0h,0E0h,0C0h,80h

put_size_graphic:
       ld ix,SizeGraphic
       ld bc,0502h
       jr ilsprite
SizeGraphic:
       DB 68h,00h,83h,0h,49h,50h,2Ah,64h,0CBh,30h,30h,30h,30h,30h,30h

largespritehl:
       ld a,h
       jr ilsprite

find_num_good_progs:
       ld hl,0
       ld bc,0FFFFh
$$:    push bc
       call getnextgoodprog
       pop bc
       inc bc
       jr z,$B
       ld a,b
       or a
       jr nz,$F
       ld a,c
       jr find_num_good_progs_1
$$:    ld a,0FFh
find_num_good_progs_1:
       ld (numgoodprogs),a
       ret

getinfo:
       ld b,0
       call isgoodprog
       cp 30h
       jr z,getinfo_1
       cp 11h
       jr z,getinfo_2
       cp 3Eh
       jr z,getinfo_3
       push hl
       ld bc,30
       ld hl,copyBuffer+6
       ld de,rambutton
       ldir
       ld a,(copyBuffer+5)
       cp 3
       jr nz,$F
       inc hl
       inc hl
$$:    ld de,ramdesc
       B_CALL strcopy
       pop hl
       ld b,80h
       jr getinfo_5
getinfo_3:
       push hl
       pop hl
       call VATnameToOP1
       push hl
       ld hl,button_1
       ld b,1
       jr getinfo_6
getinfo_2:
       push hl
       ld hl,copyBuffer+5
       ld de,ramdesc
       B_CALL strcopy
       ld b,20h
       ld hl,button_2
       jr getinfo_6
getinfo_1:
       push hl
       ld hl,copyBuffer+7
       ld de,ramdesc
       B_CALL strcopy
       ld b,02h
       ld hl,button_3
getinfo_6:
       ld de,rambutton
       push bc
       ld bc,16
       ldir
       ld bc,14
       ld hl,button_end
       ldir
       pop bc
       pop hl
getinfo_5:
       dec hl
       ld a,(hl)
       inc hl
       and 80h
       jr z,$F
       set 4,b
$$:    call getSizeOfVATvar
       ld a,(hl)
       cp 6
       jr nz,$F
       set 2,b
$$:    call VATnameToOP1
       call getVarPage
       or a
       jr z,$F
       set 3,b
$$:    ld a,(hl)
       ld (OP1),a
       bit 0,(iy+asm_Flag1)
       ret nz
       call fillRamsize
       call parseOP2
       call getRamdescPixelWidth
       bit 0,b
       ret z
       push hl
       push de
       push bc
       ld de,ramdesc
       ld hl,sBasicProgram
       B_CALL strcopy
       ld hl,OP2
       B_CALL strcopy
       ld hl,copyBuffer+3
       ld a,(hl)
       cp 2Ah
       jr nz,getinfo_end
       inc hl
       xor a
       ld de,ramdesc
getinfo_parseLoop:
       ld b,a
       ld a,(hl)
       cp 3Fh
       jr z,getinfo_end
       ld a,b
       push de
       push hl
       push af
       B_CALL GET_TOK_STRNG
       pop af
       add a,c
       pop hl
       pop de
       cp 28h
       jr nc,getinfo_end
       push hl
       ld hl,OP3
       ldir
       pop hl
       push af
       ld a,(hl)
       inc hl
       cp 0BBh
       jr z,getinfo_good
       cp 0AAh
       jr z,getinfo_good
       cp 7Eh
       jr z,getinfo_good
       sub 5Ch
       jr c,getinfo_bad
       cp 8
       jr c,getinfo_good
getinfo_bad:
       pop af
       jr getinfo_parseLoop
getinfo_good:
       pop af
       inc hl
       jr getinfo_parseLoop
       pop af
getinfo_end:
       xor a
       ld (de),a
       pop bc
       pop de
       pop hl
       ret
getSizeOfVATvar:
       push bc
       call dataPtrToDE
       call copy100ToMem
       ld a,(hl)
       push hl
       ld hl,copyBuffer
       B_CALL DataSize
       pop hl
       pop bc
       ret
fillRamsize:
       push hl
       push af
       push bc
       push de
       xor a
       ld hl,OP2
       ld b,0FFh
       cpir
       ld de,OP1+5
       or a
       sbc hl,de
       pop de
       add hl,de
       ld (ramsize),hl
       pop bc
       pop af
       pop hl
       ret
getRamdescPixelWidth:
       push hl
       push de
       push af
       push bc
       ld hl,ramdesc
       ld b,0
grpw_loop:
       ld a,(hl)
       or a
       jr z,grpw_end
       push hl
       push bc
       B_CALL LoadPattern
       pop bc
       ld a,(hl)
       add a,b
       cp 85
       ld b,a
       pop hl
       jr c,$F
       ld (hl),0
       dec hl
$$:    inc hl
       jr grpw_loop
grpw_end:
       pop bc
       pop af
       pop de
       pop hl
       ret
parseOP2:
       push hl
       push af
       ld hl,OP2
parseOP2_loop:
       ld a,(hl)
       or a
       inc hl
       jr z,$F
       cp 7Bh
       jr nc,parseOP2_loop
       cp 61h
       jr c,parseOP2_loop
       dec hl
       sub 20h
       ld (hl),a
       inc hl
       jr parseOP2_loop
$$:    ld hl,OP2
       ld a,(hl)
       bit 4,b
       jr z,parseOP2_end
       cp 1Ch
       jr nz,$F
       ld (hl),0DCh
$$:    cp 1Ch
       jr nc,parseOP2_end
       add a,40h
       ld (hl),a
parseOP2_end:
       pop af
       pop hl
       ret
sBasicProgram:
       DB "Basic Program: ",0
button_end:
       DB 00h,00h,36h,58h,2Dh,0A0h,36h,0A8h,25h,58h
button_1:
       DB 00h,00h,00h,00h,65h,0ACh,5Bh,28h,6Eh,0A8h,5Bh,0ACh,60h,00h
button_3:
       DB 00h,00h,2Eh,0E8h,51h,14h,55h,54h,55h,54h,51h,44h,2Eh,0B8h
button_2:
       DB 00h,00h,7Fh,0FCh,47h,64h,6Ah,0B4h,6Bh,6Ch,6Ah,0A4h,6Fh,7Ch,7Fh,0FCh
dofoldsortTable:
       DB 01h,20h,02h,80h

arcprog:
       call getinfo
       B_CALL Arc_Unarc
       ret

hideprog:
       call getinfo
       dec hl
       ld a,(hl)
       and 80h
       jr z,hideprog_1
       ld a,(OP1+1)
       cp 1Ch
       jr nz,$F
       ld a,9Ch
$$:    add a,40h
       ld (OP1+1),a
       push hl
       B_CALL ChkFindSym
       pop hl
       ret nc
       ld a,(hl)
       xor 80h
       ld (hl),a
       ld de,6
       sbc hl,de
       ld a,(OP2)
       ld (hl),a
       add hl,de
       inc hl
       scf
       ret
hideprog_1:
       ld a,(OP1+1)
       cp 0DCh
       jr nz,$F
       ld a,5Ch
$$:    sub 40h
       ld (OP1+1),a
       push hl
       B_CALL ChkFindSym
       pop hl
       ret nc
       ld a,(hl)
       xor 80h
       ld (hl),a
       ld de,6
       sbc hl,de
       ld a,(OP1+1)
       ld (hl),a
       add hl,de
       inc hl
       scf
       ret

renameprog:
       ld (progSize),hl
       ld hl,OP1
       ld (hl),5
       B_CALL ChkFindSym
       ret nc
       ld hl,17
       B_CALL EnoughMem
       ccf
       ret nc
       ld hl,0
       B_CALL CreateProg
       push de
       push hl
       push hl
       ld de,OP3
       ld bc,6
       lddr
       pop de
       ld hl,(progSize)
       ld c,6
       lddr
       ld de,(progSize)
       ld hl,OP3
       ld c,6
       lddr
       pop hl
       dec hl
       bit 7,(hl)
       inc hl
       jr z,$F
       dec hl
       res 7,(hl)
       inc hl
       call hideprog
$$:    pop de
       ld hl,(progSize)
       B_CALL DelVar
       scf
       ret

put_folder_name_top:
       ld a,(curfold)
       call curfoldnamea
       ld hl,OP1
       ld e,91
$$:    inc hl
       ld a,(hl)
       or a
       jr z,$F
       ld a,e
       sub 4
       ld e,a
       jr $B
$$:    ld d,1
       ld hl,OP1
       set textInverse,(iy+textFlags)
       call setvputs
       res textInverse,(iy+textFlags)
       ret

options_screen:
       jr ConfigureNoshell

put_scrollbar:
       ld a,b
       ld c,0
       ld (numscrollpixels),a
       srl b
       jr nc,$F
       ld c,1
       inc b
$$:    push bc
       push hl
       ld a,h
       ld b,7
       ld ix,scrollSprite1
       call isprite
       pop hl
       ld a,6
       add a,l
       ld l,a
       ld (scrollbarcoords),hl
       inc l
       pop bc
$$:    push bc
       push hl
       ld ix,scrollSprite2
       ld a,h
       ld b,2
       call isprite
       pop hl
       pop bc
       inc l
       inc l
       djnz $B
       ld a,c
       or a
       jr z,$F
       dec l
       push hl
       ld ix,scrollSprite3
       ld a,h
       ld b,1
       call isprite
       pop hl
$$:    ld a,h
       ld b,7
       ld ix,scrollSprite4
       jr isprite
scrollSprite1:
       DB 41h,49h,5Dh,49h,49h,41h
scrollSprite4:
       DB 7Fh,41h,49h,49h,5Dh,49h,41h,2Ah,2Ah,14h,2Ah,2Ah,14h,14h,2Ah,14h,14h
scrollbarSprite2:
       DB 2Ah,2Ah,14h,2Ah,2Ah
scrollbarSprite1:
       DB 14h,14h,2Ah,14h,14h
scrollSprite2:
       DB 55h
scrollSprite3:
       DB 6Bh,60h,9Eh,82h,82h,82h,0FEh,60h,0FCh,0CEh,82h,82h,0FEh,82h,82h,82h,82h,82h,0FEh,80h,0C0h,0E0h,0C0h,80h

Update_Scrollbar:
       ld hl,(old_element)
       ld de,(old_max_elem)
       call scrollbar_sub
Initial_Scrollbar:
       ld hl,(cur_element)
       ld (old_element),hl
       ld de,(max_elements)
       ld (old_max_elem),de
scrollbar_sub:
       push de
       dec hl
       B_CALL SetXXXXOP2
       B_CALL OP2ToOP1
       pop hl
       call decHLKeepNonZero
       B_CALL SetXXXXOP2
       B_CALL fpdiv
       B_CALL OP1ToOP2
       ld a,(numscrollpixels)
       sub 3
       push af
       B_CALL SetXXOP1
       B_CALL FPMult
       B_CALL ConvOP1
       ld hl,(scrollbarcoords)
       ld b,5
       ld ix,scrollbarSprite1
       or a
       jr z,Initial_Scrollbar_1
       bit 0,a
       jr z,multResultIsEven
       ld ix,scrollbarSprite2
multResultIsEven:
       ld c,a
       pop af
       cp c
       push af
       ld a,c
       add a,l
       ld l,a
       pop af
       jr nz,$F
       dec b
$$:    ld a,h
       jr isprite
Initial_Scrollbar_1:
       inc a
       inc ix
       dec b
       jr multResultIsEven
decHLKeepNonZero:
       dec hl
       ld de,0
       call cphlde
       ret nz
       ld hl,1
       ret

existfold:
       B_CALL PushRealO1
       B_CALL PushRealO1
       push hl
       B_CALL PopRealO1
       pop hl
       ld a,(numfolds)
       inc a
       ld b,a
       ld de,OP1
$$:    push de
       call compstrs
       pop de
       jr z,$F
       djnz $B
       inc b
       B_CALL PopRealO1
       ret
$$:    ld a,(numfolds)
       sub b
       inc a
       B_CALL PopRealO1
returnZ:
       cp a
       ret

nextfolder:
       push hl
       ld hl,numfolds
       cp (hl)
       pop hl
       ret z
       inc a
       call curfoldnamea
       xor a
       inc a
       ld a,b
       ret

sendprog:
       di
       call sendprog_main
       ei
       ret
sendprog_main:
       ld (tempVATptr),hl
       ld de,0682h
       ld (linkBuffer),de
       ld de,11
       ld (linkBuffer+2),de
       call getSizeOfVATvar
       ld (linkBuffer+4),de
       ld (varSize),de
       push hl
       B_CALL ZeroOP1
       pop hl
       call getinfo
       ld de,OP1backup
       ld hl,OP1
       ld bc,9
       ldir
       ld hl,packetData
       ld bc,11
       call getchecksum
       ld (checksum),hl
       ld bc,17
       ld hl,linkBuffer
       call sendBCbytesFromHL
       ret nz
       ld hl,linkBuffer
       ld bc,4
       call recvBCbytesToHL
       ret nz
       ld hl,(linkBuffer)
       ld de,5682h
       call cphlde
       ret nz
$$:    in a,(4)
       bit 3,a
       jr z,recvLinkHandler
       call getbytetios
       jr nz,$B
       cp 82h
       ret nz
       ld bc,3
       ld hl,linkBuffer
       call recvBCbytesToHL
       ret nz
       ld a,(linkBuffer)
       cp 9
       ret nz
       ld hl,linkAck
       ld bc,4
       call sendBCbytesFromHL
       ret nz
       ld hl,linkData
       ld bc,2
       call sendBCbytesFromHL
       ret nz
       ld hl,(varSize)
       push hl
       call sendwordtios
       pop hl
       ret nz
       ld hl,(tempVATptr)
       call dataPtrToDE
       ex de,hl
       ld bc,(varSize)
       call getchecksum
       ld (lastInsertFlag),hl
       ld hl,(tempVATptr)
       call dataPtrToDE
       ex de,hl
       ld bc,(varSize)
       call sendBCbytesFromHL
       ret nz
       ld hl,(lastInsertFlag)
       call sendwordtios
       ret nz
       call receiveAcknowledge
       ret nz
       ld hl,linkEOT
       ld bc,4
       call sendBCbytesFromHL
       ret nz
receiveAcknowledge;
       ld hl,linkBuffer
       ld bc,4
       call recvBCbytesToHL
       ret nz
       ld hl,(linkBuffer)
       ld de,5682h
       jr cphlde
recvLinkHandler:
       or 1
       ret
sendBCbytesFromHL:
       push bc
       push hl
       ld a,(hl)
       call sendbytetios
       pop hl
       pop bc
       ret nz
       inc hl
       dec bc
       ld a,b
       or c
       ret z
       jr sendBCbytesFromHL
recvBCbytesToHL:
       push bc
       push hl
       call getbytetios
       pop hl
       pop bc
       ret nz
       ld (hl),a
       inc hl
       dec bc
       ld a,b
       or c
       ret z
       jr recvBCbytesToHL
sendwordtios:
       ld a,l
       push hl
       call sendbytetios
       pop hl
       ret nz
       ld a,h
       jr sendbytetios
linkAck:
       DB 82h,56h,00h,00h
linkContinue:
       DB 82h,09h,00h,00h
linkData:
       DB 82h,15h
linkEOT:
       DB 82h,92h,00h,00h

vatswap:
       call cphlde
       jr nc,$F
       ex de,hl
$$:    push de
       ld (wTempWord1),hl
       push hl
       ld de,-6
       add hl,de
       ld a,(hl)
       neg
       ld e,a
       add hl,de
       ld (wTempWord2),hl
       ex de,hl
       pop hl
       sbc hl,de
       ld a,l
       inc a
       inc a
       ld (bTempByte1),a
       ld hl,(wTempWord1)
       ld de,bTempBuffer1
       ld b,0
       ld c,a
       lddr
       pop hl
       ld (wTempWord3),hl
       push hl
       ld de,-6
       add hl,de
       ld a,(hl)
       neg
       ld e,a
       add hl,de
       pop de
       ex de,hl
       sbc hl,de
       ld a,l
       inc a
       inc a
       ld (bTempByte2),a
       ld hl,(wTempWord3)
       ld de,bTempBuffer2
       ld b,0
       ld c,a
       lddr
       ld a,(bTempByte2)
       ld b,a
       ld a,(bTempByte1)
       cp b
       jr z,vatswap_1
       jr nc,vatswap_2
       sub b
       neg
       push af
       ld hl,(wTempWord3)
       inc hl
       push hl
       ld d,0FFh
       neg
       ld e,a
       add hl,de
       pop de
       push de
       push hl
       ld hl,(wTempWord2)
       or a
       sbc hl,de
       ld b,h
       ld c,l
       pop de
       pop hl
       ld a,b
       or c
       jr z,$F
       inc bc
       ldir
$$:    ld hl,(wTempWord1)
       ex de,hl
       ld hl,bTempBuffer2
       ld a,(bTempByte2)
       ld b,0
       ld c,a
       lddr
       ld hl,(wTempWord3)
       pop af
       neg
       ld b,0FFh
       ld c,a
       add hl,bc
       ld de,(bTempBuffer1)
       ex de,hl
       ld a,(bTempByte1)
       ld b,0
       ld c,a
       lddr
       ret
vatswap_1:
       ld hl,(wTempWord1)
       ex de,hl
       ld hl,bTempBuffer2
       ld a,(bTempByte1)
       ld b,0
       ld c,a
       lddr
       ld hl,(wTempWord3)
       ex de,hl
       ld hl,bTempBuffer1
       ld a,(bTempByte1)
       ld b,0
       ld c,a
       lddr
       ret
vatswap_2:
       sub b
       push af
       ld hl,(wTempWord2)
       dec hl
       push hl
       ld d,0
       ld e,a
       add hl,de
       pop de
       push de
       push hl
       ld hl,(wTempWord3)
       or a
       ex de,hl
       sbc hl,de
       ld b,h
       ld c,l
       pop de
       pop hl
       ld a,b
       or c
       jr z,$F
       inc bc
       lddr
$$:    ld hl,(wTempWord1)
       ex de,hl
       ld hl,bTempBuffer2
       ld a,(bTempByte2)
       ld b,0
       ld c,a
       lddr
       ld hl,(wTempWord3)
       pop af
       ld b,0
       ld c,a
       add hl,bc
       ex de,hl
       ld hl,bTempBuffer1
       ld a,(bTempByte1)
       ld b,0
       ld c,a
       lddr
       ret

getfoldsort:
       call getfoldsort_1
       inc b
       dec b
       jr z,getfoldsort_end
       sla b
$$:    rrca
       djnz $B
getfoldsort_end:
       and 3
       ret
getfoldsort_1:
       cp 8
       jr c,$F
       and 1
       ld b,a
       ld hl,savesort3
       ld a,(hl)
       ret
$$:    cp 4
       jr c,$F
       sub 4
       ld b,a
       ld hl,savesort2
       ld a,(hl)
       ret
$$:    ld b,a
       ld hl,savesort1
       ld a,(hl)
       ret

setfoldsort:
       call getfoldsort_1
       inc b
       dec b
       jr z,setfoldsort_end
       sla b
       ld a,3
$$:    rlc c
       rlca
       djnz $B
setfoldsort_end:
       cpl
       and (hl)
       ld (hl),a
       ld a,c
       or (hl)
       ld (hl),a
       ret

dofoldsort:
       ld b,a
       ld a,(curfold)
       push af
       ld a,b
       ld (curfold),a
       or a
       push af
       call z,dofoldsort_1
       pop af
       dec a
       push af
       call z,dofoldsort_2
       pop af
       dec a
       push af
       call z,dofoldsort_3
       pop af
       dec a
       call z,dofoldsort_4
       pop af
       ld (curfold),a
       ret
dofoldsort_1:
       ld hl,OP2
       ld (progSize),hl
       jr dofoldsort_2_loop2
dofoldsort_2:
       ld hl,ramdesc
       ld (progSize),hl
dofoldsort_2_loop2:
       ld hl,0
dofoldsort_2_loop:
       call getnextgoodprog
       ret nz
       call getinfo
       push hl
       ld hl,(progSize)
       ld de,OP3
       B_CALL strcopy
       pop hl
       ld (lastInsertFlag),hl
       call getnextgoodprog
       ret nz
       call getinfo
       push hl
       ld hl,OP3
       ld de,(progSize)
       call compstrs
       pop hl
       jr c,$F
       ld hl,(lastInsertFlag)
       jr dofoldsort_2_loop
$$:    ld de,(lastInsertFlag)
       call vatswap
       jr dofoldsort_2_loop2
dofoldsort_3:
       ld hl,0
dofoldsort_3_loop:
       call getnextgoodprog
       ret nz
       call getinfo
       ld (lastInsertFlag),hl
       ld de,(ramsize)
       ld (progSize),de
       call getnextgoodprog
       ret nz
       call getinfo
       push hl
       ld hl,(progSize)
       ld de,(ramsize)
       call cphlde
       pop hl
       jr c,$F
       ld hl,(lastInsertFlag)
       jr dofoldsort_3_loop
$$:    ld de,(lastInsertFlag)
       call vatswap
       jr dofoldsort_3
dofoldsort_4:
       ld hl,0
dofoldsort_4_loop:
       call getnextgoodprog
       ret nz
       call getinfo
       ld (lastInsertFlag),hl
       call dofoldsort_4_sub
       ld (progSize),a
       call getnextgoodprog
       ret nz
       call getinfo
       call dofoldsort_4_sub
       ld b,a
       ld a,(progSize)
       cp b
       jr c,$F
       ld hl,(lastInsertFlag)
       jr dofoldsort_4_loop
$$:    ld de,(lastInsertFlag)
       call vatswap
       jr dofoldsort_4
dofoldsort_4_sub:
       push hl
       ld a,b
       and 0E3h
       ld hl,dofoldsortTable
       push bc
       cpir
       pop hl
       or a
       sbc hl,bc
       ld a,l
       inc a
       pop hl
       ret

sortallfolds:
       ld a,(numfolds)
       ld b,a
       xor a
$$:    push af
       push bc
       call dofoldsort
       pop bc
       pop af
       cp b
       ret z
       inc a
       jr $B

right_align_value:
       push de
       push bc
       ld hl,ravTable1
       bit 7,(iy+asm_Flag1)
       jr z,$F
       ld hl,ravTable2
$$:    or a
       jr z,$F
       ld b,a
       cp 6
       jr c,ravLoop
$$:    ld b,5
ravLoop:
       dec b
       jr z,$F
       dec hl
       ld a,e
       sub 4
       ld e,a
       jr ravLoop
$$:    call VPutSDE
       pop de
       ld a,d
       or e
       jr z,ravSetB1
       ld hl,10000
       ld b,5
$$:    dec hl
       call cphlde
       inc hl
       jr c,ravContinue
       push bc
       push de
       B_CALL DivHLBy10
       pop de
       pop bc
       djnz $B
ravContinue:
       push de
       pop hl
       pop de
$$:    dec b
       jr z,$F
       ld a,e
       sub 4
       ld e,a
       jr $B
$$:    ld (penCol),de
       B_CALL SetXXXXOP2
       B_CALL OP2ToOP1
       ld a,5
       B_CALL DispOP1A
       ret
ravTable1:
       DB 0,6,6,6,6,6
ravTable2:
       DB 0,78h,0DCh,0BCh,0FCh,0FCh,78h
ravSetB1:
       ld b,1
       jr ravContinue
VPutSDE:
       ld (penCol),de
VPutSDE_loop:
       ld a,(hl)
       inc hl
       or a
       ret z
       cp 0DEh
       jr nc,$F
       B_CALL VPutMap
       jr nc,VPutSDE_loop
       ret
$$:    push hl
       ld hl,stringTable
       sub 0DDh
       ld b,a
       ld d,0
$$:    ld e,(hl)
       add hl,de
       djnz $B
       ld b,(hl)
       dec b
$$:    inc hl
       ld a,(hl)
       B_CALL VPutMap
       djnz $B
       pop hl
       jr VPutSDE_loop
stringTable:
       DB 2," "
       DB 7,"Tasker"
       DB 8,"Confirm"
       DB 7,"Prompt"
       DB 5,"tion"
       DB 7,"Protec"
       DB 9,"Interfac"
       DB 7,"Enable"
       DB 5,"Move"
       DB 10,"Number 0f"
       DB 15,"Miscellaneous "
       DB 5,"Prog"
       DB 9,"Mirage0S"
       DB 8,"0Ptions"
       DB 9,"Password"
       DB 9,"Key Hook"
       DB 6,"Enter"
       DB 8,"Select "
       DB 6,"Trans"
       DB 6,"Clear"
       DB 6,"Reset"
       DB 5,"File"
       DB 5,"Main"
       DB 3,"0N"
       DB 7,"System"
       DB 5,"Name"
       DB 13,"Please Wait."
       DB 12,"Delete This"
       DB 5,"Sort"
       DB 5,"Dele"
       DB 2," "
       DB 7,"Folder"
       DB 8,"Program"
       DB 3,"  "
       DB 4,"   "

setupint:
       di
       ld hl,interrupt1
       ld de,8A4Fh
       ld bc,interrupt1_end-interrupt1 ;55
       ldir
       ld hl,interrupt2
       ld de,8A8Ah
       ld bc,interrupt2_end-interrupt2 ;114
       ldir
       ld hl,interrupt3
       ld de,8C01h
       ld bc,interrupt3_end-interrupt3 ;27
       ldir
       ld bc,05FFh
       ld de,8A9Dh
       ld hl,interrupt4
setupint_loop:
       rra
       jr nc,$F
       push hl
       push de
       ldi
       ldi
       ldi
       pop de
       pop hl
$$:    inc hl
       inc hl
       inc hl
       inc de
       inc de
       inc de
       djnz setupint_loop
       ex de,hl
       rra
       jr nc,$F
       ld (hl),0CDh
       inc hl
       ld de,(custintaddr)
       ld (hl),e
       inc hl
       ld (hl),d
$$:    ld a,72
       ld (apdTimer),a
       ld hl,8B00h
       ld a,h
       ld i,a
       dec a
       ld bc,257
       B_CALL MemSet
       ld hl,statVars
       xor a
       ld (hl),a
       inc hl
       inc hl
       ld (hl),a
       inc hl
       inc hl
       ld (hl),a
       im 2
       ei
       ret
interrupt1:
       xor a
       ld (1),a
       in a,(1)
       inc a
       jr z,$F
       ld a,72
       ld (apdTimer),a
$$:    ld a,(apdSubTimer)
       dec a
       ld (apdSubTimer),a
       ret nz
       dec a
       ld (apdSubTimer),a
       ld a,(apdTimer)
       dec a
       ld (apdTimer),a
       ret nz
       ld a,72
       ld (apdTimer),a
       set 1,(iy+asm_Flag1)
       set 2,(iy+asm_Flag1)
       ld a,1
       out (3),a
       ex af,af'
       exx
       ei
       halt
       ret
interrupt1_end:
interrupt2:
       di
       ex af,af'
       exx
       ld hl,flags+asm_Flag1
       bit 2,(hl)
       res 2,(hl)
       jr nz,jp003Ah
       ld b,1
$$:    B_CALL CpHLDE
       djnz $B
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
       nop
jp003Ah:
       exx
       ex af,af'
       jp 0038h
interrupt2_end:
interrupt3:
       ld hl,timer1
       ld a,(hl)
       inc a
       ld (hl),a
       inc hl
       cp (hl)
       ret nz
       dec hl
       ld (hl),0
       inc hl
       inc hl
       ld a,(hl)
       inc a
       ld (hl),a
       inc hl
       cp (hl)
       ret nz
       dec hl
       ld (hl),0
       inc hl
       inc hl
       inc (hl)
       ret
interrupt3_end:
interrupt4:
       call 8C01h
       call 8AB2h
       call 8A4Fh
       call 8ABCh
       call 8AF3h

runprog:
;NOTE! This isn't the real implementation of runprog, this is using the parser hook to do its thing.
       dec hl
       dec hl
       dec hl
       dec hl
       dec hl
       dec hl
       ld b,(hl)
       ld de,OP1
$$:    inc de
       dec hl
       ld a,(hl)
       ld (de),a
       djnz $B
       inc de
       xor a
       ld (de),a
       ld (basic_prog+1),a
       ld a,ProgObj
       ld (OP1),a
       call runProgram
       ret nz
       ld hl,$F
       call APP_PUSH_ERRORH
       set 1,(iy+08h)
       B_CALL ParseInp
       call APP_POP_ERRORH
$$:    ret

gettext:
gettextv:
delfolder:
createfolder:
folder_menu_start:
general_key_routine:
renamefolder:
sysmain:
move_gui_prog:
       or 1
       scf
       ret

