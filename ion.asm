; Source code in this file from Noshell v1.2
; (C) 2007 by Brandon Wilson. All rights reserved.

;Ion routines

 include "settings.inc"
 include "ti83plus.inc"
 include "equates.inc" 
 SEGMENT Main
 GLOBALS ON

 EXTERN randData

iversion:
       xor a
       ld hl,0106h
       ld de,8
       ret

irandom:
       push hl
       push de
       ld hl,(randData)
       ld a,r
       ld d,a
       ld e,(hl)
       add hl,de
       add a,l
       xor h
       ld (randData),hl
       sbc hl,hl
       ld e,a
       ld d,h
$$:    add hl,de
       djnz $B
       ld a,h
       pop de
       pop hl
       ret

isprite:
       ld e,l
       ld h,0
       ld d,h
       add hl,de
       add hl,de
       add hl,hl
       add hl,hl
       ld e,a
       and 7
       ld c,a
       srl e
       srl e
       srl e
       add hl,de
       ld de,plotSScreen
       add hl,de
putSpriteLoop1:
sl1:   ld d,(ix)
       ld e,0
       ld a,c
       or a
       jr z,putSpriteSkip1
putSpriteLoop2:
       srl d
       rr e
       dec a
       jr nz,putSpriteLoop2
putSpriteSkip1:
       ld a,(hl)
       xor d
       ld (hl),a
       inc hl
       ld a,(hl)
       xor e
       ld (hl),a
       ld de,11
       add hl,de
       inc ix
       djnz putSpriteLoop1
       ret

ilsprite:
       di
       ex af,af'
       ld a,c
       push af
       ex af,af'
       ld e,l
       ld h,0
       ld d,h
       add hl,de
       add hl,de
       add hl,hl
       add hl,hl
       ld e,a
       and 7
       ld c,a
       srl e
       srl e
       srl e
       add hl,de
       ld de,plotSScreen
       add hl,de
largeSpriteLoop1:
       push hl
largeSpriteLoop2:
       ld d,(ix)
       ld e,0
       ld a,c
       or a
       jr z,largeSpriteSkip1
largeSpriteLoop3:
       srl d
       rr e
       dec a
       jr nz,largeSpriteLoop3
largeSpriteSkip1:
       ld a,(hl)
       xor d
       ld (hl),a
       inc hl
       ld a,(hl)
       xor e
       ld (hl),a
       inc ix
       ex af,af'
       dec a
       push af
       ex af,af'
       pop af
       jr nz,largeSpriteLoop2
       pop hl
       pop af
       push af
       ex af,af'
       ld de,12
       add hl,de
       djnz largeSpriteLoop1
       pop af
       ret

igetpix:
       ld d,0
       ld h,d
       ld l,e
       add hl,de
       add hl,de
       add hl,hl
       add hl,hl
       ld de,plotSScreen
       add hl,de
       ld b,0
       ld c,a
       and 00000111b
       srl c
       srl c
       srl c
       add hl,bc
       ld b,a
       inc b
       ld a,00000001b
getPixelLoop:
       rrca
       djnz getPixelLoop
       ret
       
ifastcopy:
;       di
       ld a,80h
       out (10h),a
       ld hl,plotSScreen-12-(-(12*64)+1)
ifastcopy_start:
       ld a,20h
       ld c,a
       inc hl
       dec hl
fastCopyAgain:
       ld b,64
       inc c
       ld de,-(12*64)+1
       out (10h),a
       add hl,de
       ld de,10
fastCopyLoop:
       add hl,de
       inc hl
       inc hl
       inc de
       ld a,(hl)
       out (11h),a
       dec de
       djnz fastCopyLoop
       ld a,c
       cp 2Ch
       jr nz,fastCopyAgain
       ret

idetect:
       ld de,(pTemp)
       B_CALL CpHLDE
       ld a,(hl)
       jr nz,detectContinue
       inc a
       ret
detectContinue:
       push hl
       and 1
       jr nz,detectSkip
       dec hl
       dec hl
       dec hl
       ld e,(hl)
       dec hl
       ld d,(hl)
       dec hl
       ld a,(hl)
       or a
       push af
       ld h,d
       ld l,e
       jr z,detectNoMove
       push hl
       B_CALL MemChk
       ld bc,64
       sbc hl,bc
       pop hl
       jr c,detectNotEnough
       ld de,(tempMem)
       push ix
       push hl
       push de
       B_CALL FlashToRam
       pop hl
       push hl
       pop ix
       ld a,10
       add a,(ix+9)
       ld e,a
       ld d,0
       add hl,de
       ex (sp),hl
       add hl,de
       pop de
       ex de,hl
       pop ix
detectNoMove:
       inc de
       inc de
       ld c,(hl)
       inc hl
       ld b,(hl)
       inc hl
       push bc
       push ix
       pop bc
detectCheck:
       ld a,(bc)
       or a
       jr z,detectFound
       cp (hl)
       inc bc
       inc de
       inc hl
       jr z,detectCheck
detectBad:
       pop bc
detectNotEnough:
       pop af
detectSkip:
       pop hl
       ld bc,-6
       add hl,bc
       ld b,(hl)
       dec hl
detectNameLoop2:
       dec hl
       djnz detectNameLoop2
       jr idetect
detectFound:
       pop hl
       pop af
       jr z,detectInRam
       push de
       push af
       push hl
       B_CALL EnoughMem
       pop bc
       jr c,detectBad
       pop af
       pop hl
       ld de,(tempMem)
       push de
       B_CALL FlashToRam
       pop de
detectInRam:
       pop hl
       ld bc,-6
       add hl,bc
       ld b,(hl)
       inc b
detectNameLoop1:
       dec hl
       djnz detectNameLoop1
       ex de,hl
       xor a
	ret

idecomp:
       di
decompressLoop:
       push bc
       ld a,(hl)
       ex af,af'
       ld a,c
       ld b,8
       cp 1
       jr z,dcmp1
       ld b,4
       cp 3
       jr z,dcmp1
       ld b,2
dcmp1:
       push bc
       ld a,c
       ld b,1
       cp 1
       jr z,dcmp2
       inc b
       cp 3
       jr z,dcmp2
       ld b,4
dcmp2:
       ex af,af'
dcmp3:
       rlca
       djnz dcmp3
       ld b,a
       ex af,af'
       ld a,b
       and c
       ld (de),a
       inc de
       pop bc
       djnz dcmp1
       inc hl
       pop bc
       djnz decompressLoop
       ret
