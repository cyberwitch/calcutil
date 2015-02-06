; Source code in this file from Noshell v1.2
; (C) 2007 by Brandon Wilson. All rights reserved.

 EXTERN iversion,irandom,isprite,ilsprite,igetpix,ifastcopy,idetect,idecomp
 EXTERN directin,sendbytetios,getbytetios,version,setvputs,setpixel,fastcopys,delayb,multhe,multhl,quittoshell,fastline,pixelonhl,pixeloff
 EXTERN pixelxor,pixeltest,pixeloffhl,pixelxorhl,pixeltesthl,fastlineb,fastlinew,fastlinex,pointonc,pointoffc,pointxorc,centertext
 EXTERN cphlbc,putsprite8,fastcopyb,vputsc,scrolld7,vnewline,rand127,disprle,cphlde,screentopic,fastlined,disprlel,getnextgoodprog
 EXTERN getprevgoodprog,getnext,getprev,compstrs,nextstr,getinfo,fastrectangle,gettext,gettextv,FastRectangle_Save,vputa,runprog,isgoodprog
 EXTERN existfold,delprog,filledrectangle,nextfolder,delfolder,moveall,curfoldname,curfoldnamea,createfolder,compstrsn,folder_menu_start
 EXTERN options_screen,put_folder_name_top,general_key_routine,find_num_good_progs,put_scrollbar,invert_lines,invert_1_line,right_align_value
 EXTERN put_mirageos_header,put_size_graphic,sendprog,hideprog,arcprog,filledrectangle_save,getbytetiosw,vatswap,renameprog,renamefolder,sysmain
 EXTERN setupint,move_gui_prog,largespritehl,Updaet_Scrollbar,Initial_Scrollbar,sortallfolds,dofoldsort,getfoldsort,setfoldsort,Increase_Cur_Element
 EXTERN Decrease_Cur_Element,Update_Scrollbar,Increase_Max_Elements,Decrease_Max_Elements,Add_A_To_Cur_Element,Sub_A_From_Cur_Element
 EXTERN Add_A_To_Max_Elements,Sub_A_From_Max_Elements,Skip_Forward_B_From_Top,Get_Curgoodprog_Ptr,getchecksum,freearc,swapram,hideall

;This is the application header definition area required for all apps.
 SEGMENT Header
;Field: Program length
	DB 080h,0Fh
;Length=0 (N/A for unsigned apps)
	DB 00h,00h,00h,00h
;Field: Program type
	DB 080h,012h
	DB 01h,04h
;Field: App ID
	DB 080h,021h
;Id = 1
	DB 01h
;Field: App Build
	DB 080h,031h
	;Build = 4
	DB 0A4h
;Field: App Name
	DB 080h,048h
;App Name = APPNAME
	DB AppName
;Field: App Pages
	DB 080h,081h
;App Pages = 1
	DB 01h
;No default splash screen
	DB 080h,090h
;Field: Date stamp - 5/12/1999
	DB 003h,026h,009h,004h,004h,06fh,01bh,080h
;Dummy encrypted TI date stamp signature
	DB 002h ,00dh ,040h                             
	DB 0a1h ,06bh ,099h ,0f6h ,059h ,0bch ,067h 
	DB 0f5h ,085h ,09ch ,009h ,06ch ,00fh ,0b4h ,003h ,09bh ,0c9h 
	DB 003h ,032h ,02ch ,0e0h ,003h ,020h ,0e3h ,02ch ,0f4h ,02dh 
	DB 073h ,0b4h ,027h ,0c4h ,0a0h ,072h ,054h ,0b9h ,0eah ,07ch 
	DB 03bh ,0aah ,016h ,0f6h ,077h ,083h ,07ah ,0eeh ,01ah ,0d4h 
	DB 042h ,04ch ,06bh ,08bh ,013h ,01fh ,0bbh ,093h ,08bh ,0fch 
	DB 019h ,01ch ,03ch ,0ech ,04dh ,0e5h ,075h 
;Field: Program Image length
	DB 80h,7Fh
	DB 0,0,0,0    ;Length=0, N/A
	DB 0,0,0,0    ;Reserved
	DB 0,0,0,0    ;Reserved
	DB 0,0,0,0    ;Reserved
	DB 0,0,0,0	;Reserved
	jp StartApp

;Ion routines
ionLibraryVectors:
       jp iversion
       jp irandom
       jp isprite
       jp ilsprite
       jp igetpix
       jp ifastcopy
       jp idetect
       jp idecomp
;MirageOS routines
       jr directin
       jr sendbytetios
       jr getbytetios
       jr version
       jr setvputs
       jr setpixel
       jr fastcopys
       jr delayb
       jr multhe
       jr multhl
       jr quittoshell
       jr fastline
       jr pixelonhl
       jr pixeloff
       jr pixelxor
       jr pixeltest
       jr pixeloffhl
       jr pixelxorhl
       jr pixeltesthl
       jr fastlineb
       jr fastlinew
       jr fastlinex
       jr pointonc
       jr pointoffc
       jr pointxorc
       jr centertext
       jr cphlbc
       jr putsprite8
       jr fastcopyb
       jr vputsc
       jr scrolld7
       jr vnewline
       jr rand127
       jr disprle
       jr cphlde
       jr screentopic
       jr fastlined
       jr disprlel
       jr getnextgoodprog
       jr getprevgoodprog
       jr getnext
       jr getprev
       jr compstrs
       jr nextstr
       jr getinfo
       jr fastrectangle
       jr gettext
       jr gettextv
       jr FastRectangle_Save
       jr vputa
       jr runprog
       jr isgoodprog
       jr existfold
       jr delprog
       jr filledrectangle
       jr nextfolder
       jr delfolder
       jr moveall
       jr curfoldname
       jr curfoldnamea
       jr createfolder
       jr compstrsn
       jr folder_menu_start
       jr options_screen
       jr put_folder_name_top
       jr general_key_routine
       jr find_num_good_progs
       jr put_scrollbar
       jr invert_lines
       jr invert_1_line
       jr right_align_value
       jr put_mirageos_header
       jr put_size_graphic
       jr sendprog
       jr hideprog
       jr arcprog
       jr filledrectangle_save
       jr getbytetiosw
       jr vatswap
       jr renameprog
       jr renamefolder
       jr sysmain
       jr setupint
       jr move_gui_prog
       jr largespritehl
       jr Update_Scrollbar
       jr Initial_Scrollbar
       jr sortallfolds
       jr dofoldsort
       jr getfoldsort
       jr setfoldsort
       jr Increase_Cur_Element
       jr Decrease_Cur_Element
       jr Increase_Max_Elements
       jr Decrease_Max_Elements
       jr Add_A_To_Cur_Element
       jr Sub_A_From_Cur_Element
       jr Add_A_To_Max_Elements
       jr Sub_A_From_Max_Elements
       jr Skip_Forward_B_From_Top
       jr Get_Curgoodprog_Ptr
       jr getchecksum
       jr freearc
       jr swapram
       jr hideall
