; Source code in this file from Noshell v1.2
; (C) 2007 by Brandon Wilson. All rights reserved.

Var MACRO first, second
 segment MemVars
&first:
       ds     second
 segment Main
 ENDMAC

noshellFlags         equ    asm_Flag1
swapMax              equ    0
avarArchived         equ    1
