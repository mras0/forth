ExecBase                EQU   $4

; exec.library
_LVOCloseLibrary	EQU -414
_LVOOpenLibrary	        EQU -552

; dos.library
_LVOOpen		EQU -30
_LVOClose		EQU -36
_LVORead   	        EQU -42
_LVOWrite		EQU -48
_LVOInput               EQU -54
_LVOOutput              EQU -60

MODE_OLDFILE		EQU 1005
;MODE_NEWFILE		EQU 1006
;MODE_READWRITE		EQU 1004

NUM_CELLS equ 4096
RSIZE     equ 128
S0        set Cells+NUM_CELLS*4
R0        set Cells+RSIZE*4

F_HIDDEN  EQU $80
F_IMMED   EQU $40
F_LENMASK EQU $1F

O_RDONLY  EQU MODE_OLDFILE

NEXT    macro
        move.l  (a5)+, a0
        move.l  (a0), a1
        jmp     (a1)
        endm

        SECTION code, CODE
Main:
        movem.l d0/a0, -(sp) ; Save command line arguments

        ; Open dos.library
        move.l  ExecBase.w, a6
        lea     DosName, a1
        moveq   #0, d0
        jsr     _LVOOpenLibrary(a6)
        move.l  d0, a6
        move.l  d0, DosBase

        ; Get stdout
        jsr     _LVOOutput(a6)
        move.l  d0, Stdout

        ; Get stdin
        jsr     _LVOInput(a6)
        move.l  d0, Stdin
        move.l  d0, InputFile   ; default to stdin

        ; Parse arguments
        movem.l (sp)+, d0/a0
.SkipSpace:
        tst.l   d0
        beq.s   .HasFile
        subq.l  #1, d0
        move.b  (a0)+, d1
        cmp.b   #' ', d1
        bls.s   .SkipSpace
        cmp.b   #255, d1
        beq.s   .HasFile
        subq.l  #1, a0
        move.l  a0, d1
.FindEnd:
        move.b  (a0)+, d2
        cmp.b   #255, d2
        beq.s   .HasEnd
        cmp.b   #' ', d2
        bhi.s   .FindEnd
.HasEnd:
        move.b  #0, -1(a0)

        move.l  #O_RDONLY, d2
        jsr     _LVOOpen(a6)
        move.l  d0, InputFile

.HasFile:
        move.l  sp, OrigSP
        move.l  #.Start, a5
        move.l  #R0, a6
        move.l  #S0, a7
        NEXT
.Start: align   2
        dc.l    QUIT

WriteFile:
        move.l a6, -(sp)
        move.l DosBase, a6
        jsr _LVOWrite(a6)
        move.l (sp)+, a6
        rts

Emit:
        move.b  d0, -(sp)
        move.l  Stdout, d1
        move.l  sp, d2
        moveq   #1, d3
        jsr     WriteFile
        addq.l  #2, sp
        rts

ReadKey:
        movem.l d1-d3/a0-a1/a6, -(sp)
        subq.l  #2, sp
        move.l  InputFile, d1
        move.l  sp, d2
        moveq.l #1, d3
        move.l  DosBase, a6
        jsr     _LVORead(a6)
        tst.l   d0
        beq.s   .Done
        move.b  (sp), d0
        cmp.b   #'a', d0
        bcs.s   .Done
        cmp     #'z', d0
        bhi.s   .Done
        and     #$df, d0
.Done:
        addq.l  #2, sp
        movem.l (sp)+, d1-d3/a0-a1/a6
        and.b   d0, d0
        rts

; Read word to A0/D0
ReadWord:
.SkipSpaces:
        move.l  #WordBuffer, a0
        move.l  a0, a1
        jsr     ReadKey
        beq.s   .Done
        cmp.b   #' ', d0
        bls.s   .SkipSpaces
        cmp.b   #'\\', d0
        bne.s   .ReadLoop
.SkipToNL:
        jsr     ReadKey
        beq.s   .Done
        cmp.b   #10, d0
        bne.s   .SkipToNL
        bra.s   .SkipSpaces
.ReadLoop:
        move.b  d0, (a0)+
        jsr     ReadKey
        beq.s   .Done
        cmp.b   #' ', d0
        jhi.s   .ReadLoop
.Done:
        move.b  #0, (a0) ; NUL-terminate
        sub.l   a1, a0
        move.l  a0, d0
        move.l  a1, a0
        rts

; Find word in A0/D0 to D2
FindWord:
        move.l  Latest, a2
        moveq   #0, d1
.Loop:
        cmp.l   #0, a2
        beq.s   .Done
        move.b  4(a2), d1
        and.b   #F_HIDDEN|F_LENMASK, d1
        cmp.b   d1, d0
        bne.s   .Next
.Compare:
        subq.b  #1, d1
        bmi.s   .Done
        move.b  (a0,d1.w), d2
        cmp.b   5(a2,d1.w), d2
        beq.s   .Compare
.Next:
        move.l  (a2), a2
        bra.s   .Loop
.Done:
        rts

WordCFA:
        moveq   #0, d0
        move.b  4(a2), d0
        and.b   #F_LENMASK, d0
        add.l   a2, d0
        add.l   #5+3, d0
        and.b   #-4, d0
        move.l  d0, a0
        rts

PrintWords:
        movem.l d0-d4/a0-a2, -(sp)
        move.l  Latest, a2
        move.l  #10, d4 ; Limit number of words to print
.L:
        cmp.l   #0, a2
        beq.s   .D
        move.l  a2, d0
        jsr     HexDot
        move.b  #' ', d0
        jsr     Emit
        moveq   #0, d0
        move.b  4(a2), d0
        jsr     HexDot
        move.b  #' ', d0
        jsr     Emit
        moveq   #0, d3
        move.b  4(a2), d3
        and.b   #F_LENMASK, d3
        move.l  a2, d2
        add.l   #5, d2
        move.l  Stdout, d1
        jsr     WriteFile
        moveq   #10, d0
        jsr     Emit
        move.l  (a2), a2
        subq    #1, d4
        bne.s   .L
.D:
        movem.l (sp)+, d0-d4/a0-a2
        rts

ConvertNumber:
        move.l  a0, a3
        move.l  d0, d7

        move.b  (a0), d4
        cmp.b   #'-', d4
        bne.s   .NotNeg
        addq.l  #1, a0
        subq.l  #1, d0
.NotNeg:
        moveq   #0, d1
        moveq   #0, d2
.Cvt:
        move.b  (a0)+, d2
        sub.b   #'0', d2
        bcs.s   .Err
        cmp.b   #9, d2
        bls.s   .Add
        sub.b   #'A'-'0'-10, d2
        cmp.b   #10, d2
        bcs.s   .Err
.Add:
        cmp.b   Base+3, d2
        bcc.s   .Err
        move.l  d1, d3
        swap    d3
        mulu.w  Base+2, d1
        swap    d1
        mulu.w  Base+2, d3
        add.w   d3, d1
        swap    d1
        add.l   d2, d1
        subq    #1, d0
        bne.s   .Cvt
        move.l  d1, d0
        cmp.b   #'-', d4
        bne.s   .Ret
        neg.l   d0
.Ret:
        rts
.Err:
        move.l  #.Msg, a0
        jsr     PutZStr
        move.l  Stdout, d1
        move.l  a3, d2
        move.l  d7, d3
        jsr     WriteFile
        move.l  #.Msg2, a0
        jsr     PutZStr
        move.l  #1, -(sp)
        jmp     HALT+4
.Msg:   dc.b 13, 10, "Invalid Number: \"", 0
.Msg2   dc.b "\"", 13, 10, 0
        even

HexDot:
        movem.l d0-d3/a0-a1, -(sp)
        subq.l  #8, sp
        move.l  sp, a0
        moveq   #8-1, d1
.Cvt:
        rol.l   #4, d0
        move.l  d0, d2
        and.b   #$f, d0
        add.b   #'0', d0
        cmp.b   #'9', d0
        bls.s   .Store
        addq.b  #7, d0
.Store:
        move.b  d0, (a0)+
        move.l  d2, d0
        subq    #1, d1
        bpl.s   .Cvt

        move.l  Stdout, d1
        move.l  sp, d2
        move.l  #8, d3
        jsr     WriteFile
        addq    #8, sp
        movem.l (sp)+, d0-d3/a0-a1
        rts

DoComma:
        move.l  Here, a0
        move.l  d0, (a0)+
        move.l  a0, Here
        rts

UDiv32:
        move.l  d0, d2  ; N
        move.l  d1, d3  ; D
        moveq   #0, d0  ; Q = 0
        moveq   #0, d1  ; R = 0
        move.w  #32-1, d4
.div:
        lsl.l   #1, d1  ; R <<= 1
        btst.l  d4, d2
        beq.s   .notset
        or.w    #1, d1  ; R(0) = N(i)
.notset:
        cmp.l   d3, d1  ; R >= D
        blt.s   .next
        sub     d3, d1  ; R -= D
        bset.l  d4, d0  ; Q(i) = 1
.next:
        dbf     d4, .div
        rts

LAST_LINK set 0

WORD_HEADER macro
        align   2
.Start\@:
        dc.l    LAST_LINK
LAST_LINK set .Start\@
        ifnb    \3
                dc.b    (.StrEnd\@ - .Str\@) | \3
        else
                dc.b    .StrEnd\@ - .Str\@
        endif
.Str\@:
        ifnb \2
                dc.b    \2
        else
                dc.b    "\1"
        endif
.StrEnd\@:
        align   2
\1:
        endm

NATIVE  macro
        WORD_HEADER \1, \2, \3
        dc.l    *+4
        endm

        NATIVE HALT
        move.l (sp)+, d7
        beq.s   .OK ; Only print on error
        jsr     PrintWords
.OK:
        move.l  OrigSP, sp
        ; Close dos.library
        move.l	DosBase, a1
	move.l	ExecBase.w, a6
	jsr	_LVOCloseLibrary(a6)
        move.l  d7, d0
        rts

        NATIVE HEXDOT, "H."
        move.l (sp)+, d0
        jsr     HexDot
        moveq   #' ', d0
        jsr     Emit
        NEXT

        NATIVE EXIT
        move.l  (a6)+, a5
        NEXT

        NATIVE BRANCH
        add.l   (a5), a5
        NEXT

        NATIVE ZBRANCH, "0BRANCH"
        move.l  (sp)+, d0
        beq.s   BRANCH+4
        addq.l  #4, a5
        NEXT

        NATIVE EXECUTE
        move.l  (sp)+, a0
        move.l  (a0), a1
        jmp     (a1)

        NATIVE LIT
        move.l  (a5)+, -(sp)
        NEXT

        NATIVE LITSTRING
        move.l  (a5)+, d0
        move.l  a5, -(sp)
        move.l  d0, -(sp)
        lea     3(a5, d0.l), a5
        move.l  a5, d0
        and.b   #-4, d0
        move.l  d0, a5
        NEXT

        NATIVE CHAR
        jsr     ReadWord
        moveq   #0, d0
        move.b  (a0), d0
        move.l  d0, -(sp)
        NEXT

        NATIVE COMMA, ","
        move.l  (sp)+, d0
        jsr     DoComma
        NEXT

        NATIVE OPENFILE, "OPEN-FILE"
        move.l  (sp)+, d2
        move.l  (sp)+, d1
        move.l  a6, -(sp)
        move.l  DosBase, a6
        jsr     _LVOOpen(a6)
        move.l  (sp)+, a6
        move.l  d0, -(sp)
        NEXT

        NATIVE CLOSEFILE, "CLOSE-FILE"
        move.l  (sp)+, d1
        move.l  a6, -(sp)
        move.l  DosBase, a6
        jsr     _LVOClose(a6)
        move.l  (sp)+, a6
        NEXT

        NATIVE WRITEFILE, "WRITE-FILE"
        move.l  (sp)+, d1
        move.l  (sp)+, d3
        move.l  (sp)+, d2
        move.l  a6, -(sp)
        move.l  DosBase, a6
        jsr     _LVOWrite(a6)
        move.l  (sp)+, a6
        move.l  d0, -(sp)
        NEXT

        NATIVE READFILE, "READ-FILE"
        move.l  (sp)+, d1
        move.l  (sp)+, d3
        move.l  (sp)+, d2
        move.l  a6, -(sp)
        move.l  DosBase, a6
        jsr     _LVORead(a6)
        move.l  (sp)+, a6
        move.l  d0, -(sp)
        NEXT

        NATIVE EMIT
        move.l  (sp)+, d0
        jsr     Emit
        NEXT

        NATIVE KEY
        jsr     ReadKey
        move.l  d0, -(sp)
        NEXT

        NATIVE GETWORD, "WORD"
        jsr     ReadWord
        move.l  a0, -(sp)
        move.l  d0, -(sp)
        NEXT

        NATIVE CREATE
        move.l  (sp)+, d0
        move.l  (sp)+, a0

        move.l  Here, a1
        move.l  Latest, a2
        move.l  a1, Latest      ; Update Latest
        move.l  a2, (a1)+
        move.b  d0, (a1)+
.CopyName:
        move.b  (a0)+, (a1)+
        subq.b  #1, d0
        bne.s   .CopyName
        move.l  a1, d0
        addq.l  #3, d0
        and.b   #-4, d0
        move.l  d0, Here
        NEXT

        NATIVE FIND
        move.l  (sp)+, d0
        move.l  (sp)+, a0
        jsr     FindWord
        move.l  a2, -(sp)
        NEXT

        NATIVE CFA, ">CFA"
        move.l  (sp)+, a2
        jsr     WordCFA
        move.l  a0, -(sp)
        NEXT

        NATIVE HIDDEN
        move.l  (sp)+, a0
        eor.b   #F_HIDDEN, 4(a0)
        NEXT

        NATIVE IMMEDIATE,,F_IMMED
        move.l  Latest, a0
        eor.b   #F_IMMED, 4(a0)
        NEXT

        NATIVE LBRACKET, "[", F_IMMED
        move.b  #0, State+3
        NEXT

        NATIVE RBRACKET, "]"
        move.b  #1, State+3
        NEXT

        NATIVE TICK, "'"
        move.l  (a5)+, -(sp)
        NEXT

        NATIVE FETCH, "@"
        move.l  (sp)+, a0
        move.l  (a0), -(sp)
        NEXT

        NATIVE STORE, "!"
        move.l  (sp)+, a0
        move.l  (sp)+, d0
        move.l  d0, (a0)
        NEXT

        NATIVE CFETCH, "C@"
        move.l  (sp)+, a0
        moveq   #0, d0
        move.b  (a0), d0
        move.l  d0, -(sp)
        NEXT

        NATIVE CSTORE, "C!"
        move.l  (sp)+, a0
        move.l  (sp)+, d0
        move.b  d0, (a0)
        NEXT

        NATIVE DSPFETCH, "DSP@"
        move.l  sp, -(sp)
        NEXT

        NATIVE DSPSTORE, "DSP!"
        move.l  (sp)+, sp
        NEXT

        NATIVE RSPFETCH, "RSP@"
        move.l  a6, -(sp)
        NEXT

        NATIVE RSPSTORE, "RSP!"
        move.l  (sp)+, a6
        NEXT

        NATIVE TOR, ">R"
        move.l  (sp)+, -(a6)
        NEXT

        NATIVE FROMR, "R>"
        move.l  (a6)+, -(sp)
        NEXT

        NATIVE SWAP
        move.l  (sp)+, d1
        move.l  (sp)+, d0
        move.l  d1, -(sp)
        move.l  d0, -(sp)
        NEXT

        NATIVE ROT
        movem.l (sp)+, d0-d2
        move.l  d1, -(sp)
        move.l  d0, -(sp)
        move.l  d2, -(sp)
        NEXT


NATIVEBINOP macro
        NATIVE  \1, \2
        move.l  (sp)+, d0
        \1.l    d0, (sp)
        NEXT
        endm

        NATIVEBINOP ADD , "+"
        NATIVEBINOP SUB , "-"
        NATIVEBINOP EOR , "XOR"
        NATIVEBINOP OR  , "OR"
        NATIVEBINOP AND , "AND"

        NATIVE INVERT
        not.l   (sp)
        NEXT

        NATIVE  ALIGNED
        move.l  (sp), d0
        addq.l  #3, d0
        and.b   #-4, d0
        move.l  d0, (sp)
        NEXT

        NATIVE MUL, "*"
        movem.l (sp)+, d0-d1
        move.w  d0, d2
        move.w  d1, d3
        swap    d0
        swap    d1
        mulu    d2, d1
        mulu    d3, d0
        mulu    d3, d2
        add.w   d1, d0
        swap    d0
        clr.w   d0
        add.l   d2, d0
        move.l  d0, -(sp)
        NEXT

        NATIVE UDIVMOD, "U/MOD"
        move.l  (sp)+, d1
        move.l  (sp)+, d0
        jsr     UDiv32
        move.l  d1, -(sp) ; Rem
        move.l  d0, -(sp) ; Quot
        NEXT

        NATIVE DIVMOD, "/MOD"
        move.l  (sp)+, d1
        bmi.s   .DN
        move.l  (sp)+, d0
        bmi.s   .DPNN
        jsr     UDiv32
.Done:
        move.l  d1, -(sp) ; Rem
        move.l  d0, -(sp) ; Quot
        NEXT
.DN:
        neg.l   d1
        move.l  (sp)+, d0
        bmi.s   .DNNN
        jsr     UDiv32
        neg.l   d0
        bra.s   .Done
.DNNN:
        neg.l   d0
        jsr     UDiv32
        neg.l   d1
        bra.s   .Done
.DPNN:
        neg.l   d0
        jsr     UDiv32
        neg.l   d0
        neg.l   d1
        bra.s   .Done

NATIVECOMPARE macro
        NATIVE CMP\1, \2
        movem.l (sp)+, d0-d1
        moveq   #-1, d2
        cmp.l   d0, d1
        b\1.s   \@
        moveq   #0, d2
\@:     move.l  d2, -(sp)
        NEXT
        endm

        NATIVECOMPARE EQ, "="
        NATIVECOMPARE NE, "<>"
        NATIVECOMPARE LT, "<"
        NATIVECOMPARE LE, "<="
        NATIVECOMPARE GE, ">="
        NATIVECOMPARE GT, ">"

        NATIVE INTERPRET
.L:
        jsr     ReadWord
        and.b   d0, d0
        bne.s   .NotEmpty
        move.l  #0, -(sp)
        bra     HALT+4
.NotEmpty:
        jsr     FindWord

        if 0
        movem.l d0-d3/a0-a3, -(sp)
        move.l  Stdout, d1
        move.l  a0, d2
        move.l  d0, d3
        jsr     WriteFile
        move.b  #' ', d0
        jsr     Emit
        move.l  a2, d0
        jsr     HexDot
        move.b  #10, d0
        jsr     Emit
        movem.l (sp)+, d0-d3/a0-a3
        endif

        cmp.l   #0, a2
        beq.s   .Number
        jsr     WordCFA
        btst.b  #6, 4(a2) ; Immediate?
        bne.s   .ExecuteWord
        cmp.b   #0, State+3
        beq.s   .ExecuteWord
        move.l  a0, d0
        jsr     DoComma
        bra.s   .L
.ExecuteWord:
        move.l  (a0), a1
        jmp     (a1)
.Number:
        jsr     ConvertNumber
        move.l  d0, -(sp)
        cmp.b   #0, State+3
        beq.s   .L
        move.l  #LIT, d0
        jsr     DoComma
        move.l  (sp)+, d0
        jsr     DoComma
        bra.s   .L

PutZStr:
        movem.l d2-d3, -(sp)
        moveq   #0, d3
.L:
        tst.b   (a0, d3.w)
        beq.s   .D
        addq.l  #1, d3
        bra.s   .L
.D:
        move.l  Stdout, d1
        move.l  a0, d2
        jsr     WriteFile
        movem.l (sp)+, d2-d3
        rts

DOCOL:
        move.l  a5, -(a6)
        lea     4(a0), a5
        NEXT

DEFWORD macro
        WORD_HEADER \1, \2, \3
        dc.l    DOCOL
        endm

        DEFWORD QUIT
        dc.l    LIT, R0, RSPSTORE
        dc.l    INTERPRET
        dc.l    BRANCH, -8

        DEFWORD COLON, ":"
        dc.l    GETWORD, CREATE
        dc.l    LIT, DOCOL, COMMA
        dc.l    LIT, Latest, FETCH, HIDDEN
        dc.l    RBRACKET, EXIT

        DEFWORD SEMICOLON, ";", F_IMMED
        dc.l    LIT, EXIT, COMMA
        dc.l    LIT, Latest, FETCH, HIDDEN
        dc.l    LBRACKET, EXIT

DEFCONST macro
        NATIVE \1, \2
        move.l  \3, -(sp)
        NEXT
        endm

        DEFCONST LATEST   , "LATEST"    , #Latest
        DEFCONST STATE    , "STATE"     , #State
        DEFCONST HERE     , "HERE"      , #Here
        DEFCONST BASE     , "BASE"      , #Base
        DEFCONST CELLSIZE , "CELL-SIZE" , #4
        DEFCONST GETDOCOL , "DOCOL"     , #DOCOL
        DEFCONST GETS0    , "S0"        , #S0
        DEFCONST GETR0    , "R0"        , #R0
        DEFCONST GETFHID  , "F_HIDDEN"  , #F_HIDDEN
        DEFCONST GETFIMM  , "F_IMMED"   , #F_IMMED
        DEFCONST GETFLEN  , "F_LENMASK" , #F_LENMASK
        DEFCONST GETORDO  , "O_RDONLY"  , #O_RDONLY
        DEFCONST STDIN    , "STDIN"     , Stdin
        DEFCONST STDOUT   , "STDOUT"    , Stdout
        DEFCONST GETDSP   , "DSP"       , #0 ; FIXME

DosName:        dc.b 'dos.library', 0

        SECTION data, DATA
Latest:         dc.l LAST_LINK
State:          dc.l 0
Here:           dc.l R0
Base:           dc.l 10

        SECTION bss, BSS
DosBase:        ds.l 1
Stdin:          ds.l 1
Stdout:         ds.l 1
InputFile:      ds.l 1
OrigSP:         ds.l 1
WordBuffer:     ds.b F_LENMASK+1
                align 2
Cells:          ds.l NUM_CELLS
; vim: syntax=asm68k
