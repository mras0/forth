        .p02
        .export __LOADADDR__: absolute = 1
        .segment "LOADADDR"
        .addr *+2
        .segment "STARTUP"
        .code
        .byte $0b, $08, $01, $00, $9e, $32, $30, $36, $31, $00, $00, $00 ; SYS 2061

BASOUT = $ffd2

LASTCFA = $55
IPTR    = $57
RSP     = $59
DSP     = $5B
TMP1    = $5D
TMP2    = $5F
RES     = $61
HERE    = $63

NUM_CELLS = 4096
RSIZE     = 128

R0 = Cells + RSIZE*2
S0 = Cells + NUM_CELLS * 2

F_HIDDEN  = $80
F_IMMED   = $40
F_LENMASK = $1f

.macro PUTSTR str
        ldx #<:+
        ldy #>:+
        jsr PrintZStr
        jmp :++
:
        .asciiz str
:
.endmacro

.macro PUTZPWORD addr
        ldx addr
        ldy addr+1
        jsr PrintHexWord
.endmacro

.macro PUTIZPWORD addr
        ldy #0
        lda (addr), y
        tax
        iny
        lda (addr), y
        tay
        jsr PrintHexWord
.endmacro

.macro PUTCR
        jsr PrintCr
.endmacro

.macro WORD_HEADER label, name, flags, last
        .word last
        .byte .strlen(name) | flags, name
label:
.endmacro

.macro NATIVE label, name, flags, last
        WORD_HEADER label, name, flags, last
        .word *+2-1 ; Adjustment for RTS
.endmacro

.macro DEFWORD label, name, flags, last
        WORD_HEADER label, name, flags, last
        .word DoCol-1 ; Adjustment for RTS
.endmacro

.macro ADDN addr, n
        lda addr
        clc
        adc #n
        sta addr
        bcc :+
        inc addr+1
:
.endmacro

.macro SUBN addr, n
        lda addr
        sec
        sbc #n
        sta addr
        bcs :+
        dec addr+1
:
.endmacro

.macro GOCFA
        ;PUTSTR "lastcfa="
        ;PUTZPWORD LASTCFA
        ;PUTSTR " *lastcfa="
        ;PUTIZPWORD LASTCFA
        ;PUTCR

        ldy #1
        lda (LASTCFA), y
        pha
        dey
        lda (LASTCFA), y
        pha
        rts
.endmacro

.macro NEXT
        ;PUTSTR "iptr="
        ;PUTZPWORD IPTR
        ;PUTSTR " *iptr="
        ;PUTIZPWORD IPTR
        ;PUTCR

        ldy #0
        lda (IPTR), y
        sta LASTCFA
        iny
        lda (IPTR), y
        sta LASTCFA+1

        ADDN IPTR, 2
        GOCFA
.endmacro

.macro STACK_PUSH ptr, addr
        SUBN ptr, 2
        ldy #0
        lda addr
        sta (ptr), y
        iny
        lda addr+1
        sta (ptr), y
.endmacro

.macro STACK_POP ptr, addr
        ldy #0
        lda (ptr), y
        sta addr
        iny
        lda (ptr), y
        sta addr+1
        ADDN ptr, 2
.endmacro

.macro DPUSH addr
        STACK_PUSH DSP, addr
.endmacro

.macro DPOP addr
        STACK_POP DSP, addr
.endmacro

.macro NEGWORD addr
        sec
        lda #0
        sbc addr
        sta addr
        lda #0
        sbc addr+1
        sta addr+1
.endmacro

start:
        lda #<S0
        sta DSP
        lda #>S0
        sta DSP+1

        lda #<R0
        sta RSP
        sta HERE
        lda #>R0
        sta RSP+1
        sta HERE+1

        lda #<@Init
        sta IPTR
        lda #>@Init
        sta IPTR+1
        NEXT
@Init:  .word Quit

; Print NUL-terminated string in X,Y
PrintZStr:
        stx @L2+1
        sty @L2+2
        ldx #0
        beq @L2
@L1:    jsr BASOUT
        inx
@L2:    lda $1234, x
        bne @L1
        rts

; Print string of length A in X,y
PrintStr:
        cmp #0
        beq @L2
        stx @L1+1
        sty @L1+2
        tay
        ldx #0
@L1:    lda $1234, x
        jsr BASOUT
        inx
        dey
        bne @L1
@L2:    rts

PrintSpace:
        lda #' '
        jmp BASOUT

PrintCr:
        lda #13
        jmp BASOUT

; Print word in X,Y
PrintHexWord:
        tya
        jsr PrintByte
        txa
; Print byte in A
PrintByte:
        pha
        lsr
        lsr
        lsr
        lsr
        jsr PrintDigit
        pla
PrintDigit:
        and #$f
        clc
        adc #'0'
        cmp #'9'+1
        bcc :+
        adc #6
:       jmp BASOUT

; RES *= A
Mul8:
        tax
        lda #0
        sta TMP1
        sta TMP1+1
        ldy #8
@Loop:  txa
        lsr
        tax
        bcc @Next
        clc
        lda RES
        adc TMP1
        sta TMP1
        lda RES+1
        adc TMP1+1
        sta TMP1+1
@Next:  asl RES
        rol RES+1
        dey
        bne @Loop
        lda TMP1
        sta RES
        lda TMP1+1
        sta RES+1
        rts

; RES = TMP1*TMP2
Mul16:
        lda #0
        sta RES
        sta RES+1
        ldx #16
:       lsr TMP1+1
        ror TMP1
        bcc :+
        clc
        lda TMP2
        adc RES
        sta RES
        lda TMP2+1
        adc RES+1
        sta RES+1
:       asl TMP2
        rol TMP2+1
        dex
        bne :--
        rts

; TMP1=TMP1/TMP2 RES=TMP1%TMP2
UDiv16:
        lda #0
        sta RES
        sta RES+1
        ldx #16
@DivLoop:
        asl TMP1
        rol TMP1+1
        rol RES
        rol RES+1
        lda RES
        sec
        sbc TMP2
        tay
        lda RES+1
        sbc TMP2+1
        bcc @Skip
        sta RES+1
        sty RES
        inc TMP1
@Skip:  dex
        bne @DivLoop
        rts

ReadKey:
:       lda InputText
        beq :+
        inc :- + 1
        bne :+
        inc :- + 2
:       cmp #$61 ; ASCII lower case 'a'
        bcc :++
        cmp #$7a+1 ; ASCII lower case 'z'
        bcs :+
        and #$df
:       rts
:       cmp #$5c ; ASCII backslash '\'
        bne :+
        lda #'\'
        rts
:       cmp #$5f ; ASCII underscore '_'
        bne :+
        lda #'_'
:       rts

ReadWord:
@SkipSpaces:
        jsr ReadKey
        cmp #0
        beq @End
        cmp #' '+1
        bcc @SkipSpaces
        cmp #'\'
        bne @DoRead
@SkipLine:
        jsr ReadKey
        cmp #0
        beq @End
        cmp #13
        beq @SkipSpaces
        cmp #10
        beq @SkipSpaces
        bne @SkipLine
@DoRead:
        ldx #0
@ReadLoop:
        sta WordBuffer, x
        inx
        jsr ReadKey
        cmp #' '+1
        bcs @ReadLoop
        lda #0
        sta WordBuffer, x ; NUL-terminate
        txa
        sta WordLen
@End:   rts

ConvertNumber:
        lda #0
        sta RES
        sta RES+1
        ldx #0
        lda WordBuffer
        beq @Err
        pha
        cmp #'-'
        bne @Loop
        inx
        bne @Load
@Loop:  pha
        txa
        pha
        lda Base
        jsr Mul8
        pla
        tax
        pla
        sec
        sbc #'0'
        bcc @Err
        cmp #'9'+1
        bcc @Add
        sbc #$C1-'0'-10 ; Petscii 'A' is $C1
@Add:   cmp Base
        bcs @Err
        beq @Err
        clc
        adc RES
        sta RES
        bcc :+
        inc RES+1
:       inx
@Load:  lda WordBuffer, x
        bne @Loop
        pla
        cmp #'-'
        bne @Done
        NEGWORD RES
@Done:
        rts
@Err:   ldx #<@InvalidNumMsg
        ldy #>@InvalidNumMsg
        jsr PrintZStr
        ldx #<WordBuffer
        ldy #>WordBuffer
        jsr PrintZStr
        ldx #<@InvalidNumMsg2
        ldy #>@InvalidNumMsg2
        jsr PrintZStr
        jsr PrintWords
        jmp Halt+2

@InvalidNumMsg:  .byte 13, "invalid number: ", '"', 0
@InvalidNumMsg2: .byte '"', 13, 0

PrintWords:
        lda Latest
        sta TMP1
        lda Latest+1
        sta TMP1+1
        lda #10 ; Limit number of words to print
        sta TMP2
:
        dec TMP2
        beq @Done
        ldx TMP1
        ldy TMP1+1
        jsr PrintHexWord
        jsr PrintSpace
        ldy #2
        lda (TMP1), y
        pha
        jsr PrintByte
        jsr PrintSpace
        lda TMP1
        clc
        adc #3
        tax
        lda TMP1+1
        adc #0
        tay
        pla
        and #F_LENMASK
        jsr PrintStr
        jsr PrintCr
        ldy #0
        lda (TMP1), y
        tax
        iny
        lda (TMP1), y
        sta TMP1+1
        stx TMP1
        cmp #0
        bne :-
        cpx #0
        bne :-
@Done:  rts

        ; Find word of length A in X,Y
        ; Pointer returned in RES, carry clear if found
FindWord:
        sta TMP1
        stx TMP2
        sty TMP2+1
        lda Latest
        sta RES
        lda Latest+1
        sta RES+1
        ; -3 from WordPtr
        sec
        lda TMP2
        sbc #3
        sta TMP2
        bne :+
        dec TMP2+1
:
@Loop:
        ; Byte @ TMP1 = WordLen, Word @ TMP2 = WordPtr-3, RES = Candidate word
        ldy #2
        lda (RES), y
        and #F_LENMASK|F_HIDDEN
        cmp TMP1
        bne @Next
        tax
        ldy #3
@Compare:
        lda (TMP2), y
        cmp (RES), y
        bne @Next
        iny
        dex
        bne @Compare
        clc
        rts
@Next:
        ldy #0
        lda (RES), y
        tax
        iny
        lda (RES), y
        sta RES+1
        stx RES
        cmp #0
        bne @Loop
        cpx #0
        bne @Loop
        sec
        rts

DoCol:
        STACK_PUSH RSP, IPTR
        lda LASTCFA
        clc
        adc #2
        sta IPTR
        lda LASTCFA+1
        adc #0
        sta IPTR+1
        NEXT

W_EXIT:
NATIVE Exit, "exit", 0, 0
        STACK_POP RSP, IPTR
        NEXT

W_LIT:
NATIVE Lit, "lit", 0, W_EXIT
        ldy #0
        lda (IPTR), y
        sta TMP1
        iny
        lda (IPTR), y
        sta TMP1+1
        DPUSH TMP1
        ADDN IPTR, 2
        NEXT

W_LITSTRING:
NATIVE LitString, "litstring", 0, W_LIT
        ldy #0
        lda (IPTR), y
        tax
        ADDN IPTR, 2
        DPUSH IPTR
        clc
        txa
        adc IPTR
        sta IPTR
        bcc :+
        inc IPTR+1
:       txa
        sta TMP1
        lda #0
        sta TMP1+1
        DPUSH TMP1


        DPOP TMP1
        DPOP TMP2
        PUTSTR "tmp1="
        PUTZPWORD TMP1
        PUTSTR " tmp2="
        PUTZPWORD TMP2
        PUTCR
        jmp *

W_CHAR:
NATIVE Char, "char", 0, W_LITSTRING
        jsr ReadWord
        lda WordBuffer
        sta RES
        lda #0
        sta RES+1
        DPUSH RES
        NEXT

W_INTERPRET:
NATIVE Interpret, "interpret", 0, W_CHAR
@Loop:
        jsr ReadWord
        cmp #0
        bne @NotEmpty
        jmp Halt+2
@NotEmpty:
        .if 0
        lda State
        jsr PrintByte
        jsr PrintSpace
        ldx #<WordBuffer
        ldy #>WordBuffer
        jsr PrintStr
        jsr PrintCr
        .endif

        ldx #<WordBuffer
        ldy #>WordBuffer
        lda WordLen
        jsr FindWord
        bcs @Number

        ; WordCFA
        ldy #2
        lda (RES), y
        pha
        and #F_LENMASK
        clc
        adc #3
        adc RES
        sta LASTCFA
        lda RES+1
        adc #0
        sta LASTCFA+1

        pla
        and #F_IMMED
        bne @ExecuteWord
        lda State
        beq @ExecuteWord

        ldy #0
        lda LASTCFA
        sta (HERE), y
        iny
        lda LASTCFA+1
        sta (HERE), y
        ADDN HERE, 2
        jmp @Loop
@ExecuteWord:
        GOCFA
@Number:
        jsr ConvertNumber
        lda State
        bne @CompileNumber
        DPUSH RES
        jmp @Loop
@CompileNumber:
        ldy #0
        lda #<Lit
        sta (HERE), y
        iny
        lda #>Lit
        sta (HERE), y
        ADDN HERE, 2
        ldy #0
        lda RES
        sta (HERE), y
        iny
        lda RES+1
        sta (HERE), y
        ADDN HERE, 2
        jmp @Loop

W_EMIT:
NATIVE Emit, "emit", 0, W_INTERPRET
        ldy #0
        lda (DSP), y
        pha
        ADDN DSP, 2
        pla
        cmp #10
        bne :+
        lda #13 ; Convert LF->CR
:       jsr BASOUT
        NEXT

W_KEY:
NATIVE Key, "key", 0, W_EMIT
        jsr ReadKey
        sta RES
        lda #0
        sta RES+1
        DPUSH RES
        NEXT

W_HALT:
NATIVE Halt, "halt", 0, W_KEY
        ldx #<:+
        ldy #>:+
        jsr PrintZStr
        jmp *
: .asciiz "halting."

W_PLUS:
NATIVE Plus, "+", 0, W_HALT
        DPOP TMP2
        DPOP TMP1
        clc
        lda TMP1
        adc TMP2
        sta TMP1
        lda TMP1+1
        adc TMP2+1
        sta TMP1+1
        DPUSH TMP1
        NEXT

W_SUB:
NATIVE Sub, "-", 0, W_PLUS
        DPOP TMP2
        DPOP TMP1
        sec
        lda TMP1
        sbc TMP2
        sta TMP1
        lda TMP1+1
        sbc TMP2+1
        sta TMP1+1
        DPUSH TMP1
        NEXT

W_MUL:
NATIVE Mul, "*", 0, W_SUB
        DPOP TMP2
        DPOP TMP1
        jsr Mul16
        DPUSH RES
        NEXT

W_UDIVMOD:
NATIVE UDivMod, "u/mod", 0, W_MUL
        DPOP TMP2
        DPOP TMP1
        jsr UDiv16
        DPUSH RES
        DPUSH TMP1
        NEXT

W_DIVMOD:
NATIVE DivMod, "/mod", 0, W_UDIVMOD
        DPOP TMP2
        DPOP TMP1
        BIT TMP1+1
        BMI @L1
        BIT TMP2+1
        BMI @L3
        ; N >= 0, D >= 0
        jsr UDiv16
@Done:
        DPUSH RES
        DPUSH TMP1
        NEXT
@L3:    ; N >= 0, D < 0
        NEGWORD TMP2
        jsr UDiv16
        NEGWORD TMP1
        jmp @Done
@L1:    ; N < 0
        NEGWORD TMP1
        BIT TMP2+1
        BMI @L2
        ; N < 0, D >= 0
        jsr UDiv16
        NEGWORD RES
        NEGWORD TMP1
        jmp @Done
@L2:    ; N < 0, D < 0
        NEGWORD TMP2
        jsr UDiv16
        NEGWORD RES
        jmp @Done

W_EQ:
NATIVE Eq, "=", 0, W_DIVMOD
        DPOP TMP2
        DPOP TMP1
        ldx #0
        lda TMP1
        cmp TMP2
        bne @Done
        lda TMP1+1
        cmp TMP2+1
        bne @Done
        ldx #$ff
@Done:  txa
        sta RES
        sta RES+1
        DPUSH RES
        NEXT

W_NE:
NATIVE Ne, "<>", 0, W_EQ
        DPOP TMP2
        DPOP TMP1
        ldx #$ff
        lda TMP1
        cmp TMP2
        bne @Done
        lda TMP1+1
        cmp TMP2+1
        bne @Done
        ldx #$00
@Done:  txa
        sta RES
        sta RES+1
        DPUSH RES
        NEXT

; Returns N=0 if TMP1 >= TMP2 (rev reverses the arguments)
.macro SIGNED_COMPARE rev
        .if rev
        DPOP TMP1
        DPOP TMP2
        .else
        DPOP TMP2
        DPOP TMP1
        .endif
        LDA TMP1
        CMP TMP2
        LDA TMP1+1
        SBC TMP2+1
        BVC :+
        EOR #$80
:
.endmacro

W_LT:
NATIVE Lt, "<", 0, W_NE
        ldx #$00
        SIGNED_COMPARE 0
        bpl :+
        ldx #$ff
:       stx RES
        stx RES+1
        DPUSH RES
        NEXT

W_LE:
NATIVE Le, "<=", 0, W_LT
        ldx #$ff
        SIGNED_COMPARE 1
        bpl :+
        ldx #$00
:       stx RES
        stx RES+1
        DPUSH RES
        NEXT

W_GE:
NATIVE Ge, ">=", 0, W_LE
        ldx #$00
        SIGNED_COMPARE 1
        bpl :+
        ldx #$ff
:       stx RES
        stx RES+1
        DPUSH RES
        NEXT

W_GT:
NATIVE Gt, ">", 0, W_GE
        ldx #$ff
        SIGNED_COMPARE 0
        bpl :+
        ldx #$00
:       stx RES
        stx RES+1
        DPUSH RES
        NEXT
W_AND:
NATIVE BAnd, "and", 0, W_GT
        DPOP TMP2
        DPOP TMP1
        clc
        lda TMP1
        and TMP2
        sta TMP1
        lda TMP1+1
        and TMP2+1
        sta TMP1+1
        DPUSH TMP1
        NEXT

W_OR:
NATIVE BOr, "or", 0, W_AND
        DPOP TMP2
        DPOP TMP1
        clc
        lda TMP1
        ora TMP2
        sta TMP1
        lda TMP1+1
        ora TMP2+1
        sta TMP1+1
        DPUSH TMP1
        NEXT

W_XOR:
NATIVE BXor, "xor", 0, W_OR
        DPOP TMP2
        DPOP TMP1
        clc
        lda TMP1
        eor TMP2
        sta TMP1
        lda TMP1+1
        eor TMP2+1
        sta TMP1+1
        DPUSH TMP1
        NEXT

W_INVERT:
NATIVE Invert, "invert", 0, W_XOR
        DPOP TMP1
        lda #$ff
        tax
        eor TMP1
        sta TMP1
        txa
        eor TMP1+1
        sta TMP1+1
        DPUSH TMP1
        NEXT

W_BRANCH:
NATIVE Branch, "branch", 0, W_INVERT
        ldy #1
        lda (IPTR), y
        tax
        dey
        lda (IPTR), y
        clc
        adc IPTR
        sta IPTR
        txa
        adc IPTR+1
        sta IPTR+1
        NEXT

W_ZBRANCH:
NATIVE ZBranch, "0branch", 0, W_BRANCH
        DPOP TMP1
        lda TMP1
        bne @Ret
        lda TMP1+1
        bne @Ret
        jmp Branch+2
@Ret:   ADDN IPTR, 2
        NEXT

W_HEXDOT:
NATIVE HexDot, "h.", 0, W_ZBRANCH
        DPOP TMP1
        ldx TMP1
        ldy TMP1+1
        jsr PrintHexWord
        jsr PrintSpace
        NEXT

W_GETWORD:
NATIVE GetWord, "word", 0, W_HEXDOT
        jsr ReadWord
        SUBN DSP, 4
        ldy #0
        lda WordLen
        sta (DSP), y
        iny
        lda #0
        sta (DSP), y
        iny
        lda #<WordBuffer
        sta (DSP), y
        iny
        lda #>WordBuffer
        sta (DSP), y
        NEXT

W_CREATE:
NATIVE Create, "create", 0, W_GETWORD
        ldy #0
        lda (DSP), y
        tax             ; X = length
        iny
        iny
        lda (DSP), y
        sta @Load+1
        iny
        lda (DSP), y
        sta @Load+2

        ldy #0
        lda Latest
        sta (HERE), y
        iny
        lda Latest+1
        sta (HERE), y
        iny
        txa
        sta (HERE), y

        lda HERE
        sta Latest
        lda HERE+1
        sta Latest+1

        ADDN HERE, 3

        lda HERE
        sta @Store+1
        lda HERE+1
        sta @Store+2

        txa
        clc
        adc HERE
        sta HERE
        bcc @Loop
        inc HERE+1

@Loop:  dex
@Load:  lda $1234, x
@Store: sta $1234, x
        cpx #0
        bne @Loop

        ADDN DSP, 4
        NEXT

W_COMMA:
NATIVE Comma, ",", 0, W_CREATE
        ldy #0
        lda (DSP), y
        sta (HERE), y
        iny
        lda (DSP), y
        sta (HERE), y
        ADDN DSP, 2
        ADDN HERE, 2
        NEXT

W_LBRACKET:
NATIVE LBracket, "[", F_IMMED, W_COMMA
        lda #0
        sta State
        NEXT

W_RBRACKET:
NATIVE RBracket, "]", 0, W_LBRACKET
        lda #1
        sta State
        NEXT

W_TICK:
NATIVE Tick, "'", 0, W_RBRACKET
        jmp Lit+2

W_FETCH:
NATIVE Fetch, "@", 0, W_TICK
        DPOP TMP1
        SUBN DSP, 2
        ldy #0
        lda (TMP1), y
        sta (DSP), y
        iny
        lda (TMP1), y
        sta (DSP), y
        NEXT

W_STORE:
NATIVE Store, "!", 0, W_FETCH
        DPOP TMP2
        DPOP TMP1
        ldy #0
        lda TMP1
        sta (TMP2), y
        iny
        lda TMP1+1
        sta (TMP2), y
        NEXT

W_CFETCH:
NATIVE CFetch, "c@", 0, W_STORE
        DPOP TMP1
        SUBN DSP, 2
        ldy #0
        lda (TMP1), y
        sta (DSP), y
        iny
        lda #0
        sta (DSP), y
        NEXT

W_CSTORE:
NATIVE CStore, "c!", 0, W_CFETCH
        DPOP TMP2
        DPOP TMP1
        ldy #0
        lda TMP1
        sta (TMP2), y
        NEXT

W_DSPFETCH:
NATIVE DspFetch, "dsp@", 0, W_CSTORE
        lda DSP
        sta TMP1
        lda DSP+1
        sta TMP1+1
        DPUSH TMP1
        NEXT

W_DSPSTORE:
NATIVE DspStore, "dsp!", 0, W_DSPFETCH
        DPOP TMP1
        lda TMP1
        sta DSP
        lda TMP1+1
        sta DSP+1
        NEXT


W_RSPFETCH:
NATIVE RspFetch, "rsp@", 0, W_DSPSTORE
        DPUSH RSP
        NEXT

W_RSPSTORE:
NATIVE RspStore, "rsp!", 0, W_RSPFETCH
        DPOP RSP
        NEXT

W_TOR:
NATIVE ToR, ">r", 0, W_RSPSTORE
        DPOP TMP1
        STACK_PUSH RSP, TMP1
        NEXT

W_FROMR:
NATIVE FromR, "r>", 0, W_TOR
        STACK_POP RSP, TMP1
        DPUSH TMP1
        NEXT

W_SWAP:
NATIVE Swap, "swap", 0, W_FROMR
        DPOP TMP1
        DPOP TMP2
        DPUSH TMP1
        DPUSH TMP2
        NEXT

W_ROT:
NATIVE Rot, "rot", 0, W_SWAP
        DPOP TMP1
        DPOP TMP2
        DPOP RES
        DPUSH TMP2
        DPUSH TMP1
        DPUSH RES
        NEXT

W_HIDDEN:
NATIVE Hidden, "hidden", 0, W_ROT
        DPOP TMP1
        ldy #2
        lda (TMP1), y
        eor #F_HIDDEN
        sta (TMP1), y
        NEXT

W_IMMEDIATE:
NATIVE Immediate, "immediate", F_IMMED, W_HIDDEN
        lda Latest
        sta TMP1
        lda Latest+1
        sta TMP1+1
        ldy #2
        lda (TMP1), y
        eor #F_IMMED
        sta (TMP1), y
        NEXT

W_CFA:
NATIVE Cfa, ">cfa", 0, W_IMMEDIATE
        DPOP TMP1
        ldy #2
        lda (TMP1), y
        and #F_LENMASK
        clc
        adc #3
        adc TMP1
        sta TMP1
        bcc :+
        inc TMP1+1
:       DPUSH TMP1
        NEXT

W_FIND:
NATIVE Find, "find", 0, W_CFA
        DPOP TMP2
        DPOP TMP1
        ldx TMP1
        ldy TMP1+1
        lda TMP2
        jsr FindWord
        DPUSH RES
        NEXT

W_QUIT:
DEFWORD Quit, "quit", 0, W_FIND
        .word Lit, R0, RspStore
        .word Interpret
        .word Branch, 65536-4

W_COLON:
DEFWORD Colon, ":", 0, W_QUIT
        .word GetWord, Create
        .word Lit, DoCol-1, Comma ; -1 for RTS
        .word Lit, Latest, Fetch, Hidden
        .word RBracket
        .word Exit

W_SEMICOLON:
DEFWORD SemiColon, ";", F_IMMED, W_COLON
        .word Lit, Exit, Comma
        .word Lit, Latest, Fetch, Hidden
        .word LBracket
        .word Exit

W_ALIGNED:
DEFWORD Aligned, "aligned", 0, W_SEMICOLON
        .word Exit ; Nothing to do

.macro DPUSH_LIT val
        SUBN DSP, 2
        ldy #0
        lda #<(val)
        sta (DSP), y
        iny
        lda #>(val)
        sta (DSP), y
        NEXT
.endmacro

W_LATEST:
NATIVE GetLatest, "latest", 0, W_ALIGNED
        DPUSH_LIT Latest

W_HERE:
NATIVE GetHere, "here", 0, W_LATEST
        DPUSH_LIT HERE

W_STATE:
NATIVE GetState, "state", 0, W_HERE
        DPUSH_LIT State

W_BASE:
NATIVE GetBase, "base", 0, W_STATE
        DPUSH_LIT Base

W_CELLSIZE:
NATIVE CellSize, "cell-size", 0, W_BASE
        DPUSH_LIT 2

W_DOCOL:
NATIVE GetDoCol, "docol", 0, W_CELLSIZE
        DPUSH_LIT DoCol-1 ; -1 for RTS

W_S0:
NATIVE GetS0, "s0", 0, W_DOCOL
        DPUSH_LIT S0

W_F_LENMASK:
NATIVE GetFLenMask, "f_lenmask", 0, W_S0
        DPUSH_LIT F_LENMASK

W_F_IMMED:
NATIVE GetFImmed, "f_immed", 0, W_F_LENMASK
        DPUSH_LIT F_IMMED

W_F_HIDDEN:
NATIVE GetFHidden, "f_hidden", 0, W_F_IMMED
        DPUSH_LIT F_HIDDEN

Latest:         .word W_F_HIDDEN
State:          .word 0
Base:           .word 10

WordBuffer:     .res 32
WordLen:        .res 1

InputText:
;.byte "latest @ h. latest @ >cfa h."
.incbin "std.fth"
.byte 0

; Must be last!
Cells:
