// Register usage:
//   SP: Data stack
//   R4: Return stack
//   R5: Instruction Pointer
//   R0: LastCFA


        .syntax unified
        .thumb

        .globl Latest
        .globl State
        .globl Here
        .globl Base
        .globl WordBuffer
        .globl InputFile
        .globl OutputFile
        .globl StdinFile
        .globl StdoutFile
        .globl ForthMain

        .extern C_ReadWord
        .extern C_ReadKey
        .extern C_FindWord
        .extern C_ConvertNumber
        .extern C_Emit
        .extern C_Debug

        .text

        .set NUM_CELLS,         8192 // 4096
        .set RSIZE,             128
        .set S0,                (Cells + NUM_CELLS*4)
        .set R0,                (Cells + RSIZE*4)

        .set S_IMMEDIATE,       0x00
        .set S_COMPILE,         0x01

        .set F_HIDDEN,          0x80
        .set F_IMMED,           0x40
        .set F_LENMASK,         0x1F

        .set LAST_LINK, 0

        .macro WORD_HEADER label, name="", flags=0
        .align 2, 0
1:
        .long LAST_LINK
        .set LAST_LINK, 1b
        .byte (3f-2f)|\flags
        .set WORD_FLAGS, 0
2:
        .ifnb \name
                .ascii "\name"
        .else
                .ascii "\label"
        .endif
3:
        .align 2, 0
\label:
        .endm

        .macro NATIVE label, name="", flags=0
WORD_HEADER \label, "\name", \flags
        .long \label\()_code
        .thumb_func
\label\()_code:
        .endm

        .macro NEXT
        ldr r0, [r5], #4
        ldr r1, [r0]
        bx r1
        .endm

        .thumb_func
ForthMain:
        push {r4-r8, lr}
        ldr r0, =OldSP
        str sp, [r0]

        ldr sp, =S0
        ldr r4, =R0
        ldr r5, =1f
        NEXT
1:
        .long QUIT

        .thumb_func
ReadWord:
        push {lr}
        blx C_ReadWord
        ldr r2, =WordBuffer+F_LENMASK+1
        ldrb r0, [r2]
        ldr r1, =WordBuffer
        pop {pc}

        .thumb_func
FindWord:
        push {lr}
        blx C_FindWord
        pop {pc}

        .thumb_func
WordCFA:
        ldrb r1, [r0, #4]
        and r1, r1, #F_LENMASK
        add r1, r1, #5+3
        add r0, r0, r1
        and r0, r0, #-4
        bx lr

        .thumb_func
ConvertNumber:
        push {lr}
        blx C_ConvertNumber
        pop {pc}

        .thumb_func
Emit:
        push {lr}
        blx C_Emit
        pop {pc}

        .thumb_func
DoComma:
        ldr r1, =Here
        ldr r2, [r1]
        str r0, [r2], #4
        str r2, [r1]
        bx lr

        .thumb_func
UDiv32:
        mov r2, #0              // Quotient
        mov r3, #0              // Remainder
        .set mask, 1<<31
        .rept 32
        add r3, r3, r3          // R <<= 1
        tst r0, #mask           // Numerator & Mask
        it ne
        orrne r3, r3, #1        // if (Numerator & Mask) Rem |= 1
        cmp r3, r1              // Rem >= Den
        itt hs
        subhs r3, r3, r1        // Rem -= Den
        orrhs r2, r2, #mask     // Quot |= Mask
        .set mask, mask>>1
        .endr
        mov r0, r2
        mov r1, r3
        bx lr

        .thumb_func
Div32:
        cmp r0, #0
        blt 1f
        cmp r1, #0
        blt 3f
        b UDiv32
1: // N < 0
        cmp r1, #0
        bge 2f
   // N < 0, D < 0
        rsb r0, r0, #0
        rsb r1, r1, #0
        push {lr}
        bl UDiv32
        rsb r1, r1, #0
        pop {pc}
2: // N < 0, D >= 0
        rsb r0, r0, #0
        push {lr}
        bl UDiv32
        rsb r0, r0, #0
        rsb r1, r1, #0
        pop {pc}
3: // N >= 0, D < 0
        rsb r1, r1, #0
        push {lr}
        bl UDiv32
        rsb r0, r0, #0
        pop {pc}

NATIVE RETC
        // Leave r0 untouched
        ldr r1, =OldSP
        ldr sp, [r1]
        pop {r4-r8, pc}

NATIVE DEBUG
        ldr r0, [sp]
        bl C_Debug
        NEXT

NATIVE EXIT
        ldr r5, [r4], #4
        NEXT

NATIVE BRANCH
        ldr r0, [r5]
        add r5, r5, r0
        NEXT

NATIVE ZBRANCH, "0BRANCH"
        pop {r0}
        cmp r0, #0
        beq BRANCH+4
        add r5, r5, #4
        NEXT

NATIVE LIT
        ldr r0, [r5], #4
        push {r0}
        NEXT

;NATIVE LITSTRING
        

NATIVE CHAR
        bl ReadWord
        ldrb r0, [r1]
        push {r0}
        NEXT

NATIVE COMMA, ","
        pop {r0}
        bl DoComma
        NEXT

NATIVE EMIT
        pop {r0}
        bl Emit
        NEXT

NATIVE KEY
        bl C_ReadKey
        push {r0}
        NEXT

NATIVE GETWORD, "WORD"
        bl ReadWord
        mov r2, r0
        push {r1-r2}
        NEXT

NATIVE CREATE
        pop {r0-r1}             // r0 = name, r1 = length
        push {r4-r5}
        ldr r2, =Here           // r2 = &Here
        ldr r3, =Latest         // r3 = &Latest
        ldr r4, [r2]            // r4 = Here
        ldr r5, [r3]            // r5 = Latest
        str r4, [r3]            // Update Latest
        str r5, [r4], #4        // Store link in header
        strb r1, [r4], #1       // Store length
1:                              // Copy name
        cmp r1, #0
        beq 2f
        ldrb r5, [r0], #1
        strb r5, [r4], #1
        add r1, r1, #-1
        b 1b
2:
        add r4, r4, 3           // Align Here
        and r4, r4, -4
        str r4, [r2]            // Update Here
        pop {r4-r5}
        NEXT

NATIVE FIND
        pop {r0,r1}
        bl FindWord
        push {r0}
        NEXT

NATIVE CFA, ">CFA"
        pop {r0}
        bl WordCFA
        push {r0}
        NEXT

NATIVE HIDDEN
        pop {r0}
        add r0, r0, #4
        ldrb r1, [r0]
        eor r1, r1, F_HIDDEN
        strb r1, [r0]
        NEXT

NATIVE IMMEDIATE,,F_IMMED
        ldr r0, =Latest
        ldr r0, [r0]
        add r0, 4
        ldrb r1, [r0]
        eor r1, r1, F_IMMED
        strb r1, [r0]
        NEXT

NATIVE LBRACKET, "[", F_IMMED
        ldr r0, =State
        ldr r1, =S_IMMEDIATE
        str r1, [r0]
        NEXT

NATIVE RBRACKET, "]"
        ldr r0, =State
        ldr r1, =S_COMPILE
        str r1, [r0]
        NEXT

NATIVE TICK, "\x27"
        ldr r0, [r5], #4
        push {r0}
        NEXT

NATIVE FETCH, "\@"
        pop {r0}
        ldr r0, [r0]
        push {r0}
        NEXT

NATIVE STORE, "!"
        pop {r0,r1}
        str r0, [r1]
        NEXT

NATIVE CFETCH, "C@"
        pop {r0}
        ldrb r0, [r0]
        push {r0}
        NEXT

NATIVE CSTORE, "C!"
        pop {r0,r1}
        strb r0, [r1]
        NEXT

NATIVE DSPFETCH, "DSP@"
        mov r0, sp
        push {r0}
        NEXT

NATIVE DSPSTORE, "DSP!"
        pop {r0}
        mov sp, r0
        NEXT

NATIVE RSPFETCH, "RSP@"
        push {r4}
        NEXT

NATIVE RSPSTORE, "RSP!"
        pop {r4}
        NEXT

NATIVE TOR, ">R"
        pop {r0}
        str r0, [r4, #-4]!
        NEXT

NATIVE FROMR, "R>"
        ldr r0, [r4], #4
        push {r0}
        NEXT

NATIVE SWAP
        pop {r0-r1}
        mov r2, r0
        push {r1-r2}
        NEXT

NATIVE ROT
        pop {r0-r2}
        push {r1}
        push {r2}
        push {r0}
        NEXT

NATIVE ADD, "+"
        pop {r0,r1}
        add r0, r0, r1
        push {r0}
        NEXT

NATIVE SUB, "-"
        pop {r0,r1}
        sub r0, r0, r1
        push {r0}
        NEXT

NATIVE MUL, "*"
        pop {r0,r1}
        mul r0, r0, r1
        push {r0}
        NEXT

NATIVE DIVMOD, "/MOD"
        pop {r1}
        pop {r0}
        bl Div32
        push {r0,r1}
        NEXT

NATIVE UDIVMOD, "U/MOD"
        pop {r1}
        pop {r0}
        bl UDiv32
        push {r0,r1}
        NEXT

NATIVE BAND, "AND"
        pop {r0,r1}
        and r0, r0, r1
        push {r0}
        NEXT

NATIVE BOR, "OR"
        pop {r0,r1}
        orr r0, r0, r1
        push {r0}
        NEXT

NATIVE BXOR, "XOR"
        pop {r0,r1}
        eor r0, r0, r1
        push {r0}
        NEXT

NATIVE INVERT
        ldr r0, [sp]
        eor r0, r0, #-1
        str r0, [sp]
        NEXT

        .macro NATIVECOMPARE, flags, name
NATIVE CMP\flags, "\name"
        pop {r0,r1}
        mov r2, #-1
        cmp r0, r1
        b\flags 1f
        mov r2, #0
1:
        push {r2}
        NEXT
        .endm

NATIVECOMPARE EQ, "\="
NATIVECOMPARE NE, "<>"
NATIVECOMPARE LT, "<"
NATIVECOMPARE LE, "<="
NATIVECOMPARE GE, ">="
NATIVECOMPARE GT, ">"

NATIVE INTERPRET
.LInterpreterLoop:
        bl ReadWord
        cmp r0, #0
        beq RETC+4 // TODO!
        push {r0-r1}
        bl FindWord
        cmp r0, #0
        beq .LNumber
        add sp, sp, #8
        ldrb r2, [r0, #4]
        push {r2}
        bl WordCFA
        pop {r2}
        ands r2, r2, #F_IMMED
        bne .LExecute
        ldr r2, =State
        ldr r2, [r2]
        cmp r2, #S_IMMEDIATE
        beq .LExecute
        bl DoComma
        b .LInterpreterLoop
.LExecute:
        ldr r1, [r0]
        bx r1
.LNumber:
        pop {r0-r1}
        bl ConvertNumber
        push {r0}
        ldr r1, =State
        ldr r1, [r1]
        cmp r1, #S_IMMEDIATE
        beq .LInterpreterLoop
        ldr r0, =LIT
        bl DoComma
        pop {r0}
        bl DoComma
        b .LInterpreterLoop

        .thumb_func
DOCOL:
        str r5, [r4, #-4]!
        add r5, r0, #4
        NEXT

        .macro DEFWORD label, name="", flags=0
WORD_HEADER \label, "\name", \flags
        .long DOCOL
        .endm

DEFWORD QUIT
        .long LIT, R0, RSPSTORE
        .long INTERPRET
        .long BRANCH, -8

DEFWORD COLON, "\:"
        .long GETWORD, CREATE
        .long LIT, DOCOL, COMMA
        .long LIT, Latest, FETCH, HIDDEN
        .long RBRACKET, EXIT

DEFWORD SEMICOLON, "\;", F_IMMED
        .long LIT, EXIT, COMMA
        .long LIT, Latest, FETCH, HIDDEN
        .long LBRACKET, EXIT

        .macro DEFVAR label, name, val
NATIVE \label, "\name"
        ldr r0, =\val
        push {r0}
        NEXT
        .endm

        .macro DEFCONST label, name, val
NATIVE \label, "\name"
        mov r0, \val
        push {r0}
        NEXT
        .endm

DEFVAR   VAR_LATEST      , "LATEST"    , Latest
DEFVAR   VAR_STATE       , "STATE"     , State
DEFVAR   VAR_HERE        , "HERE"      , Here
DEFVAR   VAR_BASE        , "BASE"      , Base
DEFCONST CONST_CELL_SIZE , "CELL-SIZE" , 4

        .data

Latest: .long LAST_LINK
State:  .long S_IMMEDIATE
Here:   .long R0
Base:   .long 10

        .bss

.lcomm OldSP, 4
.lcomm StdinFile, 4
.lcomm StdoutFile, 4
.lcomm InputFile, 4
.lcomm OutputFile, 4
.lcomm Cells, NUM_CELLS*4
.lcomm WordBuffer, F_LENMASK+2
