// Register usage:
//   SP: Data stack
//   R4: Return stack
//   R5: Instruction Pointer
//   R0: LastCFA

        .syntax unified
        .thumb

        .text

        .set NUM_CELLS,         4096
        .set RSIZE,             128
        .set S0,                (Cells + NUM_CELLS*4)
        .set R0,                (Cells + RSIZE*4)

        .set S_IMMEDIATE,       0x00
        .set S_COMPILE,         0x01

        .set F_HIDDEN,          0x80
        .set F_IMMED,           0x40
        .set F_LENMASK,         0x1F

        .set O_RDONLY,          0x00
        .set O_WRONLY,          0x01
        .set O_RDWR,            0x02
        .set O_CREAT,           0x40

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

#include "sys-linux-arm.S"

        .thumb_func
StartInterpreter:
        ldr r0, =StdoutFile
        ldr r0, [r0]
        ldr r1, =OutputFile
        str r0, [r1]

        ldr r0, =ArgV
        ldr r0, [r0]
        ldr r0, [r0, #4]
        cmp r0, #0
        beq 1f
        mov r1, #O_RDONLY
        bl NativeOpen
        b 2f
1:
        ldr r0, =StdinFile
        ldr r0, [r0]
2:
        ldr r1, =InputFile
        str r0, [r1]

        ldr sp, =S0
        ldr r4, =R0
        ldr r5, =1f
        NEXT
1:
        .long QUIT

        .thumb_func
ReadKey:
        push {lr}
        sub sp, sp, #4
        ldr r0, =InputFile
        ldr r0, [r0]
        mov r1, sp
        mov r2, #1
        bl NativeRead
        cmp r0, #1
        beq 1f
        mov r0, #0
        b 2f
1:
        ldrb r0, [sp]
2:
        add sp, sp, #4
        pop {pc}

        .thumb_func
ReadWord:
        push {r4,lr}
        ldr r4, =WordBuffer
1:
        bl ReadKey
        cmp r0, #0
        beq 5f
        cmp r0, #' '
        bls 1b
        cmp r0, #'\\'
        bne 3f
2:
        bl ReadKey
        cmp r0, #0
        beq 5f
        cmp r0, #10
        bne 2b
        b 1b
3:
        cmp r0, #'a'
        blo 4f
        cmp r0, #'z'
        bhi 4f
        and r0, r0, #0xdf
4:
        strb r0, [r4], #1
        bl ReadKey
        cmp r0, #' '
        bhi 3b
5:
        mov r0, #0
        strb r0, [r4] // NUL-terminate for ease of debugging
        ldr r1, =WordBuffer
        sub r0, r4, r1
        pop {r4,pc}
        bx lr

        .thumb_func
FindWord: // r0 = WordLen, r1 = Name
        push {r4-r6}
        ldr r2, =Latest
        ldr r2, [r2]
1:
        cmp r2, #0
        beq 4f
        ldrb r3, [r2, #4]
        and r3, r3, #F_LENMASK|F_HIDDEN
        cmp r3, r0
        bne 3f
        mov r3, #0
        add r6, r2, #5
2:
        cmp r3, r0
        beq 4f
        ldrb r4, [r6, r3]
        ldrb r5, [r1, r3]
        cmp r4, r5
        bne 3f
        add r3, r3, #1
        b 2b
3:      ldr r2, [r2]
        b 1b
4:
        mov r0, r2
        pop {r4-r6}
        bx lr

        .thumb_func
WordCFA:
        ldrb r1, [r0, #4]
        and r1, r1, #F_LENMASK
        add r1, r1, #5+3
        add r0, r0, r1
        and r0, r0, #-4
        bx lr

        .thumb_func
// In:  r0 = length, r1 = buffer
// Out: r0 = result, r1 = number of unparsed characters
ConvertNumber:
        cmp r0, #0
        bne 20f
10:
        mov r1, #-1
        bx lr
20:
        ldrb r2, [r1], #1 // r2 = sign
        mov r3, r2
        cmp r2, #'-'
        bne 30f
        subs r0, r0, #1
        beq 10b
        ldrb r3, [r1], #1
30:
        // r3 = current char
        push {r4-r5}
        mov r4, #0 // r4 = result
        ldr r5, =Base
        ldr r5, [r5] // r5 = base
40:
        mul r4, r4, r5
        cmp r3, #'0'
        blo 60f
        cmp r3, #'9'
        bhi 45f
        sub r3, r3, #'0'
        b 50f
45:
        cmp r3, #'A'
        blo 60f
        cmp r3, #'Z'
        bhi 60f
        sub r3, r3, #'A'-10
50:
        cmp r3, r5
        bhs 60f
        add r4, r4, r3
        subs r0, r0, #1
        beq 60f
        ldrb r3, [r1], #1
        b 40b
60:
        cmp r2, #'-'
        bne 70f
        rsb r4, r4, #0
70:
        mov r1, r0
        mov r0, r4
        pop {r4-r5}
        bx lr


        .thumb_func
Emit:
        push {r0,lr}
        ldr r0, =OutputFile
        ldr r0, [r0]
        mov r1, sp
        mov r2, #1
        bl NativeWrite
        pop {r0,pc}

        .thumb_func
EmitZStr:
        push {r4,lr}
        mov r4, r0
1:
        ldrb r0, [r4], #1
        cmp r0, #0
        beq 2f
        bl Emit
        b 1b
2:
        pop {r4,pc}

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

NATIVE EXECUTE
        pop {r0}
        ldr r1, [r0]
        bx r1

NATIVE LIT
        ldr r0, [r5], #4
        push {r0}
        NEXT

NATIVE LITSTRING
        ldr r0, [r5], #4
        push {r5}
        push {r0}
        add r0, r0, 3
        add r5, r5, r0
        and r5, r5, #-4
        NEXT

NATIVE CHAR
        bl ReadWord
        ldrb r0, [r1]
        push {r0}
        NEXT

NATIVE COMMA, ","
        pop {r0}
        bl DoComma
        NEXT

NATIVE OPENFILE, "OPEN-FILE"
        pop {r1}
        pop {r0}
        bl NativeOpen
        push {r0}
        NEXT

NATIVE CLOSEFILE, "CLOSE-FILE"
        pop {r0}
        bl NativeClose
        NEXT

NATIVE READFILE, "READ-FILE"
        pop {r0}
        pop {r2}
        pop {r1}
        bl NativeRead
        push {r0}
        NEXT

NATIVE WRITEFILE, "WRITE-FILE"
        pop {r0}
        pop {r2}
        pop {r1}
        bl NativeWrite
        push {r0}
        NEXT

NATIVE EMIT
        pop {r0}
        bl Emit
        NEXT

NATIVE KEY
        bl ReadKey
        push {r0}
        NEXT

NATIVE GETWORD, "WORD"
        bl ReadWord
        push {r1}
        push {r0}
        NEXT

NATIVE CREATE
        pop {r0,r1}             // r1 = name, r0 = length
        push {r4-r5}
        ldr r2, =Here           // r2 = &Here
        ldr r3, =Latest         // r3 = &Latest
        ldr r4, [r2]            // r4 = Here
        ldr r5, [r3]            // r5 = Latest
        str r4, [r3]            // Update Latest
        str r5, [r4], #4        // Store link in header
        strb r0, [r4], #1       // Store length
1:                              // Copy name
        cmp r0, #0
        beq 2f
        ldrb r5, [r1], #1
        strb r5, [r4], #1
        add r0, r0, #-1
        b 1b
2:
        add r4, r4, 3           // Align Here
        and r4, r4, -4
        str r4, [r2]            // Update Here
        pop {r4-r5}
        NEXT

NATIVE FIND
        pop {r0}
        pop {r1}
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
        str r1, [r0]
        NEXT

NATIVE CFETCH, "C@"
        pop {r0}
        ldrb r0, [r0]
        push {r0}
        NEXT

NATIVE CSTORE, "C!"
        pop {r0,r1}
        strb r1, [r0]
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
        push {r0}
        push {r2}
        NEXT

NATIVE ADD, "+"
        pop {r0,r1}
        add r0, r0, r1
        push {r0}
        NEXT

NATIVE SUB, "-"
        pop {r0,r1}
        sub r0, r1, r0
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
        push {r0-r1}
        NEXT

NATIVE UDIVMOD, "U/MOD"
        pop {r1}
        pop {r0}
        bl UDiv32
        push {r0-r1}
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

NATIVE ALIGNED
        ldr r0, [sp]
        add r0, r0, #3
        and r0, r0, #-4
        str r0, [sp]
        NEXT

        .macro NATIVECOMPARE, flags, name
NATIVE CMP\flags, "\name"
        pop {r0,r1}
        mov r2, #-1
        cmp r1, r0
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
        beq NativeExit
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
        cmp r1, #0
        bne .LErr
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
.LErr:
        ldr r0, =2f
        bl EmitZStr
        ldr r0, =WordBuffer
        bl EmitZStr
        ldr r0, =3f
        bl EmitZStr
        mov r0, #-1
        b NativeExit

2: .asciz "Invalid Word: \""
3: .asciz "\"\n"
        .align 2, 0

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
        mov r0, #\val
        push {r0}
        NEXT
        .endm

DEFVAR   VAR_LATEST      , "LATEST"    , Latest
DEFVAR   VAR_STATE       , "STATE"     , State
DEFVAR   VAR_HERE        , "HERE"      , Here
DEFVAR   VAR_BASE        , "BASE"      , Base
DEFVAR   CONST_DOCOL     , "DOCOL"     , DOCOL
DEFVAR   CONST_R0        , "R0"        , R0
DEFVAR   CONST_S0        , "S0"        , S0
DEFCONST CONST_CELL_SIZE , "CELL-SIZE" , 4
DEFCONST CONST_F_LENMASK , "F_LENMASK" , F_LENMASK
DEFCONST CONST_F_IMMED   , "F_IMMED"   , F_IMMED
DEFCONST CONST_F_HIDDEN  , "F_HIDDEN"  , F_HIDDEN
DEFCONST CONST_O_RDONLY  , "O_RDONLY"  , O_RDONLY
DEFCONST CONST_O_WRONLY  , "O_WRONLY"  , O_WRONLY
DEFCONST CONST_O_RDWR    , "O_RDWR"    , O_RDWR
DEFCONST CONST_O_CREAT   , "O_CREAT"   , O_CREAT

NATIVE CONST_STDIN, "STDIN"
        ldr r0, =StdinFile
        ldr r0, [r0]
        push {r0}
        NEXT

NATIVE CONST_STDOUT, "STDOUT"
        ldr r0, =StdoutFile
        ldr r0, [r0]
        push {r0}
        NEXT

// TODO FIXME
DEFCONST HACK_DSP        , "DSP"       , 0
DEFCONST HACK_KERNEL32   , "KERNEL32"  , 0
DEFCONST HACK_GETPROC    , "GETPROC"   , 0

        .data

Latest: .long LAST_LINK
State:  .long S_IMMEDIATE
Here:   .long R0
Base:   .long 10

        .bss
.lcomm ArgC, 4
.lcomm ArgV, 4
.lcomm StdinFile, 4
.lcomm StdoutFile, 4
.lcomm InputFile, 4
.lcomm OutputFile, 4
.lcomm Cells, NUM_CELLS*4
.lcomm WordBuffer, F_LENMASK+1
