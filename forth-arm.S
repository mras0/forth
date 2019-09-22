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
        .extern C_FindWord
        .extern C_ConvertNumber
        .extern C_Emit

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
WORD_HEADER \label, \name, \flags
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

NATIVE RETC
        // Leave r0 untouched
        ldr r1, =OldSP
        ldr sp, [r1]
        pop {r4-r8, pc}

NATIVE EMIT
        pop {r0}
        bl Emit
        NEXT

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
        bl WordCFA
        ldr r1, [r0]
        bx r1
.LNumber:
        pop {r0-r1}
        bl ConvertNumber
        push {r0}
        b .LInterpreterLoop

        .thumb_func
DOCOL:
        str r5, [r4, #-4]!
        add r5, r0, #4
        NEXT

        .macro DEFWORD label, name="", flags=0
WORD_HEADER \label, \name, \flags
        .long DOCOL
        .endm

DEFWORD QUIT
        // .long LIT, R0, RSPSTORE
        .long INTERPRET
        .long RETC
        //.long BRANCH, -8

        .data

Latest: .long LAST_LINK
State:  .long 0
Here:   .long 0
Base:   .long 10

        .bss

.lcomm OldSP, 4
.lcomm StdinFile, 4
.lcomm StdoutFile, 4
.lcomm InputFile, 4
.lcomm OutputFile, 4
.lcomm Cells, NUM_CELLS*4
.lcomm WordBuffer, F_LENMASK+2
