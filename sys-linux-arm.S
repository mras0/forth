        .globl _start

        .set STDIN_FILENO       , 0
        .set STDOUT_FILENO      , 1

        .set sys_exit           , 1
        .set sys_read           , 3
        .set sys_write          , 4
        .set sys_open           , 5
        .set sys_close          , 6

        .thumb_func
_start:
        ldr r0, [sp]
        add r1, sp, #4

        ldr r2, =ArgC
        str r0, [r2]
        ldr r2, =ArgV
        str r1, [r2]

        mov r0, #STDIN_FILENO
        ldr r1, =StdinFile
        str r0, [r1]

        mov r0, #STDOUT_FILENO
        ldr r1, =StdoutFile
        str r0, [r1]

        b StartInterpreter

        .thumb_func
// R0 = Exit code
NativeExit:
        mov r7, #sys_exit
        swi #0
        bkpt

        .thumb_func
// R0 = FileName, R1 = Flags
NativeOpen:
        push {r7,lr}
        mov r7, #sys_open
        mov r2, #0
        swi #0
        pop {r7,pc}

        .thumb_func
// R0 = fd
NativeClose:
        push {r7,lr}
        mov r7, #sys_close
        swi #0
        pop {r7,pc}

        .thumb_func
// R0 = fd, R1 = buffer, R2 = count
NativeRead:
        push {r7,lr}
        mov r7, #sys_read
        swi #0
        pop {r7,pc}

        .thumb_func
// R0 = fd, R1 = buffer, R2 = count
NativeWrite:
        push {r7,lr}
        mov r7, #sys_write
        swi #0
        pop {r7,pc}
