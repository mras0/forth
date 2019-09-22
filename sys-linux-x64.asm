        global _start

%define sys_read  0
%define sys_write 1
%define sys_open  2
%define sys_close 3
%define sys_exit  60

%define STDIN_FILENO    0
%define STDOUT_FILENO   1

        [section .bss]
OldRSP: resq 1
        __SECT__

_start:
        mov [rel OldRSP], rsp
        mov rdi, [rsp]
        lea rsi, [rsp+8]
        mov [rel ArgC], rdi
        mov [rel ArgV], rsi

        mov qword [rel StdinFile], STDIN_FILENO
        mov qword [rel StdoutFile], STDOUT_FILENO

        jmp StartInterpreter

NativeExit:
        mov rdi, rax
        mov rax, sys_exit
        syscall
        int3

NativeOpen:
        push rsi
        push rdi
        mov rdi, rax
        mov rsi, rcx
        xor rdx, rdx
        mov rax, sys_open
        syscall
        pop rdi
        pop rsi
        ret

NativeClose:
        push rsi
        push rdi
        mov rdi, rax
        mov rax, sys_close
        syscall
        pop rdi
        pop rsi
        ret

NativeReadFile:
        push rsi
        push rdi
        mov rsi, rdi
        mov rdi, rax
        mov rdx, rcx
        mov rax, sys_read
        syscall
        pop rdi
        pop rsi
        ret

NativeWriteFile:
        push rsi
        push rdi
        mov rsi, rdi
        mov rdi, rax
        mov rdx, rcx
        mov rax, sys_write
        syscall
        pop rdi
        pop rsi
        ret
