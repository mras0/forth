        global mainCRTStartup

        extern GetStdHandle
        extern CreateFileA
        extern CloseHandle
        extern ReadFile
        extern WriteFile
        extern GetCommandLineW
        extern CommandLineToArgvW
        extern ExitProcess

%define GENERIC_READ            0x80000000
%define GENERIC_WRITE           0x40000000
%define FILE_SHARE_READ         0x00000001
%define CREATE_ALWAYS           2
%define OPEN_EXISTING           3
%define STD_INPUT_HANDLE        -10
%define STD_OUTPUT_HANDLE       -11

mainCRTStartup:
        ; Save initial stack for use when calling Win32 API functions
        ; Note: We're still skirting the rules as we don't provide
        ; unwind tables / ensure alignment etc..
        mov [rel OldRSP], rsp
        sub rsp, 0x28

        mov rcx, STD_OUTPUT_HANDLE
        call GetStdHandle
        mov [rel StdoutFile], rax

        mov rcx, STD_INPUT_HANDLE
        call GetStdHandle
        mov [rel StdinFile], rax

        call GetCommandLineW
        mov rdx, ArgC           ; pNumArgs
        mov rcx, rax            ; lpCmdLine
        call CommandLineToArgvW
        mov [rel ArgV], rax

        ; Lame wide->narrow conversion
        mov rcx, [rel ArgC]
        mov rbx, rax
.CvtLoop:
        and rcx, rcx
        jz .CvtDone
        mov rsi, [rbx]
        mov rdi, rsi
.Cvt:
        lodsw
        stosb
        and al, al
        jnz .Cvt
        add rbx, 8
        dec rcx
        jmp .CvtLoop
.CvtDone:

        add rsp, 0x28
        jmp StartInterpreter

NativeExit:
        xchg rsp, [rel OldRSP]
        sub rsp, 0x28
        mov rcx, rax
        call ExitProcess
        int3

NativeOpen:
        xchg rsp, [rel OldRSP]
        sub rsp, 0x48
        mov qword [rsp+0x30], 0 ; hTemplateFile
        mov qword [rsp+0x28], 0 ; dwFlagsAndAttributes
        mov rdx, OPEN_EXISTING
        test ecx, O_CREAT
        jz .L1
        mov rdx, CREATE_ALWAYS
.L1:
        mov qword [rsp+0x20], rdx ; dwCreationDisposition
        mov r9, 0               ; lpSecurityAttributes
        and ecx, 3
        cmp ecx, O_RDWR
        je .L2
        cmp ecx, O_WRONLY
        je .L3
        mov r8, FILE_SHARE_READ ; dwShareMode
        mov rdx, GENERIC_READ   ; dwDesiredAccess
        jmp .L4
.L2: ; RDWR
        mov r8, 0               ; dwShareMode
        mov rdx, GENERIC_READ|GENERIC_WRITE ; dwDesiredAccess
        jmp .L4
.L3: ; WRONLY
        mov r8, 0               ; dwShareMode
        mov rdx, GENERIC_WRITE  ; dwDesiredAccess
.L4:
        mov rcx, rax            ; lpFileName
        call CreateFileA
        add rsp, 0x48
        xchg rsp, [rel OldRSP]
        ret

NativeClose:
        xchg rsp, [rel OldRSP]
        sub rsp, 0x28
        mov rcx, rax
        call CloseHandle
        add rsp, 0x28
        xchg rsp, [rel OldRSP]
        ret

NativeReadFile:
        xchg rsp, [rel OldRSP]
        sub rsp, 0x38
        mov qword [rsp+0x20], 0 ; lpOverlapped
        lea r9, [rsp+0x28]      ; lpNumberOfBytesWritten
        mov r8, rcx             ; nNumberOfBytesToWrite
        mov rdx, rdi            ; lpBuffer
        mov rcx, rax            ; hFile
        call ReadFile
        mov rax, [rsp+0x28]
        add rsp, 0x38
        xchg rsp, [rel OldRSP]
        ret

NativeWriteFile:
        xchg rsp, [rel OldRSP]
        sub rsp, 0x38
        mov qword [rsp+0x20], 0 ; lpOverlapped
        lea r9, [rsp+0x28]      ; lpNumberOfBytesWritten
        mov r8, rcx             ; nNumberOfBytesToWrite
        mov rdx, rdi            ; lpBuffer
        mov rcx, rax            ; hFile
        call WriteFile
        mov rax, [rsp+0x28]
        add rsp, 0x38
        xchg rsp, [rel OldRSP]
        ret

        [section .bss]
OldRSP:         resq 1
        __SECT__
