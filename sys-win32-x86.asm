        global _mainCRTStartup

        extern _CreateFileA@28
        extern _CloseHandle@4
        extern _ReadFile@20
        extern _WriteFile@20
        extern _GetStdHandle@4
        extern _ExitProcess@4
        extern _GetCommandLineW@0
        extern _CommandLineToArgvW@8

%define GENERIC_READ            0x80000000
%define GENERIC_WRITE           0x40000000
%define FILE_SHARE_READ         0x00000001
%define CREATE_ALWAYS           2
%define OPEN_EXISTING           3
%define STD_INPUT_HANDLE        -10
%define STD_OUTPUT_HANDLE       -11

; Program entry point
_mainCRTStartup:
        ; Get console handle
        push STD_OUTPUT_HANDLE
        call _GetStdHandle@4
        mov [OutputFile], eax
        mov [StdoutFile], eax

        ; And stdin
        push STD_INPUT_HANDLE
        call _GetStdHandle@4
        mov [StdinFile], eax

        ; Get command line arguments
        call _GetCommandLineW@0
        push ArgC ; pNumArgs
        push eax  ; lpCmdLine
        call _CommandLineToArgvW@8
        mov [ArgV], eax

        ; Convert argument strings in-place, assuming roughly latin-1 characters
        ; TODO: Proper handling of unicode characters
        mov ecx, [ArgC]
        mov ebx, eax
.CvtLoop:
        and ecx, ecx
        jz .CvtDone
        mov esi, [ebx]
        mov edi, esi
.Cvt:
        lodsw
        stosb
        and al, al
        jnz .Cvt
        add ebx, 4 ; Next arguments
        dec ecx
        jmp .CvtLoop
.CvtDone:

        jmp StartInterpreter

; Exit with error code in EAX
NativeExit:
        push eax
        call _ExitProcess@4

; Read ECX bytes from EAX to EDI
; Returns number of bytes read in EAX
NativeReadFile:
        push ebp
        mov ebp, esp
        sub esp, 4
        push 0          ; lpOverlapped
        lea  edx, [ebp-4]
        push edx        ; lpNumberOfBytesRead
        push ecx        ; nNumberOfBytesToRead
        push edi        ; lpBuffer
        push eax        ; hFile
        call _ReadFile@20
        mov eax, [ebp-4]
        add esp, 4
        pop ebp
        ret

; Write ECX bytes from EDI to EAX
; Returns number of bytes written in EAX
NativeWriteFile:
        push ebp
        mov ebp, esp
        sub esp, 4
        push 0          ; lpOverlapped
        lea edx, [ebp-4]
        push edx        ; lpNumberOfBytesWritten
        push ecx        ; nNumberOfBytesToWrite
        push edi        ; lpBuffer
        push eax        ; hFile
        call _WriteFile@20
        mov eax, [ebp-4]
        add esp, 4
        pop ebp
        ret

; Open file with name in EAX and mode (O_RDONLY,...) in ECX
NativeOpenFile:
        push 0                  ; hTemplateFile
        push 0                  ; dwFlagsAndAttributes
        test ecx, O_CREAT
        jnz .L1
        push OPEN_EXISTING      ; dwCreationDisposition
        jmp .L2
.L1:
        push CREATE_ALWAYS      ; dwCreationDisposition
.L2:
        push 0                  ; lpSecurityAttributes
        and ecx, 3
        cmp ecx, O_RDWR
        je .L3
        cmp ecx, O_WRONLY
        je .L4
        push FILE_SHARE_READ    ; dwShareMode
        push GENERIC_READ       ; dwDesiredAccess
        jmp .L5
.L3:    ; RDWR
        push 0                  ; dwShareMode
        push GENERIC_READ|GENERIC_WRITE       ; dwDesiredAccess
        jmp .L5
.L4:    ; WRONLY
        push 0                  ; dwShareMode
        push GENERIC_WRITE      ; dwDesiredAccess
.L5:
        push eax                ; lpFileName
        call _CreateFileA@28
        ret

NativeCloseFile:
        push eax
        call _CloseHandle@4
        ret
