        global _start

%define sys_exit        1       ; int sys_exit(int status)
%define sys_read        3       ; ssize_t sys_read(unsigned int fd, char* buf, size_t count)
%define sys_write       4       ; ssize_t sys_write(unsigned int fd, char* buf, size_t count)
%define sys_open        5       ; int sys_open(const char* filename, int flags, int mode)
%define sys_close       6       ; int sys_close(unsigned int fd)

%define STDIN_FILENO    0
%define STDOUT_FILENO   1

_start:
        pop dword [ArgC]
        mov [ArgV], esp

        mov dword [OutputFile], STDOUT_FILENO
        mov dword [StdinFile], STDIN_FILENO
        mov dword [StdoutFile], STDOUT_FILENO

        jmp StartInterpreter

NativeExit:
        mov ebx, eax
        mov eax, sys_exit
        int 0x80

NativeOpenFile:
        mov ebx, eax
        mov eax, sys_open
        mov edx, 0
        int 0x80
        ret

NativeCloseFile:
        mov ebx, eax
        mov eax, sys_close
        int 0x80

NativeReadFile:
        mov ebx, eax
        mov edx, ecx
        mov ecx, edi
        mov eax, sys_read
        int 0x80
        ret

NativeWriteFile:
        mov ebx, eax
        mov edx, ecx
        mov ecx, edi
        mov eax, sys_write
        int 0x80
        ret
