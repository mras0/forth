        bits 64

        global Latest
        global Base
        global State
        global InputFile
        global OutputFile
        global WordBuffer
        global StdoutFile

%define NUM_CELLS 4096
%define RSIZE     128
%define S0        (Cells + NUM_CELLS*8)
%define R0        (Cells + RSIZE*8)

%define S_IMMEDIATE     0x00
%define S_COMPILE       0x01

%define F_HIDDEN        0x80
%define F_IMMED         0x40
%define F_LENMASK       0x1F

; Chosen to match Linux values
%define O_RDONLY        0
%define O_WRONLY        1
%define O_RDWR          2
%define O_CREAT         0x40

%macro NEXT 0
        lodsq
        jmp [rax]
%endmacro

%xdefine LAST_LINK      0
%xdefine WORD_FLAGS     0

%macro WORD_HEADER 1-2
        %if %0 == 1
                %defstr Name %1
        %else
                %define Name %2
        %endif
        %strlen NameLen Name
        align 8
%%Link:
        dq LAST_LINK
        db NameLen | WORD_FLAGS
        db Name
        align 8
%1:
%xdefine LAST_LINK %%Link
%xdefine WORD_FLAGS 0
%endmacro

%macro NATIVE 1-2
        WORD_HEADER %{1:-1}
        dq %%Code
%%Code:
%endmacro

        section .text

%ifdef WIN32
%include "sys-win32-x64.asm"
%else
%include "sys-linux-x64.asm"
%endif

; After initialization we jump here and start intepreting
StartInterpreter:
        mov rax, [rel StdoutFile]
        mov [rel OutputFile], rax
        mov rax, [rel StdinFile]
        mov [rel InputFile], rax

        mov rax, [rel ArgV]
        mov rax, [rax+8]
        and rax, rax
        jz .HasFile
        mov rcx, O_RDONLY
        call NativeOpen
        ; TODO: Check return value
        mov [rel InputFile], rax
.HasFile:

        mov rsp, S0
        mov rbp, R0
        mov rsi, .Start
        NEXT
.Start:
        dq QUIT

ReadKey:
        push rcx
        push rdi
        sub rsp, 8
        mov rdi, rsp
        mov rcx, 1
        mov rax, [rel InputFile]
        call NativeReadFile
        and eax, eax
        jz .Ret
        xor rax, rax
        mov al, [rsp]
.Ret:
        add rsp, 8
        pop rdi
        pop rcx
        ret

ReadWord:
        xor rcx, rcx
        mov rdi, WordBuffer
.SkipSpaces:
        call ReadKey
        and al, al
        jz .Done
        cmp al, ' '
        jbe .SkipSpaces
        cmp al, '\'
        jne .Read
.SkipToNL:
        call ReadKey
        and al, al
        jz .Done
        cmp al, 10
        jne .SkipToNL
        jmp .SkipSpaces
.Read:
        mov [rdi+rcx], al
        inc rcx
        call ReadKey
        cmp al, ' '
        ja .Read
.Done:
        mov byte [rdi+rcx], 0
        ret

FindWord:
        push rsi
        mov rdx, [rel Latest]
.Loop:
        test rdx, rdx
        jz .Return
        mov al, [rdx+8]
        and al, F_LENMASK|F_HIDDEN
        cmp cl, al
        jne .Next
        push rcx
        push rdi
        lea rsi, [rdx+9]
        repe cmpsb
        pop rdi
        pop rcx
        je .Return
.Next:
        mov rdx, [rdx]
        jmp .Loop
.Return:
        pop rsi
        mov rax, rdx
        ret

ConvertNumber:
        xor rax, rax
        and rcx, rcx
        jz .Err
        mov rdx, [rel Base]
        xor rbx, rbx
        mov bl, [rdi]
        push rbx
        cmp bl, '-'
        jne .Cvt
        inc rdi
        dec ecx
        jnz .Cvt
        pop rbx
.Err:
        mov rcx, -1
        ret
.Cvt:
        mov bl, [rdi]
        cmp bl, '0'
        jb .Out
        cmp bl, '9'
        ja .Letter
        sub bl, '0'
        jmp .NextDig
.Letter:
        cmp bl, 'A'
        jb .Out
        cmp bl, 'Z'
        ja .Out
        sub bl, 'A'-10
.NextDig:
        cmp bl, dl
        jae .Out
        imul rax, rdx
        add rax, rbx
        inc rdi
        dec rcx
        jnz .Cvt
.Out:
        pop rbx
        cmp bl, '-'
        jne .Ret
        neg rax
.Ret:
        ret

Emit:
        push rax
        mov rdi, rsp
        mov rcx, 1
        mov rax, [rel OutputFile]
        call NativeWriteFile
        add rsp, 8
        ret

EmitZStr:
        lodsb
        and al, al
        jnz .P1
        ret
.P1:
        call Emit
        jmp EmitZStr

WordCFA:
        xor rcx, rcx
        mov cl, [rax+8]
        and cl, F_LENMASK
        lea rax, [rax+rcx+9+7]
        and rax, -8
        ret

DoComma:
        mov rdi, [rel Here]
        stosq
        mov [rel Here], rdi
        ret

NATIVE EXIT
        mov rsi, [rbp]
        add rbp, 8
        NEXT

NATIVE BRANCH
        add rsi, [rsi]
        NEXT

NATIVE ZBRANCH, "0BRANCH"
        pop rax
        and rax, rax
        jz BRANCH+8
        lodsq
        NEXT

NATIVE EXECUTE
        pop rax
        jmp [rax]

NATIVE LIT
        lodsq
        push rax
        NEXT

NATIVE LITSTRING
        lodsq
        push rsi
        push rax
        lea rsi, [rsi+rax+7]
        and rsi, -8
        NEXT

NATIVE CHAR
        call ReadWord
        xor rax, rax
        mov al, [rdi]
        push rax
        NEXT

NATIVE COMMA, ","
        pop rax
        call DoComma
        NEXT

NATIVE EMIT
        pop rax
        call Emit
        NEXT

NATIVE KEY
        call ReadKey
        push rax
        NEXT

NATIVE OPENFILE, "OPEN-FILE"
        pop rcx
        pop rax
        call NativeOpen
        push rax
        NEXT

NATIVE CLOSEFILE, "CLOSE-FILE"
        pop rax
        call NativeClose
        NEXT

NATIVE READFILE, "READ-FILE"
        pop rax
        pop rcx
        pop rdi
        call NativeReadFile
        push rax
        NEXT

NATIVE WRITEFILE, "WRITE-FILE"
        pop rax
        pop rcx
        pop rdi
        call NativeWriteFile
        push rax
        NEXT

NATIVE GETWORD, "WORD"
        call ReadWord
        push rdi
        push rcx
        NEXT

NATIVE CREATE
        pop rcx
        pop rbx
        mov rdi, [rel Here]
        mov rax, [rel Latest]
        mov [rel Latest], rdi
        stosq
        mov al, cl
        stosb
        push rsi
        mov rsi, rbx
        rep movsb
        pop rsi
        lea rdi, [rdi+7]
        and rdi, -8
        mov [rel Here], rdi
        NEXT

NATIVE FIND
        pop rcx
        pop rdi
        call FindWord
        push rax
        NEXT

NATIVE CFA, ">CFA"
        pop rax
        call WordCFA
        push rax
        NEXT

NATIVE HIDDEN
        pop rax
        xor byte [rax+8], F_HIDDEN
        NEXT

%define WORD_FLAGS F_IMMED
NATIVE IMMEDIATE
        mov rax, [rel Latest]
        xor byte [rax+8], F_IMMED
        NEXT

%define WORD_FLAGS F_IMMED
NATIVE LBRACKET, "["
        mov qword [rel State], S_IMMEDIATE
        NEXT

NATIVE RBRACKET, "]"
        mov qword [rel State], S_COMPILE
        NEXT

NATIVE TICK, "'"
        lodsq
        push rax
        NEXT

NATIVE FETCH, "@"
        pop rax
        push qword [rax]
        NEXT

NATIVE STORE, "!"
        pop rdi
        pop rax
        stosq
        NEXT

NATIVE CFETCH, "C@"
        pop rdi
        xor rax, rax
        mov al, [rdi]
        push rax
        NEXT

NATIVE CSTORE, "C!"
        pop rdi
        pop rax
        stosb
        NEXT

NATIVE DSPFETCH, "DSP@"
        push rsp
        NEXT

NATIVE DSPSTORE, "DSP!"
        pop rsp
        NEXT

NATIVE RSPFETCH, "RSP@"
        push rbp
        NEXT

NATIVE RSPSTORE, "RSP!"
        pop rbp
        NEXT

NATIVE TOR, ">R"
        sub rbp, 8
        pop qword [rbp]
        NEXT

NATIVE FROMR, "R>"
        push qword [rbp]
        add rbp, 8
        NEXT

NATIVE SWAP
        pop rax
        pop rbx
        push rax
        push rbx
        NEXT

NATIVE ROT
        pop rcx
        pop rbx
        pop rax
        push rbx
        push rcx
        push rax
        NEXT

NATIVE ADD, "+"
        pop rax
        add [rsp], rax
        NEXT

NATIVE SUB, "-"
        pop rax
        sub [rsp], rax
        NEXT

NATIVE MUL, "*"
        pop rax
        pop rbx
        imul rax, rbx
        push rax
        NEXT

NATIVE DIVMOD, "/MOD"
        xor rdx, rdx
        pop rbx
        pop rax
        idiv rbx
        push rdx
        push rax
        NEXT

NATIVE UDIVMOD, "U/MOD"
        xor rdx, rdx
        pop rbx
        pop rax
        div rbx
        push rdx
        push rax
        NEXT

NATIVE BAND, "AND"
        pop rax
        and [rsp], rax
        NEXT

NATIVE BOR, "OR"
        pop rax
        or [rsp], rax
        NEXT

NATIVE BXOR, "XOR"
        pop rax
        xor [rsp], rax
        NEXT

NATIVE INVERT
        not qword [rsp]
        NEXT

%macro NATIVECOMPARE 2
NATIVE CMP%1, %2
        pop rax
        pop rbx
        cmp rbx, rax
        j%1 %%True
        push 0
        jmp %%Done
%%True:
        push -1
%%Done:
        NEXT
%endmacro

NATIVECOMPARE   E  , "="
NATIVECOMPARE   NE , "<>"
NATIVECOMPARE   L  , "<"
NATIVECOMPARE   G  , ">"
NATIVECOMPARE   LE , "<="
NATIVECOMPARE   GE , ">="

NATIVE INTERPRET
.InterpreterLoop:
        call ReadWord
        and ecx, ecx
        jnz .NotEmpty
        xor eax, eax
        jmp NativeExit
.NotEmpty:
        call FindWord
        and rax, rax
        jz .Number
        test byte [rax+8], F_IMMED
        pushf
        call WordCFA
        popf
        jnz .ExecuteWord
        cmp qword [rel State], S_IMMEDIATE
        je .ExecuteWord
        call DoComma
        jmp .InterpreterLoop
.ExecuteWord:
        jmp [rax]
.Number:
        call ConvertNumber
        and ecx, ecx
        jnz .Error
        push rax
        cmp qword [rel State], S_IMMEDIATE
        je .InterpreterLoop
        mov rax, LIT
        call DoComma
        pop rax
        call DoComma
        jmp .InterpreterLoop
.Error:
        mov rsi, .InvalidWordMsg
        call EmitZStr
        mov rsi, WordBuffer
        call EmitZStr
        mov eax, 10
        call Emit
        mov eax, 1
        jmp NativeExit
.InvalidWordMsg: db 'Invalid Word: ', 0

DOCOL:
        sub rbp, 8
        mov [rbp], rsi
        lea rsi, [rax+8]
        NEXT

%macro DEFWORD 1-2
        WORD_HEADER %{1:-1}
        dq DOCOL
%endmacro

DEFWORD QUIT
        dq LIT, R0, RSPSTORE ; Clear return stack
        dq INTERPRET
        dq BRANCH, -16

DEFWORD COLON, ":"
        dq GETWORD, CREATE
        dq LIT, DOCOL, COMMA
        dq LIT, Latest, FETCH, HIDDEN
        dq RBRACKET
        dq EXIT

%xdefine WORD_FLAGS F_IMMED
DEFWORD SEMICOLON, ";"
        dq LIT, EXIT, COMMA
        dq LIT, Latest, FETCH, HIDDEN
        dq LBRACKET
        dq EXIT

%macro DEFLIT 3
NATIVE %1, %2
        mov rax, %3
        push rax
        NEXT
%endmacro

DEFLIT VAR_LATEST      , "LATEST"    , Latest
DEFLIT VAR_STATE       , "STATE"     , State
DEFLIT VAR_HERE        , "HERE"      , Here
DEFLIT VAR_BASE        , "BASE"      , Base
DEFLIT CONST_DOCOL     , "DOCOL"     , DOCOL
DEFLIT CONST_R0        , "R0"        , R0
DEFLIT CONST_S0        , "S0"        , S0
DEFLIT CONST_CELLSIZE  , "CELL-SIZE" , 8
DEFLIT CONST_F_LENMASK , "F_LENMASK" , F_LENMASK
DEFLIT CONST_F_IMMED   , "F_IMMED"   , F_IMMED
DEFLIT CONST_F_HIDDEN  , "F_HIDDEN"  , F_HIDDEN
DEFLIT CONST_O_RDONLY  , "O_RDONLY"  , O_RDONLY
DEFLIT CONST_O_WRONLY  , "O_WRONLY"  , O_WRONLY
DEFLIT CONST_O_RDWR    , "O_RDWR"    , O_RDWR
DEFLIT CONST_O_CREAT   , "O_CREAT"   , O_CREAT
DEFLIT CONST_STDIN     , "STDIN"     , [rel StdinFile]
DEFLIT CONST_STDOUT    , "STDOUT"    , [rel StdoutFile]

DEFLIT HACK_DSP        , "DSP"       , 0 ; TODO FIXME
DEFLIT HACK_KERNEL32   , "KERNEL32"  , 0 ; TODO FIXME
DEFLIT HACK_GETPROC    , "GETPROC"   , 0 ; TODO FIXME

        section .data
Latest:         dq LAST_LINK
State:          dq S_IMMEDIATE
Here:           dq R0
Base:           dq 10

        section .bss
ArgC:           resq 1
ArgV:           resq 1
StdinFile:      resq 1
StdoutFile:     resq 1
InputFile:      resq 1
OutputFile:     resq 1
Cells:          resq NUM_CELLS
WordBuffer:     resb F_LENMASK+1
