; Inspired by http://git.annexia.org/?p=jonesforth.git
; EBP: Return Stack
; ESP: Data stack
; ESI: Current codeword
; EAX: Last CFA (after "NEXT")

        bits 32

%define NUM_CELLS 4096
%define RSIZE     128
%define S0        (Cells + NUM_CELLS*4)
%define R0        (Cells + RSIZE*4)

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

; NEXT is the central macro of the interpreter
; it loads to next code word address to EAX
; and jumps to it. Every natively defined word
; should end with NEXT.
%macro NEXT 0
        lodsd
        jmp [eax]
%endmacro

;
; Helper macros for defining words in asm


; Macro state, these are updated/consumed on each
; invokation of WORD_HEADER
%xdefine LAST_LINK      0
%xdefine WORD_FLAGS     0

%macro WORD_HEADER 1-2
        %if %0 == 1
                %defstr Name %1
        %else
                %define Name %2
        %endif
        %strlen NameLen Name
        align 4
%%Link:
        dd LAST_LINK
        db NameLen | WORD_FLAGS
        db Name
        align 4
%1:
%xdefine LAST_LINK %%Link
%xdefine WORD_FLAGS 0
%endmacro

%macro NATIVE 1-2
        WORD_HEADER %{1:-1}
        dd %%Code
%%Code:
%endmacro

        section .text

; The system specific files handle low-level initialization
; and provide I/O routines
%ifdef WIN32
%include "sys-win32-x86.asm"
%else
%include "sys-linux-x86.asm"
%endif

; After initialization we jump here and start intepreting
StartInterpreter:
        mov eax, [ArgV]
        mov eax, [eax+4] ; Grab first argument
        and eax, eax
        jz .UseStdin
        mov ecx, O_RDONLY
        call NativeOpenFile
        jmp .HasFile
.UseStdin:
        mov eax, [StdinFile]
.HasFile:
        mov [InputFile], eax
        mov esp, S0
        mov ebp, R0
        mov esi, .Start
        NEXT
.Start:
        dd QUIT

;
; Helper/utility functions for implementing native words
;

; Get next character to EAX
ReadKey:
        push ecx
        push edi
        sub esp, 4
        mov edi, esp
        mov ecx, 1
        mov eax, [InputFile]
        call NativeReadFile
        and eax, eax
        jz .Ret
        xor eax, eax
        mov al, [esp]
.Ret:
        add esp, 4
        pop edi
        pop ecx
        ret

; Put character in EAX
Emit:
        push eax
        mov edi, esp
        mov ecx, 1
        mov eax, [OutputFile]
        call NativeWriteFile
        add esp, 4
        ret

; Emit NUL-terminated message in ESI
EmitZStr:
        lodsb
        and al, al
        jnz .P1
        ret
.P1:
        call Emit
        jmp EmitZStr

; Search for word in ecx/edi, return pointer to header in eax (or NULL)
FindWord:
        push esi
        mov edx, [Latest]
.Loop:
        test edx, edx
        jz .Return
        mov al, [edx+4]
        and al, F_LENMASK|F_HIDDEN ; Include F_HIDDEN so length will never match for these
        cmp cl, al
        jne .Next
        push ecx
        push edi
        lea esi, [edx+5]
        repe cmpsb
        pop edi
        pop ecx
        jne .Next
        jmp .Return
.Next:
        mov edx, [edx]
        jmp .Loop
.Return:
        pop esi
        mov eax, edx
        ret

; Move pointer from word header in EAX to code field
WordCFA:
        xor ecx, ecx
        mov cl, [eax+4]
        and cl, F_LENMASK
        lea eax, [eax+ecx+5+3]
        and eax, -4
        ret

; In:  String [len=ecx, ptr=edi]
; Out: Number [val=eax, unparsed characters ecx]
ConvertNumber:
        xor eax, eax
        and ecx, ecx
        jz .Err
        mov edx, [Base]
        xor ebx, ebx
        mov bl, [edi]
        push ebx
        cmp bl, '-'
        jne .Cvt
        inc edi
        dec ecx
        jnz .Cvt
        pop ebx
.Err:
        mov ecx, -1
        ret
.Cvt:
        mov bl, [edi]
        cmp bl, '0'
        jb .Out
        cmp bl, '9'
        ja .CheckLetter
        sub bl, '0'
        jmp .NextDig
.CheckLetter:
        cmp bl, 'A'
        jb .Out
        cmp bl, 'Z'
        ja .Out
        sub bl, 'A'-10
        jmp .NextDig
.NextDig:
        cmp bl, dl
        jae .Out
        imul eax, edx
        add eax, ebx
        inc edi
        dec ecx
        jnz .Cvt
.Out:
        pop ebx
        cmp bl, '-'
        jne .Ret
        neg eax
.Ret:
        ret

; Read word to WordBuffer, returns length in ecx and pointer in edi
ReadWord:
        xor ecx, ecx
        mov edi, WordBuffer
.SkipSpace:
        call ReadKey
        and al, al
        jz .Done
        cmp al, ' '
        jbe .SkipSpace
        cmp al, '\'
        jne .Read
.SkipToNL:
        call ReadKey
        and al, al
        jz .Done
        cmp al, 10
        jne .SkipToNL
        jmp .SkipSpace
.Read:
        mov [edi+ecx], al
        inc ecx
        call ReadKey
        cmp al, ' '
        ja .Read
.Done:
        mov byte [edi+ecx], 0 ; Ensure NUL terminated for debugging purposes
        ret

; Store word at current cell
DoComma:
        mov edi, [Here]
        stosd
        mov [Here], edi
        ret

;
; Natively implemented words
;

NATIVE EXIT
        ; Pop from return stack
        mov esi, [ebp]
        add ebp, 4
        NEXT

NATIVE BRANCH
        add esi, [esi]
        NEXT

NATIVE ZBRANCH, "0BRANCH"
        pop eax
        test eax, eax
        jz BRANCH+4
        lodsd ; Skip offset
        NEXT

NATIVE EXECUTE
        pop eax
        jmp [eax]

NATIVE LIT
        lodsd
        push eax
        NEXT

NATIVE LITSTRING
        lodsd                   ; Get length
        push esi                ; Push string start address
        push eax                ; And length
        lea esi, [esi+eax+3]    ; Skip string
        and esi, -4             ; Align
        NEXT

NATIVE CHAR
        call ReadWord
        xor eax, eax
        mov al, [edi]
        push eax
        NEXT

NATIVE COMMA, ","
        pop eax
        call DoComma
        NEXT

; ( filename mode -- fd )
; Note: Asciiz filename NOT a forth string
NATIVE OPENFILE, "OPEN-FILE"
        pop ecx
        pop eax
        call NativeOpenFile
        push eax
        NEXT

; ( fd -- )
NATIVE CLOSEFILE, "CLOSE-FILE"
        pop eax
        call NativeCloseFile
        NEXT

; ( addr count fd -- nread )
NATIVE READFILE, "READ-FILE"
        pop eax
        pop ecx
        pop edi
        call NativeReadFile
        push eax
        NEXT

; ( addr count fd -- nwritten )
NATIVE WRITEFILE, "WRITE-FILE"
        pop eax
        pop ecx
        pop edi
        call NativeWriteFile
        push eax
        NEXT

NATIVE EMIT
        pop eax
        call Emit
        NEXT

NATIVE KEY
        call ReadKey
        push eax
        NEXT

NATIVE GETWORD, "WORD"
        call ReadWord
        push edi
        push ecx
        NEXT

NATIVE CREATE
        pop ecx ; Length
        pop ebx ; Name
        mov edi, [Here]
        mov eax, [Latest]
        mov [Latest], edi ; Update latest
        stosd ; Store link
        mov al, cl
        stosb ; Store length
        push esi
        mov esi, ebx
        rep movsb ; Store name
        pop esi
        lea edi, [edi+3]
        and edi, -4 ; Align
        mov [Here], edi ; Update here
        NEXT

NATIVE FIND
        pop ecx
        pop edi
        call FindWord
        push eax
        NEXT

NATIVE CFA, ">CFA"
        pop eax
        call WordCFA
        push eax
        NEXT

NATIVE HIDDEN
        pop eax
        xor byte [eax+4], F_HIDDEN
        NEXT

; Toggle immediate flag of last defined word (or the current one)
%xdefine WORD_FLAGS F_IMMED
NATIVE IMMEDIATE
        mov eax, [Latest]
        xor byte [eax+4], F_IMMED
        NEXT

%xdefine WORD_FLAGS F_IMMED
NATIVE LBRACKET, "["
        mov dword [State], S_IMMEDIATE
        NEXT

NATIVE RBRACKET, "]"
        mov dword [State], S_COMPILE
        NEXT

; Get address of next word and skip it (only works in compiled mode)
NATIVE TICK, "'"
        lodsd
        push eax
        NEXT

; Fetch dword from address at top of stack
NATIVE FETCH, "@"
        pop eax
        push dword [eax]
        NEXT

NATIVE STORE, "!"
        pop edi
        pop eax
        stosd
        NEXT

NATIVE CFETCH, "C@"
        pop edi
        xor eax, eax
        mov al, [edi]
        push eax
        NEXT

NATIVE CSTORE, "C!"
        pop edi
        pop eax
        stosb
        NEXT

NATIVE DSPFETCH, "DSP@"
        push esp
        NEXT

NATIVE DSPSTORE, "DSP!"
        pop esp
        NEXT

NATIVE RSPFETCH, "RSP@"
        push ebp
        NEXT

NATIVE RSPSTORE, "RSP!"
        pop ebp
        NEXT

NATIVE TOR, ">R"
        sub ebp, 4
        pop dword [ebp]
        NEXT

NATIVE FROMR, "R>"
        push dword [ebp]
        add ebp, 4
        NEXT

; ( a b -- b a )
NATIVE SWAP
        pop eax
        pop ebx
        push eax
        push ebx
        NEXT

; ( a b c -- b c a )
NATIVE ROT
        pop ecx
        pop ebx
        pop eax
        push ebx
        push ecx
        push eax
        NEXT

NATIVE ADD, "+"
        pop eax
        add [esp], eax
        NEXT

NATIVE SUB, "-"
        pop eax
        sub [esp], eax
        NEXT

NATIVE MUL, "*"
        pop eax
        pop ebx
        imul eax, ebx
        push eax
        NEXT

; ( x y -- y%x y/x )
NATIVE DIVMOD, "/MOD"
        xor edx, edx
        pop ebx
        pop eax
        idiv ebx
        push edx
        push eax
        NEXT

NATIVE UDIVMOD, "U/MOD"
        xor edx, edx
        pop ebx
        pop eax
        div ebx
        push edx
        push eax
        NEXT

; Bitwise operations
NATIVE BAND, "AND"
        pop eax
        and [esp], eax
        NEXT

NATIVE BOR, "OR"
        pop eax
        or [esp], eax
        NEXT

NATIVE BXOR, "XOR"
        pop eax
        xor [esp], eax
        NEXT

NATIVE INVERT
        not dword [esp]
        NEXT

NATIVE ALIGNED
        pop eax
        add eax, 3
        and eax, -4
        push eax
        NEXT

%macro NATIVECOMPARE 2
NATIVE CMP%1, %2
        pop eax
        pop ebx
        cmp ebx, eax
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
        ; Read next word
        call ReadWord
        ; Done?
        and ecx, ecx
        jnz .NotEmpty
        xor eax, eax
        jmp NativeExit
.NotEmpty:
        ; Is it a known word?
        call FindWord
        and eax, eax
        jz .Number
        ; Check if immediate
        test byte [eax+4], F_IMMED
        ; And save result
        pushf
        ; Get code word address to EAX
        call WordCFA
        popf
        ; If it's an immediate word execute it
        jnz .ExecuteWord
        ; Or if we're in immediate mode
        cmp dword [State], S_IMMEDIATE
        je .ExecuteWord
        ; Compile it
        call DoComma
        jmp .InterpreterLoop
.ExecuteWord:
        jmp [eax]
.Number:
        ; Not a known word, is it a number?
        call ConvertNumber
        and ecx, ecx
        jnz .Error
        ; Yes, push it on data stack
        push eax
        ; And if we're in immediate mode we're done
        cmp dword [State], S_IMMEDIATE
        je .InterpreterLoop
        ; Otherwise compile LIT <number>
        mov eax, LIT
        call DoComma
        pop eax
        call DoComma
        jmp .InterpreterLoop
.Error:
        mov esi, .InvalidWordMsg
        call EmitZStr
        mov esi, WordBuffer
        call EmitZStr
        mov eax, 10
        call Emit
        mov eax, 1
        jmp NativeExit
.InvalidWordMsg: db 'Invalid Word: ', 0

;
; Now there are enough primitives to implement the rest as
; normal forth words.
;

; Non-native forth words have DOCOL as their code word.
; This ensure the current instruction pointer is pushed
; on the return stack and execution continues at the
; following word.
%macro DEFWORD 1-2
        WORD_HEADER %{1:-1}
        dd DOCOL
%endmacro

; Note: DOCOL is a normal label (no indirection)
DOCOL:
        ; Push current instruction pointer to return stack
        sub ebp, 4
        mov [ebp], esi
        lea esi, [eax+4]
        NEXT

DEFWORD QUIT
        dd LIT, R0, RSPSTORE ; Clear return stack
        dd INTERPRET
        dd BRANCH, -8

DEFWORD COLON, ":"
        dd GETWORD, CREATE
        dd LIT, DOCOL, COMMA
        dd LIT, Latest, FETCH, HIDDEN
        dd RBRACKET
        dd EXIT

%xdefine WORD_FLAGS F_IMMED
DEFWORD SEMICOLON, ";"
        dd LIT, EXIT, COMMA
        dd LIT, Latest, FETCH, HIDDEN
        dd LBRACKET
        dd EXIT

;
; Define builtin constants/variables
;

%macro DEFLIT 3
NATIVE %1, %2
        push dword %3
        NEXT
%endmacro

DEFLIT VAR_LATEST      , "LATEST"    , Latest
DEFLIT VAR_STATE       , "STATE"     , State
DEFLIT VAR_HERE        , "HERE"      , Here
DEFLIT VAR_BASE        , "BASE"      , Base
DEFLIT CONST_DOCOL     , "DOCOL"     , DOCOL
DEFLIT CONST_R0        , "R0"        , R0
DEFLIT CONST_S0        , "S0"        , S0
DEFLIT CONST_CELLSIZE  , "CELL-SIZE" , 4
DEFLIT CONST_F_LENMASK , "F_LENMASK" , F_LENMASK
DEFLIT CONST_F_IMMED   , "F_IMMED"   , F_IMMED
DEFLIT CONST_F_HIDDEN  , "F_HIDDEN"  , F_HIDDEN
DEFLIT CONST_O_RDONLY  , "O_RDONLY"  , O_RDONLY
DEFLIT CONST_O_WRONLY  , "O_WRONLY"  , O_WRONLY
DEFLIT CONST_O_RDWR    , "O_RDWR"    , O_RDWR
DEFLIT CONST_O_CREAT   , "O_CREAT"   , O_CREAT
DEFLIT CONST_STDIN     , "STDIN"     , [StdinFile]
DEFLIT CONST_STDOUT    , "STDOUT"    , [StdoutFile]

DEFLIT HACK_DSP        , "DSP"       , 0 ; TODO FIXME
DEFLIT HACK_KERNEL32   , "KERNEL32"  , 0 ; TODO FIXME
DEFLIT HACK_GETPROC    , "GETPROC"   , 0 ; TODO FIXME

        section .data
Latest:         dd LAST_LINK            ; Last defined word
State:          dd S_IMMEDIATE          ; Are we in immediate mode or compiling?
Here:           dd R0                   ; When compiling data is stored at this location (note: sometimes used as scratch buffer)
Base:           dd 10                   ; Current base when printing/reading numbers

        section .bss
ArgC:           resd 1                  ; Command line argument count
ArgV:           resd 1                  ; Command line argument string array (NUL-terimnated and NULL-terminated repsectively)
StdinFile:      resd 1                  ; Standard input file handle
StdoutFile:     resd 1                  ; Standard output file handle
InputFile:      resd 1                  ; KEY reads from here
OutputFile:     resd 1                  ; EMIT goes here
Cells:          resd NUM_CELLS          ; Main storage area (for stacks, words and data)
WordBuffer:     resb F_LENMASK+1        ; Buffer for ReadWord
