; Keep SASM (https://github.com/mras0/sasm) comptabile
; A lot like the x86 version, except LastCFA is kept in bx

        cpu 8086
        org 0x100

NUM_CELLS EQU 4096
RSIZE     EQU 128

F_HIDDEN  EQU 0x80
F_IMMED   EQU 0x40
F_LENMASK EQU 0x1f

O_RDONLY  EQU 0

STDIN_FILENO  EQU -1 ; 0
STDOUT_FILENO EQU -2 ; 1

Main:
        cld

        mov word [OutputFile], STDOUT_FILENO

        ;
        ; Process command line
        ;
        mov si, 0x81
        xor di, di
.SkipSpace:
        lodsb
        cmp al, 13
        je .NoFile
        cmp al, ' '
        jbe .SkipSpace
        mov di, si
        dec di
        xor bx, bx
.FindEnd:
        inc bx
        lodsb
        cmp al, 13
        je .AtEnd
        jmp .FindEnd
.AtEnd:
        mov byte [di+bx], 0

        xor ax, ax
        call OpenFile
        mov [InputFile], ax
        call PutHexByte
        jmp .HasFile
.NoFile:
        mov word [InputFile], STDIN_FILENO
.HasFile:
        mov sp, Cells+NUM_CELLS*2 ; S0
        mov bp, Cells+RSIZE*2     ; R0
        mov si, .Start
        jmp Next
.Start:
        dw Quit

; Filename in DI, mode in AX (ignored for now)
OpenFile:
        mov dx, di
        mov ax, 0x3d00
        int 0x21
        jc .Err
        ret
.Err:
        mov bx, .MsgErrOpen
        call PutZString
        mov bx, di
        call PutZString
        mov al, 10
        call Emit
        mov ax, 0x4c02
        int 0x21
.MsgErrOpen: db 'Error opening file: ', 0

CloseFile:
        mov bx, ax
        mov ax, 0x3e00
        int 0x21
        ret

ReadFile:
        %if 1
        cmp ax, STDIN_FILENO
        jne .Normal
        push cx
        push di
        and cx, cx
        jz .Done
.L:
        xor ah, ah
        int 0x16
        cmp al, 13
        jne .S
        mov al, 10 ; CR -> LF
.S:
        stosb
        call Emit ; Echo
        dec cx
        jnz .L
.Done:
        pop di
        pop ax
        ret
.Normal:
        %endif
        mov bx, ax
        mov ah, 0x3f
        mov dx, di
        int 0x21
        jnc .OK
        xor ax, ax
.OK:    ret

WriteFile:
        %if 1
        cmp ax, STDOUT_FILENO
        jne .Normal

        push cx
        push bp
        and cx, cx
        jz .Done
.L:
        mov al, [di]
        inc di
        push cx
        mov bx, 7
        mov ah, 0x0e
        cmp al, 10
        jne .Pr
        mov al, 13
        int 0x10
        mov bx, 7
        mov ax, 0x0e0a
.Pr:    int 0x10
        pop cx
        dec cx
        jnz .L
.Done:
        pop bp
        pop ax
        ret
.Normal:
        %endif

        mov bx, ax
        mov ah, 0x40
        mov dx, di
        int 0x21
        jnc .OK
        xor ax, ax
.OK:    ret

Emit:
        push bx
        push cx
        push dx
        push di
        push ax
        mov ax, [OutputFile]
        mov di, sp
        mov cx, 1
        call WriteFile
        pop ax
        pop di
        pop dx
        pop cx
        pop bx
        ret

ReadKey:
        push bx
        push di
        sub sp, 2
        mov di, sp
        mov cx, 1
        mov ax, [InputFile]
        call ReadFile
        and ax, ax
        jz .Ret
        mov al, [di]
        cmp al, 'a'
        jb .Ret
        cmp al, 'z'
        ja .Ret
        and al, 0xdf
.Ret:   add sp, 2
        pop di
        pop bx
        ret

Next:
        lodsw
        mov bx, ax
        jmp [bx]

PutZString:
        mov al, [bx]
        and al, al
        jz .Done
        call Emit
        inc bx
        jmp PutZString
.Done:  ret

PutHexWord:
        push ax
        mov al, ah
        call PutHexByte
        pop ax
PutHexByte:
        push ax
        shr al, 1
        shr al, 1
        shr al, 1
        shr al, 1
        call PutHexDigit
        pop ax
PutHexDigit:
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe Emit
        add al, 7
        jmp Emit

ReadWord:
.SkipSpaces:
        call ReadKey
        and al, al
        jz .DoRead
        cmp al, ' '
        jbe .SkipSpaces
        cmp al, 0x5C ; '\'
        jne .DoRead
.SkipToNL:
        call ReadKey
        and al, al
        jz .DoRead
        cmp al, 10
        jne .SkipToNL
        jmp .SkipSpaces
.DoRead:
        mov di, WordBuffer
.ReadLoop:
        cmp al, ' '
        jbe .ReadDone
        stosb
        call ReadKey
        jmp .ReadLoop
.ReadDone:
        mov cx, di
        mov dx, WordBuffer
        sub cx, dx
        xor al, al
        stosb
        mov di, dx
        ret

FindWord:
        mov bx, [Latest]
.Search:
        and bx, bx
        jz .Done
        mov al, [bx+2]
        and al, F_LENMASK|F_HIDDEN
        cmp al, cl
        jne .Next
        push cx
        push si
        push di
        mov si, bx
        add si, 3
        repe cmpsb
        pop di
        pop si
        pop cx
        je .Done
.Next:
        mov bx, [bx]
        jmp .Search
.Done:
        ret

WordCFA:
        xor ah, ah
        mov al, [bx+2]
        and al, F_LENMASK
        add al, 3
        add bx, ax
        ret

ConvertNumber:
        push si
        mov si, [Base]
        xor ax, ax
        and cx, cx
        jz .Err
        xor bh, bh
        mov bl, [di]
        push bx
        cmp bl, '-'
        jne .Cvt
        dec cx
        jz .Err
        inc di
.Cvt:
        mov bl, [di]
        sub bl, '0'
        jb .Err
        cmp bl, 9
        jbe .Add
        sub bl, 'A'-'0'-10
.Add:
        cmp bx, si
        jae .Err
        mul si
        add ax, bx
        inc di
        dec cx
        jnz .Cvt
        pop bx
        cmp bl, '-'
        jne .P
        neg ax
.P:
        pop si
        ret
.Err:
InvalidWord:
        mov bx, .MsgInvalid
        call PutZString
        mov bx, WordBuffer
        call PutZString
        mov bx, .MsgInvalid2
        call PutZString
        call PrintWords
        mov ax, 0x4c01
        int 0x21

.MsgInvalid: db 13, 10, 'Invalid word: "', 0
.MsgInvalid2: db '"', 13, 10, 0

PrintWords:
        mov di, [Latest]
        mov dx, 10 ; Limit count
.Loop:
        and di, di
        jnz .NotDone
        ret
.NotDone:
        mov ax, di
        call PutHexWord
        mov al, ' '
        call Emit
        xor cx, cx
        mov cl, [di+2]
        mov al, cl
        call PutHexByte
        mov al, ' '
        call Emit
        and cl, F_LENMASK
        push di
        add di, 3
.L:
        and cl, cl
        jz .N
        mov al, [di]
        inc di
        call Emit
        dec cl
        jmp .L
.N:
        pop di
        mov al, 10
        call Emit
        mov di, [di]
        dec dx
        jnz .Loop

DoComma:
        mov di, [Here]
        stosw
        mov [Here], di
        ret

W_EMIT:
dw 0
db 4, 'EMIT'
DoEmit:
        dw $+2
        pop ax
        call Emit
        jmp Next

W_KEY:
dw W_EMIT
db 3, 'KEY'
DoKey:
        dw $+2
        call ReadKey
        xor ah, ah
        push ax
        jmp Next

W_EXIT:
dw W_KEY
db 4, 'EXIT'
Exit:
        dw $+2
        mov si, [bp]
        add bp, 2
        jmp Next

W_BRANCH:
dw W_EXIT
db 6, 'BRANCH'
Branch:
        dw $+2
        add si, [si]
        jmp Next

W_ZBRANCH:
dw W_BRANCH
db 7, '0BRANCH'
ZBranch:
        dw $+2
        pop ax
        test ax, ax
        jz Branch+2
        lodsw
        jmp Next

W_EXECUTE:
dw W_ZBRANCH
db 7, 'EXECUTE'
Execute:
        dw $+2
        pop bx
        jmp [bx]

W_LIT:
dw W_EXECUTE
db 3, 'LIT'
Lit:
        dw $+2
        lodsw
        push ax
        jmp Next

W_LITSTRING:
dw W_LIT
db 9, 'LITSTRING'
LitString:
        dw $+2
        lodsw
        push si
        push ax
        add si, ax
        jmp Next

W_CHAR:
dw W_LITSTRING
db 4, 'CHAR'
GetChar:
        dw $+2
        call ReadWord
        xor ax, ax
        mov al, [di]
        push ax
        jmp Next

W_COMMA:
dw W_CHAR
db 1, ','
Comma:
        dw $+2
        pop ax
        call DoComma
        jmp Next

W_OPENFILE:
dw W_COMMA
db 9, 'OPEN-FILE'
DoOpenFile:
        dw $+2
        pop ax
        pop di
        call OpenFile
        push ax
        jmp Next

W_CLOSEFILE:
dw W_OPENFILE
db 10, 'CLOSE-FILE'
DoCloseFile:
        dw $+2
        pop ax
        call CloseFile
        jmp Next

W_READFILE:
dw W_CLOSEFILE
db 9, 'READ-FILE'
DoReadFile:
        dw $+2
        pop ax
        pop cx
        pop di
        call ReadFile
        push ax
        jmp Next

W_WRITEFILE:
dw W_READFILE
db 10, 'WRITE-FILE'
DoWriteFile:
        dw $+2
        pop ax
        pop cx
        pop di
        call WriteFile
        push ax
        jmp Next

W_WORD:
dw W_WRITEFILE
db 4, 'WORD'
GetWord:
        dw $+2
        call ReadWord
        push di
        push cx
        jmp Next

W_CREATE:
dw W_WORD
db 6, 'CREATE'
Create:
        dw $+2
        pop cx
        pop bx
        mov di, [Here]
        mov ax, [Latest]
        mov [Latest], di
        stosw
        mov al, cl
        stosb
        push si
        mov si, bx
        rep movsb
        pop si
        mov [Here], di
        jmp Next

W_FIND:
dw W_CREATE
db 4, 'FIND'
Find:
        dw $+2
        pop cx
        pop di
        call FindWord
        push bx
        jmp Next

W_CFA:
dw W_FIND
db 4, '>CFA'
Cfa:
        dw $+2
        pop bx
        call WordCFA
        push bx
        jmp Next

W_HIDDEN:
dw W_CFA
db 6, 'HIDDEN'
Hidden:
        dw $+2
        pop bx
        xor byte [bx+2], F_HIDDEN
        jmp Next

W_IMMEDIATE:
dw W_HIDDEN
db 9|F_IMMED, 'IMMEDIATE'
Immediate:
        dw $+2
        mov bx, [Latest]
        xor byte [bx+2], F_IMMED
        jmp Next

W_LBRACKET:
dw W_IMMEDIATE
db 1|F_IMMED, '['
LBracket:
        dw $+2
        mov byte [State], 0
        jmp Next

W_RBRACKET:
dw W_LBRACKET
db 1, ']'
RBracket:
        dw $+2
        mov byte [State], 1
        jmp Next
W_TICK:
dw W_RBRACKET
db 1, 0x27 ; '
Tick:
        dw $+2
        lodsw
        push ax
        jmp Next

W_FETCH:
dw W_TICK
db 1, '@'
Fetch:
        dw $+2
        pop bx
        push word [bx]
        jmp Next

W_STORE:
dw W_FETCH
db 1, '!'
Store:
        dw $+2
        pop di
        pop ax
        stosw
        jmp Next

W_CFETCH:
dw W_STORE
db 2, 'C@'
CFetch:
        dw $+2
        pop bx
        xor ah, ah
        mov al, [bx]
        push ax
        jmp Next

W_CSTORE:
dw W_CFETCH
db 2, 'C!'
CStore:
        dw $+2
        pop di
        pop ax
        stosb
        jmp Next

W_DSPFETCH:
dw W_CSTORE
db 4, 'DSP@'
DspFetch:
        dw $+2
        mov ax, sp
        push ax
        jmp Next

W_DSPSTORE:
dw W_DSPFETCH
db 4, 'DSP!'
DspStore:
        dw $+2
        pop ax
        mov sp, ax
        jmp Next

W_RSPFETCH:
dw W_DSPSTORE
db 4, 'RSP@'
RspFetch:
        dw $+2
        push bp
        jmp Next

W_RSPSTORE:
dw W_RSPFETCH
db 4, 'RSP!'
RspStore:
        dw $+2
        pop bp
        jmp Next

W_TOR:
dw W_RSPSTORE
db 2, '>R'
ToR:
        dw $+2
        sub bp, 2
        pop word [bp]
        jmp Next

W_FROMR:
dw W_TOR
db 2, 'R>'
FromR:
        dw $+2
        push word [bp]
        add bp, 2
        jmp Next

W_SWAP:
dw W_FROMR
db 4, 'SWAP'
Swap:
        dw $+2
        pop ax
        pop bx
        push ax
        push bx
        jmp Next

W_ROT:
dw W_SWAP
db 3, 'ROT'
Rot:
        dw $+2
        pop cx
        pop bx
        pop ax
        push bx
        push cx
        push ax
        jmp Next

W_ADD:
dw W_ROT
db 1, '+'
Add:
        dw $+2
        pop bx
        pop ax
        add ax, bx
        push ax
        jmp Next

W_SUB:
dw W_ADD
db 1, '-'
Sub:
        dw $+2
        pop bx
        pop ax
        sub ax, bx
        push ax
        jmp Next

W_MUL:
dw W_SUB
db 1, '*'
Mul:
        dw $+2
        pop bx
        pop ax
        mul bx
        push ax
        jmp Next

W_DIVMOD:
dw W_MUL
db 4, '/MOD'
DivMod:
        dw $+2
        pop bx
        pop ax
        cwd
        idiv bx
        push dx
        push ax
        jmp Next

W_UDIVMOD:
dw W_DIVMOD
db 5, 'U/MOD'
UDivMod:
        dw $+2
        pop bx
        pop ax
        xor dx, dx
        div bx
        push dx
        push ax
        jmp Next

W_AND:
dw W_UDIVMOD
db 3, 'AND'
BAnd:
        dw $+2
        pop bx
        pop ax
        and ax, bx
        push ax
        jmp Next

W_OR:
dw W_AND
db 2, 'OR'
BOr:
        dw $+2
        pop bx
        pop ax
        or ax, bx
        push ax
        jmp Next

W_XOR:
dw W_OR
db 3, 'XOR'
BXor:
        dw $+2
        pop bx
        pop ax
        xor ax, bx
        push ax
        jmp Next

W_INVERT:
dw W_XOR
db 6, 'INVERT'
Invert:
        dw $+2
        mov bx, sp
        not word [bx]
        jmp Next

W_ALIGNED:
dw W_INVERT
db 7, 'ALIGNED'
Aligned:
        dw $+2
        jmp Next

W_EQ:
dw W_ALIGNED
db 1, '='
Eq:
        dw $+2
        pop bx
        pop ax
        xor cx, cx
        cmp ax, bx
        jne .P
        not cx
.P:     push cx
        jmp Next

W_NE:
dw W_EQ
db 2, '<>'
Ne:
        dw $+2
        pop bx
        pop ax
        xor cx, cx
        cmp ax, bx
        je .P
        not cx
.P:     push cx
        jmp Next

W_LT:
dw W_NE
db 1, '<'
Lt:
        dw $+2
        pop bx
        pop ax
        xor cx, cx
        cmp ax, bx
        jnl .P
        not cx
.P:     push cx
        jmp Next

W_LE:
dw W_LT
db 2, '<='
Le:
        dw $+2
        pop bx
        pop ax
        xor cx, cx
        cmp ax, bx
        jg .P
        not cx
.P:     push cx
        jmp Next

W_GE:
dw W_LE
db 2, '>='
Ge:
        dw $+2
        pop bx
        pop ax
        xor cx, cx
        cmp ax, bx
        jl .P
        not cx
.P:     push cx
        jmp Next

W_GT:
dw W_GE
db 1, '>'
Gt:
        dw $+2
        pop bx
        pop ax
        xor cx, cx
        cmp ax, bx
        jng .P
        not cx
.P:     push cx
        jmp Next


W_INTERPRET:
dw W_GT
db 9, 'INTERPRET'
Interpret:
        dw $+2
.Loop:
        call ReadWord
        and cx, cx
        jnz .NotDone
        mov ax, 0x4c00
        int 0x21
.NotDone:
        %if 0
        mov al, [State]
        call PutHexByte
        mov al, ' '
        call Emit
        mov bx, di
        call PutZString
        mov al, 10
        call Emit
        %endif

        push cx
        push di
        call FindWord
        pop di
        pop cx
        and bx, bx
        jz .Number
        test byte [bx+2], F_IMMED
        pushf
        call WordCFA
        popf
        jnz .ExecuteWord
        cmp byte [State], 0
        jz .ExecuteWord
        mov ax, bx
        and ax, ax
        jnz .CompileWord
        jmp InvalidWord
.CompileWord:
        call DoComma
        jmp .Loop
.ExecuteWord:
        jmp [bx]
.Number:
        call ConvertNumber
        push ax
        cmp byte [State], 0
        jz .Loop
        mov ax, Lit
        call DoComma
        pop ax
        call DoComma
        jmp .Loop

DoCol:
        sub bp, 2
        mov [bp], si
        mov si, bx
        add si, 2
        jmp Next

W_QUIT:
dw W_INTERPRET
db 4, 'QUIT'
Quit:
        dw DoCol
        dw Lit, Cells+RSIZE*2, RspStore
        dw Interpret
        dw Branch, -4

W_COLON:
dw W_QUIT
db 1, ':'
Colon:
        dw DoCol
        dw GetWord, Create
        dw Lit, DoCol, Comma
        dw Lit, Latest, Fetch, Hidden
        dw RBracket
        dw Exit

W_SEMICOLON:
dw W_COLON
db 1|F_IMMED, ';'
SemiColon:
        dw DoCol
        dw Lit, Exit, Comma
        dw Lit, Latest, Fetch, Hidden
        dw LBracket
        dw Exit

W_LATEST:
dw W_SEMICOLON
db 6, 'LATEST'
GetLatest:
        dw $+2
        mov ax, Latest
        push ax
        jmp Next

W_HERE:
dw W_LATEST
db 4, 'HERE'
GetHere:
        dw $+2
        mov ax, Here
        push ax
        jmp Next

W_STATE:
dw W_HERE
db 5, 'STATE'
GetState:
        dw $+2
        mov ax, State
        push ax
        jmp Next

W_BASE:
dw W_STATE
db 4, 'BASE'
GetBase:
        dw $+2
        mov ax, Base
        push ax
        jmp Next

W_CELLSIZE:
dw W_BASE
db 9, 'CELL-SIZE'
GetCellSize:
        dw $+2
        mov ax, 2
        push ax
        jmp Next

W_DOCOL:
dw W_CELLSIZE
db 5, 'DOCOL'
DOCOL:
        dw $+2
        mov ax, DoCol
        push ax
        jmp Next

W_S0:
dw W_DOCOL
db 2, 'S0'
GetS0:
        dw $+2
        mov ax, Cells+NUM_CELLS*2 ; S0
        push ax
        jmp Next

W_R0:
dw W_S0
db 2, 'R0'
GetR0:
        dw $+2
        mov ax, Cells+RSIZE*2 ; R0
        push ax
        jmp Next

W_F_HIDDEN:
dw W_R0
db 8, 'F_HIDDEN'
GetFHidden:
        dw $+2
        mov ax, F_HIDDEN
        push ax
        jmp Next

W_F_IMMED:
dw W_F_HIDDEN
db 7, 'F_IMMED'
GetFImmed:
        dw $+2
        mov ax, F_IMMED
        push ax
        jmp Next

W_F_LENMASK:
dw W_F_IMMED
db 9, 'F_LENMASK'
GetFLenmask:
        dw $+2
        mov ax, F_LENMASK
        push ax
        jmp Next

W_O_RDONLY:
dw W_F_LENMASK
db 8, 'O_RDONLY'
GetORdOnly:
        dw $+2
        mov ax, O_RDONLY
        push ax
        jmp Next

W_STDIN:
dw W_O_RDONLY
db 5, 'STDIN'
GetStdin:
        dw $+2
        mov ax, STDIN_FILENO
        push ax
        jmp Next

W_STDOUT:
dw W_STDIN
db 6, 'STDOUT'
GetStdout:
        dw $+2
        mov ax, STDOUT_FILENO
        push ax
        jmp Next

W_DSP:
dw W_STDOUT
db 3, 'DSP'
GetDsp:
        dw $+2
        mov ax, 0
        push ax
        jmp Next

Latest:    dw W_DSP
State:     dw 0
Here:      dw Cells+RSIZE*2 ; R0
Base:      dw 10

InputFile:  resw 1
OutputFile: resw 1
Cells:      resw NUM_CELLS
WordBuffer: resb F_LENMASK+1
