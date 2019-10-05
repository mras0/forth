; Keep SASM (https://github.com/mras0/sasm) comptabile
; A lot like the x86 version, except LastCFA is kept in bx

        cpu 8086
        org 0x100

NUM_CELLS EQU 4096
RSIZE     EQU 128

F_HIDDEN  EQU 0x80
F_IMMED   EQU 0x40
F_LENMASK EQU 0x1f

Main:
        cld
        call PrintWords

        mov sp, Cells+NUM_CELLS*2 ; S0
        mov bp, Cells+RSIZE*2     ; R0
        mov si, .Start
        jmp Next
.Start:
        dw Quit

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

Emit:
        push ax
        push bx
        push bp
        mov bx, 7
        mov ah, 0x0e
        cmp al, 10
        jne .Pr
        mov al, 13
        int 0x10
        mov al, 10
.Pr:    int 0x10
        pop bp
        pop bx
        pop ax
        ret

ReadKey:
        push bx
        mov bx, [InputText]
        mov al, [bx]
        and al, al
        jz .Ret
        cmp al, 'a'
        jb .L
        cmp al, 'z'
        ja .L
        and al, 0xdf
.L:
        inc bx
        mov [InputText], bx
.Ret:   pop bx
        ret

ReadWord:
.SkipSpaces:
        call ReadKey
        and al, al
        jz .DoRead
        cmp al, ' '
        jbe .SkipSpaces
        cmp al, 0x5C ; '\'
        jne .DoRead
        call ReadKey
        and al, al
        jz .DoRead
        cmp al, 10
        je .SkipSpaces
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
        mov bx, .MsgInvalid
        call PutZString
        mov bx, WordBuffer
        call PutZString
        mov bx, .MsgInvalid2
        call PutZString
        mov ax, 0x4c01
        int 0x21

.MsgInvalid: db 10, 'Invalid number: "', 0
.MsgInvalid2: db '"', 10, 0

PrintWords:
        mov di, [Latest]
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
        jmp .Loop

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

W_EXIT:
dw W_EMIT
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

W_LIT:
dw W_BRANCH
db 3, 'LIT'
Lit:
        dw $+2
        lodsw
        push ax
        jmp Next

W_COMMA:
dw W_LIT
db 1, ','
Comma:
        dw $+2
        pop ax
        call DoComma
        jmp Next

W_WORD:
dw W_COMMA
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

W_HIDDEN:
dw W_CREATE
db 6, 'HIDDEN'
Hidden:
        dw $+2
        pop bx
        xor byte [bx+2], F_HIDDEN
        jmp Next

W_LBRACKET:
dw W_HIDDEN
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

W_FETCH:
dw W_RBRACKET
db 1, '@'
Fetch:
        dw $+2
        pop bx
        push word [bx]
        jmp Next

W_INTERPRET:
dw W_FETCH
db 9, 'INTERPRET'
Interpret:
        dw $+2
.Loop:
        call ReadWord
        and cx, cx
        jnz .NotDone
        call PrintWords
        mov ax, 0x4c00
        int 0x21
.NotDone:
        mov al, [State]
        call PutHexByte
        mov al, ' '
        call Emit
        mov bx, di
        call PutZString
        mov al, 10
        call Emit

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
        ; dw Lit, R0, RSPSTORE
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

Latest:    dw W_SEMICOLON
State:     dw 0
Here:      dw Cells+RSIZE*2 ; R0
Base:      dw 10

InputData: db '33 EMIT : STAR 42 EMIT ; STAR', 0
InputText: dw InputData

Cells:      resw NUM_CELLS
WordBuffer: resb F_LENMASK+1
