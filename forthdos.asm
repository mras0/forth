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

W_EMIT:
dw 0
db 4, 'EMIT'
DoEmit:
        dw $+2
        pop ax
        call Emit
        jmp Next

W_BRANCH:
dw W_EMIT
db 6, 'BRANCH'
Branch:
        dw $+2
        add si, [si]
        jmp Next

W_INTERPRET:
dw W_BRANCH
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
        call WordCFA
        jmp [bx]
.Number:
        call ConvertNumber
        push ax
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

InputData: db '42 EMIT HALT', 0
InputText: dw InputData
Latest:    dw W_QUIT
Base:      dw 10

WordBuffer: resw F_LENMASK+1
Cells:      resw NUM_CELLS
