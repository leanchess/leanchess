; LeanChess
;
; Copyright (c) 2019 Dmitry Shechtman
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

.model tiny
.186
code segment
    org 100h
    assume cs: code
    
start:
    int 10h                                    ;BIOS display mode 0
    mov cl, 13                                 ;Set row count
    mov si, offset init_db                     ;Set row metadata address
    mov di, offset board_db                    ;Set board address

init_loop:
    push cx                                    ;Save row counter
    mov cl, 8                                  ;Set square counter
    lodsb                                      ;Read one byte
    test al, 80h                               ;First rank?
    jz init_cont                               ;No, proceed to write row
    dec si                                     ;Back one byte
    rep movsb                                  ;Copy row
    
init_cont:
    rep stosb                                  ;Write row
    mov al, 08h                                ;Border square
    mov cl, 8                                  ;Set square counter
    rep stosb                                  ;Write border
    pop cx                                     ;Restore row counter
    loop init_loop                             ;Move to next row

init_done:
    push di                                    ;Save buffer address

main_loop:
    mov cl, 74                                 ;Ranks * (files + 1) + LF * 2
    mov si, offset board_db + 20h              ;Grab preceding LF * 2
    pop di                                     ;Restore buffer address
    mov dx, di                                 ;Clone buffer address

disp_loop:
    lodsb                                      ;Read square contents
    test al, 08h                               ;Border?
    jz disp_cont                               ;No, proceed to write square
    add si, 7                                  ;Skip border squares

disp_cont:
    push ax                                    ;Save square contents
    and al, 0Fh                                ;Isolate piece type/border
    mov bx, offset disp_db                     ;Set base chars' address
    xlat                                       ;Translate piece type to char
    pop bx                                     ;Restore square contents
    and bl, 20h                                ;Isolate black/lowercase
    or al, bl                                  ;Apply case
    stosb                                      ;Write square
    loop disp_loop                             ;Move to next square
    
disp_done:
    mov ax, 0924h                              ;String terminator/write string
    stosb                                      ;Write string terminator
    int 21h                                    ;BIOS output function

play:
    push dx                                    ;Save buffer address
    mov dx, 1828h                              ;Set player's and opponent's colors
    mov cl, 3                                  ;Set search depth
    push offset main_loop                      ;Repeat forever
    push offset auto_move_sub                  ;Perform computer's move
    push offset move_sub                       ;Perform human's move
    push offset read_sub                       ;Read destination square

;Read square from input
;Output:
;  DI - Square address
read_sub:
    mov bp, di                                 ;Clone address
    mov di, offset board_db + 0AEh - 15Fh      ;Bottom right corner + margin - ASCII offset
    mov ah, 01h                                ;Read character
    int 21h                                    ;DOS I/O function
    add di, ax                                 ;Add result to base address
    int 21h                                    ;DOS I/O function
    and ax, 0Fh                                ;Isolate number (expected 1-8)
    shl ax, 4                                  ;Multiply by 16
    sub di, ax                                 ;Subtract result from base address
    ret
read_end:

;Calculate gain
;Input:
;  AL - Piece
;  CH - Total gain
;Output:
;  AL - Piece type
;  CH - Updated total gain
gain_sub:
    and al, 07h                                ;Isolate piece type
    push ax                                    ;Save piece type
    push bx                                    ;Save player's and opponent's colors
    mov bx, offset eval_db                     ;Set base values' address
    xlat                                       ;Get piece's value
    add ch, al                                 ;Add to total gain
    pop bx                                     ;Restore player's and opponent's colors
    pop ax                                     ;Restore piece type
    ret
gain_end:

;Evaluate move
;Input:
;  AH - Promoted piece type
;  BL - Source piece type
;  BH - Source square contents + moved
;  CX - Destination counter
;  DL - Player's color + border
;  DH - Opponent's color + border
;  BP - Source square address
;  DI - Destination square address
;Output:
;  AH - Promoted piece type
;  BL - Source piece type
;  BH - Source square contents + moved
;  CX - Destination counter
;  DL - Player's color + border
;  DH - Opponent's color + border
;  BP - Source square address
;  DI - Destination square address
;  ZF = 0 if OK to move to next destination
eval_sub:
    pusha                                      ;Save all GP registers
    push bp                                    ;Save source square address
    push di                                    ;Save destination square address
    
    mov si, sp                                 ;Clone stack pointer
    mov cx, [si + 36]                          ;Read current depth
    dec cx                                     ;Decrement depth
    call move_sub                              ;Perform move and find next best move
    mov ah, [si + 39]                          ;Read player's max value
    cmp al, ah                                 ;Max value exceeds current value?
    pop di                                     ;Restore destination square address
    pop bp                                     ;Restore source square address
    jl undo                                    ;Yes, proceed to undo move

best:
    mov [si + 39], al                          ;Write max value
    mov [si + 24], di                          ;Write destination square address
    mov [si + 28], bp                          ;Write source square address

undo:
    mov [di], bh                               ;Write original destination square
    mov [bp], bl                               ;Write original source square

    cmp dl, 03h                                ;King?
    jne undo_cont                              ;No, proceed to check destination square

undo_king:
    pusha                                      ;Save king's move parameters
    mov ax, di                                 ;Clone destination square address
    sub ax, bp                                 ;Calculate king's vector
    cmp al, 0FEh                               ;Queenside castling?
    je undo_cast_cont                          ;Yes, proceed to calculate rook's move
    cmp al, 02h                                ;Kingside castling?
    jne undo_king_cont                         ;No, proceed to restore king's move
    dec di                                     ;Decrement source address

undo_cast_cont:
    add di, ax                                 ;Calculate rook's source address
    sar ax, 1                                  ;Normalize king's vector
    add bp, ax                                 ;Calculate rook's destination address
    mov al, [bp]                               ;Read rook's destination square
    and byte ptr [bp], 80h                     ;Clear rook's destination square
    mov [di], al                               ;Write rook's source square

undo_king_cont:
    popa                                       ;Restore king's move parameters

undo_cont:
    test bh, dh                                ;Opponent's piece (or border)?
    popa                                       ;Restore all GP registers
    ret
eval_end:

move_cast_undo:
    popa                                       ;Restore king's move parameters
    mov [di], bh                               ;Write original destination square
    mov [bp], bl                               ;Write original source square
    ret

auto_move_sub:
    mov ah, 07h                                ;Always promote to queen

;Perform move and find best next move
;Input:
;  AH - Promoted piece type
;  DL - Player's color + border
;  DH - Opponent's color + border
;  CX - Search depth
;  BP - Source square address
;  DI - Destination square address
;Output:
;  AL - Player's max value
;  AH - Opponent's max value
;  BL - Original source square contents
;  BH - Original destination square contents
;  DL - Opponent's color + border
;  DH - Player's color + border
;  BP - Opponent's best source square address
;  DI - Opponent's best destination square address
move_sub:
    mov bl, [bp]                               ;Read source square
    mov bh, [di]                               ;Read destination square
    and byte ptr [bp], 80h                     ;Clear source square
    mov al, bl                                 ;Clone source square contents
    and al, 07h                                ;Isolate piece type
    cmp al, 03h                                ;King or pawn?
    je move_king                               ;King, proceed to special move checks
    jg move_dest                               ;Other, proceed to write destination square

move_pawn:
    test bh, 80h                               ;Promotion destination?
    jz move_dest                               ;No, proceed to write destination square

move_promo:
    mov al, ah                                 ;Clone promoted piece type
    or al, al                                  ;Promoted piece type set?
    jnz move_promo_cont                        ;Yes, proceed to calculate promotion gain

move_promo_read:
    mov ah, 01h                                ;Read character
    int 21h                                    ;DOS I/O function
    
move_promo_cont:
    call gain_sub                              ;Calculate promotion gain
    dec ch                                     ;Subtract pawn's value

move_king:
    pusha                                      ;Save king's move parameters
    mov ax, di                                 ;Clone destination square address
    sub ax, bp                                 ;Calculate king's vector
    cmp al, 0FEh                               ;Queenside castling?
    je move_cast_cont                          ;Yes, proceed to calculate rook's move
    cmp al, 02h                                ;Kingside castling?
    jne move_king_cont                         ;No, proceed to restore king's move
    dec di                                     ;Decrement source address

move_cast_cont:
    add di, ax                                 ;Calculate rook's source address
    push ax                                    ;Save king's vector
    mov ax, [di]                               ;Read rook's source square and next square
    and ax, 774Fh                              ;Filter out colors + promo and border + promo
    cmp ax, 0006h                              ;Unmoved rook and vacant square?
    pop ax                                     ;Restore king's vector
    jne move_cast_undo                         ;No, proceed to undo king's move

    sar ax, 1                                  ;Normalize king's vector
    add bp, ax                                 ;Calculate rook's destination address
    mov al, [di]                               ;Read rook's source square
    and byte ptr [di], 80h                     ;Clear rook's source square
    mov [bp], al                               ;Write rook's destination square

move_king_cont:
    popa                                       ;Restore king's move parameters

move_dest:
    or al, dh                                  ;Set color
    mov ah, bh                                 ;Clone destination contents
    and ax, 8037h                              ;Isolate piece type, color and promotion flag
    or al, 40h                                 ;Set moved flag
    or al, ah                                  ;Update promotion flag
    mov [di], al                               ;Write destination square

    mov al, bh                                 ;Clone destination square contents
    call gain_sub                              ;Calculate capture gain
    mov al, ch                                 ;Save total gain

    xchg dl, dh                                ;Swap player's and opponent's colors
    xor ch, ch                                 ;Clear depth's upper byte
    jcxz move_cont                             ;If depth is zero, return
    cmp al, 46                                 ;King captured?
    jge move_cont                              ;If yes, return

next:
    xor ah, ah                                 ;Initialize opponent's max value
    pusha                                      ;Save all GP registers
    mov bp, offset board_db + 30h              ;Start from top left corner
    mov cl, 78h                                ;Ranks * width - margin width

src_loop:
    push cx                                    ;Save source counter
    mov al, [bp]                               ;Read source square
    test al, dl                                ;Opponent's piece or border?
    jnz src_cont                               ;Yes, proceed to next source square
    
    mov bh, al                                 ;Save source square
    and ax, 07h                                ;Isolate piece type
    jz src_cont                                ;No piece, proceed to next source square
    
    mov bl, al                                 ;Save piece type
    mov si, offset moves_wpawn - 1             ;Set base metadata address
    add si, ax                                 ;Calculate absolute metadata address
    lodsb                                      ;Read relative vectors address
    add si, ax                                 ;Calculate absolute vectors address

vec_loop:
    lodsb                                      ;Read vector

    xor cx, cx                                 ;Reset range
    cmp bl, 04h                                ;Check piece type
    jg sign_loop                               ;Slider, proceed to reset destination address
    inc cx                                     ;Crawler, limit range

sign_loop:
    mov di, bp                                 ;Clone source square address

dest_loop:
    cbw                                        ;Extend vector's sign
    add di, ax                                 ;Calculate destination square address
    mov ah, [di]                               ;Read destination square
    test ah, dh                                ;Player's piece or border?
    jnz vec_cont                               ;Yes, proceed to next vector
    
    cmp bl, 03h                                ;Black or white pawn?
    jge eval                                   ;No, proceed to evaluate move

pawn:
    test al, 01h                               ;Any file change?
    jnz pawn_cont                              ;Yes, proceed to check destination square

pawn_vert:
    test bh, 40h                               ;Source piece moved?
    jnz pawn_inv                               ;Yes, proceed to invert color
    or bh, 40h                                 ;Mark as moved
    inc cx                                     ;Increment range

pawn_inv:
    xor ah, 30h                                ;Invert destination's color

pawn_cont:
    test ah, dl                                ;Opponent's piece (or border)?
    jz vec_loop                                ;No, move to next vector

eval_knight:
    mov ah, 04h                                ;Promote to knight
    call eval_sub                              ;Evaluate move
    mov ah, 07h                                ;Promote to queen

eval:
    call eval_sub                              ;Evaluate move
    loopz dest_loop                            ;No, move to next destination

vec_cont:
    cmp bl, 03h                                ;Black or white pawn?
    jl vec_zero                                ;Yes, proceed to zero vector check

    neg al                                     ;Invert vector
    js sign_loop                               ;Negative, proceed to reset destination address

vec_zero:
    or al, al                                  ;Non-zero vector?
    jnz vec_loop                               ;Yes, move to next vector

src_cont:
    inc bp                                     ;Increment source square address
    pop cx                                     ;Restore source counter
    loop src_loop                              ;Move to next source
    
move_done:
    popa                                       ;Restore all GP registers
    sub al, ah                                 ;Calculate player's max value

move_cont:
    ret
move_end:

moves_db:
    moves_wpawn  db vec_wpawn  - moves_wpawn  - 1
    moves_bpawn  db vec_bpawn  - moves_bpawn  - 1
    moves_king   db vec_king   - moves_king   - 1
    moves_knight db vec_knight - moves_knight - 1
    moves_bishop db vec_bishop - moves_bishop - 1
    moves_rook   db vec_rook   - moves_rook   - 1
    moves_queen  db vec_king   - moves_queen  - 1

    vec_wpawn    db 0F0h, 0EFh, 0F1h, 0
    vec_knight   db 01Fh, 021h, 00Eh, 012h, 0
    vec_bpawn    db 010h
    vec_bishop   db 00Fh, 011h, 0
    vec_king     db 00Fh, 011h
    vec_rook     db 010h, 001h

eval_db: ;Fall through 1 byte
    db 0, 1, 1, 46, 3, 3, 5, 9

disp_db:
    db ".PpKNBRQ"
    
init_db: ;Fall through 1 byte
    db 0Ah, 08h, 08h, 0A6h, 0A4h, 0A5h, 0A7h, 0A3h, 0A5h, 0A4h, 0A6h
    db 22h, 00h, 00h, 00h, 00h, 11h
    db 96h, 94h, 95h, 97h, 93h, 95h, 94h, 96h

board_db: ;Fall through 2 bytes

code ends
end start
