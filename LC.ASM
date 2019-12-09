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
    mov si, offset init_db                     ;Set row metadata address
    mov di, offset board_db                    ;Set board address

init_loop:
    push cx                                    ;Save row counter
    mov ax, 0A0Dh                              ;AL=CR, AH=LF
    stosw                                      ;Write two bytes
    mov cl, 8                                  ;Set square counter
    lodsb                                      ;Read one byte
    test al, 80h                               ;First rank?
    jz init_cont                               ;No, proceed to write row
    dec si                                     ;Back one byte
    rep movsb                                  ;Copy row
    
init_cont:
    rep stosb                                  ;Write row
    pop cx                                     ;Restore row counter
    loop init_loop                             ;Move to next row

main_loop:
    mov cl, 92                                 ;(Ranks + 1) * width + margin width + CRLF
    mov si, offset board_db + 10               ;Grab preceding CR/LF
    mov dx, di                                 ;Clone buffer address

disp_loop:
    lodsb                                      ;Read square contents
    test al, 30h                               ;Piece?
    jz disp_cont                               ;No, proceed to write square

disp_piece:
    inc ax                                     ;Zero-align king
    and al, 27h                                ;Isolate piece type and black/lowercase
    add al, 4Bh                                ;King, (none), (reserved), kNight, bishOp, Pawn, Queen, Rook
    
disp_cont:
    stosb                                      ;Write square
    loop disp_loop                             ;Move to next square
    
    mov ax, 0924h                              ;String terminator/Write string
    stosb                                      ;Write string terminator
    int 21h                                    ;DOS I/O function

    push dx                                    ;Save buffer address
    call read_move_sub                         ;Read move
    call play_sub                              ;Perform two moves
    pop di                                     ;Restore buffer address
    jmp main_loop                              ;Repeat forever
main_end:

;Read move from input
;Output:
;  BP - Source square address
;  DI - Destination square address
read_move_sub:
    call read_square_sub                       ;Read source square
    mov bp, di                                 ;Clone address

;Read square from input
;Output:
;  DI - Square address
read_square_sub:
    mov di, offset board_db + 101 - 15Fh       ;Bottom right corner + margin - ASCII offset
    mov ah, 01h                                ;Read character
    int 21h                                    ;DOS I/O function
    add di, ax                                 ;Add result to base address
    int 21h                                    ;DOS I/O function
    and al, 0Fh                                ;Isolate number (expected 1-8)
    xchg ah, al                                ;Flip BCD digits
    aad                                        ;Multiply AH by 10 and add AL
    sub di, ax                                 ;Subtract result from base address

sub_ret:
    ret
read_end:

;Perform two moves
;Input:
;  BP - Source square address
;  DI - Destination square address
play_sub:
    mov bx, 1828h                              ;Set player's and opponent's colors
    mov cl, 3                                  ;Set search depth
    call move_sub                              ;Perform move and find best next move

;Perform move and find best next move
;Input:
;  BL - Player's color + border
;  BH - Opponent's color + border
;  CX - Search depth
;  BP - Source square address
;  DI - Destination square address
;Output:
;  AL - Player's max value
;  AH - Opponent's max value
;  BL - Opponent's color + border
;  BH - Player's color + border
;  DL - Original source square contents
;  DH - Original destination square contents
;  BP - Opponent's best source square address
;  DI - Opponent's best destination square address
move_sub:
    mov ax, 0C000h                             ;Clear contents + opponent's max value
    xchg al, [bp]                              ;Read and write source square
    mov dl, al                                 ;Clone source square contents
    xchg al, [di]                              ;Read and write destination square
    mov dh, al                                 ;Clone destination square contents
    xchg bl, bh                                ;Swap player's and opponent's colors
    
    push bx                                    ;Save player's and opponent's colors
    and al, 07h                                ;Isolate piece type
    mov bx, offset eval_db                     ;Set base values' address
    xlat                                       ;Get player's gain
    pop bx                                     ;Restore player's and opponent's colors
    jcxz sub_ret                               ;If depth is zero, return

next:
    pusha                                      ;Save all GP registers
    mov bp, offset board_db + 99               ;Start from bottom right corner
    mov cl, 78                                 ;Ranks * width - margin width

src_loop:
    mov al, [bp]                               ;Read source square
    test al, bl                                ;Opponent's piece or border?
    jnz src_cont                               ;Yes, proceed to next source square
    
    and ax, 07h                                ;Isolate piece type
    jz src_cont                                ;No piece, proceed to next source square
    
    mov dl, al                                 ;Save piece type
    mov si, offset moves_knight - 2            ;Set base metadata address
    add si, ax                                 ;Calculate absolute metadata address
    lodsb                                      ;Read relative vectors address
    add si, ax                                 ;Calculate absolute vectors address

vec_loop:
    lodsb                                      ;Read vector

sign_loop:
    mov di, bp                                 ;Clone source square address

dest_loop:
    cbw                                        ;Extend vector's sign
    add di, ax                                 ;Calculate destination square address
    mov ah, [di]                               ;Read destination square
    test ah, bh                                ;Player's piece or border?
    jnz vec_cont                               ;Yes, proceed to next vector
    
    cmp dl, 04h                                ;Black or white pawn?
    jne eval                                   ;No, proceed to evaluate move

pawn:
    test al, 01h                               ;Any file change?
    jnz pawn_cont                              ;Yes, proceed to check destination square

pawn_inv:
    xor ah, 30h                                ;Invert destination's color

pawn_cont:
    test ah, bl                                ;Opponent's piece (or border)?
    jz vec_cont                                ;No, proceed to next vector

    push ax                                    ;Save vector
    and al, 80h                                ;Isolate sign bit
    shr al, 2                                  ;Align with black bit
    xor al, bh                                 ;Flip color and border bits
    pop ax                                     ;Restore vector
    jnp vec_cont                               ;Odd, proceed to next vector

eval:
    pusha                                      ;Save all GP registers
    push bp                                    ;Save source square address
    push di                                    ;Save destination square address
    
    mov si, sp                                 ;Clone stack pointer
    mov cx, [si + 32]                          ;Read current depth
    dec cx                                     ;Decrement depth
    call move_sub                              ;Recursively call self
    mov ah, [si + 35]                          ;Read player's max value
    cmp al, ah                                 ;Current value exceeds max value?
    pop di                                     ;Restore destination square address
    pop bp                                     ;Restore source square address
    jle undo                                   ;No, proceed to undo move

best:
    mov [si + 35], al                          ;Write max value
    mov [si + 20], di                          ;Write destination square address
    mov [si + 24], bp                          ;Write source square address

undo:
    mov [di], dh                               ;Write original destination square
    mov [bp], dl                               ;Write original source square
    test dh, bh                                ;Opponent's piece (or border)?
    
    popa                                       ;Restore all GP registers
    jnz vec_cont                               ;Yes, proceed to next vector
    
    test dl, dl                                ;Check piece type
    jp dest_loop                               ;Slider, move to next destination

vec_cont:
    neg al                                     ;Invert vector
    js sign_loop                               ;Negative, proceed to reset destination address
    jnz vec_loop                               ;Non-zero, move to next vector

src_cont:
    dec bp                                     ;Decrement source square address
    loop src_loop                              ;Move to next source
    
    popa                                       ;Restore all GP registers
    sub al, ah                                 ;Calculate player's max value
    ret
play_end:

moves_db:
    moves_knight db vec_knight - moves_knight - 1
    moves_bishop db vec_bishop - moves_bishop - 1
    moves_pawn   db vec_pawn   - moves_pawn   - 1
    moves_queen  db vec_king   - moves_queen  - 1
    moves_rook   db vec_rook   - moves_rook   - 1
    moves_king   db vec_king   - moves_king   - 1

    vec_knight   db  19,  21,   8,  12,   0
    vec_pawn     db  10
    vec_bishop   db   9,  11,   0
    vec_king     db   9,  11
    vec_rook     db  10,   1

eval_db: ;Fall through 1 byte
    db 0, 0, 3, 3, 1, 9, 5, 46

init_db:
    db 08h, 08h, 0A6h, 0A2h, 0A3h, 0A5h, 0A7h, 0A3h, 0A2h, 0A6h
    db 24h, 00h, 00h, 00h, 00h, 14h
    db 96h, 92h, 93h, 95h, 97h, 93h, 92h, 96h

board_db: ;Fall through 3 bytes

code ends
end start
