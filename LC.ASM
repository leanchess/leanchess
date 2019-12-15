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
    mov ax, 0808h                              ;Border
    stosw                                      ;Write two bytes
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

init_done:
    mov bp, di                                 ;Save buffer address

main_loop:
    mov cl, 60h                                ;Ranks * width + margin width + safety
    mov si, offset board_db + 26               ;Grab two preceding tabs
    mov di, bp                                 ;Restore buffer address
    push cx                                    ;Save output length

disp_loop:
    lodsb                                      ;Read square contents
    test al, 38h                               ;Piece or border?
    jz disp_cont                               ;No, proceed to write square

disp_piece:
    inc ax                                     ;Zero-align king
    cmp al, 09h                                ;Border?
    jz disp_cont                               ;Yes, proceed to write square
    and al, 27h                                ;Isolate piece type and black/lowercase
    add al, 4Bh                                ;King, (none), (reserved), kNight, bishOp, Pawn, Queen, Rook
    
disp_cont:
    stosb                                      ;Write square
    loop disp_loop                             ;Move to next square
    
disp_done:
    pop cx                                     ;Restore output length
    mov ah, 13h                                ;Write string
    mov bx, cx                                 ;Black on brown
    xor dx, dx                                 ;Set position to 0,0
    int 10h                                    ;BIOS output function

play:
    mov dx, 1828h                              ;Set player's and opponent's colors
    mov cl, 3                                  ;Set search depth
    push offset main_loop                      ;Repeat forever
    mov ax, offset move_sub                    ;Perform two moves:
    push ax                                    ;Perform computer's move
    push ax                                    ;Perform human's move
    push offset read_square_sub                ;Read destination square
main_end:

;Read square from input
;Output:
;  DI - Square address
read_square_sub:
    mov si, di                                 ;Clone address
    mov di, offset board_db + 123              ;Bottom right corner + margin
    mov bl, 1                                  ;Set file factor
    call read_coord_sub                        ;Read character and add to address
    mov bl, -12                                ;Set rank factor
    
;Read coordinate from input and update square address
;Input:
;  BL - Multiplication factor
;  DI - Base square address
;Output:
;  DI - Updated square address
read_coord_sub:
    xor ah, ah                                 ;Read character
    int 16h                                    ;BIOS input function
    and al, 0Fh                                ;Isolate number (expected 1-8)
    imul bl                                    ;Multiply signed
    add di, ax                                 ;Add result to base address

sub_ret:
    ret
read_end:

;Perform move and find best next move
;Input:
;  DL - Player's color + border
;  DH - Opponent's color + border
;  CX - Search depth
;  SI - Source square address
;  DI - Destination square address
;Output:
;  AL - Player's max value
;  AH - Opponent's max value
;  DL - Opponent's color + border
;  DH - Player's color + border
;  SI - Opponent's best source square address
;  DI - Opponent's best destination square address
move_sub:
    xor ax, ax                                 ;Clear contents + opponent's max value
    xchg al, [si]                              ;Read and write source square
    xchg al, [di]                              ;Read and write destination square
    xchg dl, dh                                ;Swap player's and opponent's colors
    
    and al, 07h                                ;Isolate piece type
    mov bx, offset eval_db                     ;Set base values' address
    xlat                                       ;Get player's gain
    jcxz sub_ret                               ;If depth is zero, return

next:
    pusha                                      ;Save all GP registers
    mov bp, offset board_db + 28               ;Start from top left corner
    mov cl, 78                                 ;Ranks * width - margin width

src_loop:
    mov al, [bp]                               ;Read source square
    test al, dl                                ;Opponent's piece or border?
    jnz src_cont                               ;Yes, proceed to next source square
    
    and ax, 07h                                ;Isolate piece type
    jz src_cont                                ;No piece, proceed to next source square
    
    mov bl, al                                 ;Save piece type
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
    mov bh, ah                                 ;Clone destination square contents
    test ah, dh                                ;Player's piece or border?
    jnz vec_cont                               ;Yes, proceed to next vector
    
    cmp bl, 04h                                ;Black or white pawn?
    jne eval                                   ;No, proceed to evaluate move

pawn:
    test al, 01h                               ;Any file change?
    jnz pawn_cont                              ;Yes, proceed to check destination square

pawn_inv:
    xor ah, 30h                                ;Invert destination's color

pawn_cont:
    test ah, dl                                ;Opponent's piece (or border)?
    jz vec_cont                                ;No, proceed to next vector

    push ax                                    ;Save vector
    and al, 80h                                ;Isolate sign bit
    shr al, 2                                  ;Align with black bit
    xor al, dh                                 ;Flip color and border bits
    pop ax                                     ;Restore vector
    jnp vec_cont                               ;Odd, proceed to next vector

eval:
    pusha                                      ;Save all GP registers
    push bp                                    ;Save source square address
    push di                                    ;Save destination square address
    
    mov si, bp                                 ;Clone source square address
    mov bp, sp                                 ;Clone stack pointer
    mov cx, [bp + 32]                          ;Read current depth
    dec cx                                     ;Decrement depth
    call move_sub                              ;Recursively call self
    mov ah, [bp + 35]                          ;Read player's max value
    cmp al, ah                                 ;Max value exceeds current value?
    pop di                                     ;Restore destination square address
    pop si                                     ;Restore source square address
    jl undo                                    ;Yes, proceed to undo move

best:
    mov [bp + 35], al                          ;Write max value
    mov [bp + 20], di                          ;Write destination square address
    mov [bp + 22], si                          ;Write source square address

undo:    
    popa                                       ;Restore all GP registers
    xchg bh, [di]                              ;Read and write original destination square
    mov [bp], bh                               ;Write original source square
    test [di], dl                              ;Opponent's piece (or border)?
    jnz vec_cont                               ;Yes, proceed to next vector
    
    test bl, bl                                ;Check piece type
    jp dest_loop                               ;Slider, move to next destination

vec_cont:
    neg al                                     ;Invert vector
    js sign_loop                               ;Negative, proceed to reset destination address
    jnz vec_loop                               ;Non-zero, move to next vector

src_cont:
    inc bp                                     ;Increment source square address
    loop src_loop                              ;Move to next source
    
move_done:
    popa                                       ;Restore all GP registers
    sub al, ah                                 ;Calculate player's max value
    ret
move_end:

moves_db:
    moves_knight db vec_knight - moves_knight - 1
    moves_bishop db vec_bishop - moves_bishop - 1
    moves_pawn   db vec_pawn   - moves_pawn   - 1
    moves_queen  db vec_king   - moves_queen  - 1
    moves_rook   db vec_rook   - moves_rook   - 1
    moves_king   db vec_king   - moves_king   - 1

    vec_knight   db  10,  14,  23,  25,   0
    vec_pawn     db  12
    vec_bishop   db  11,  13,   0
    vec_king     db  11,  13
    vec_rook     db  12,   1

eval_db: ;Fall through 1 byte
    db 0, 0, 3, 3, 1, 9, 5, 46

init_db:
    db 08h, 08h, 0A6h, 0A2h, 0A3h, 0A5h, 0A7h, 0A3h, 0A2h, 0A6h
    db 24h, 00h, 00h, 00h, 00h, 14h
    db 96h, 92h, 93h, 95h, 97h, 93h, 92h, 96h

board_db: ;Fall through 3 bytes
    db 156 dup(?)

buffer_db:

code ends
end start
