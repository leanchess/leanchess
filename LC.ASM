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
    mov cl, 60h                                ;Ranks * width + margin width + safety
    mov si, offset board_db + 12               ;Grab preceding CR/LF
    mov bp, di                                 ;Clone buffer address
    push cx                                    ;Save output length

disp_loop:
    lodsb                                      ;Read square contents
    test al, 08h                               ;Border?
    jnz disp_cont                              ;Yes, proceed to write square

disp_piece:
    mov dx, ax                                 ;Clone square contents
    and al, 07h                                ;Isolate piece type
    mov bx, offset disp_db                     ;Set base chars' address
    xlat                                       ;Translate piece type to char
    and dx, 0020h                              ;Isolate black/lowercase/first display row
    or al, dl                                  ;Apply case
    
disp_cont:
    stosb                                      ;Write square
    loop disp_loop                             ;Move to next square
    
    pop cx                                     ;Restore output length
    mov ah, 13h                                ;Write string
    mov bx, cx                                 ;Black on brown
    xor dx, dx                                 ;Set position to 0,0
    int 10h                                    ;BIOS output function

    push bp                                    ;Save buffer address
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
    mov di, offset board_db + 101              ;Bottom right corner + margin
    mov dl, 1                                  ;Set file factor
    call read_coord_sub                        ;Read character and add to address
    mov dl, -10                                ;Set rank factor
    
;Read coordinate from input and update square address
;Input:
;  DL - Multiplication factor
;  DI - Base square address
;Output:
;  DI - Updated square address
read_coord_sub:
    xor ah, ah                                 ;Read character
    int 16h                                    ;BIOS input function
    and al, 0Fh                                ;Isolate number (expected 1-8)
    imul dl                                    ;Multiply signed
    add di, ax                                 ;Add result to base address

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
    mov dl, [bp]                               ;Read source square
    mov al, [di]                               ;Read destination square
    mov dh, al                                 ;Clone destination square contents
    sub [bp], dl                               ;Clear source square
    mov [di], dl                               ;Write destination square
    xchg bl, bh                                ;Swap player's and opponent's colors
    
    push bx                                    ;Save player's and opponent's colors
    and al, 07h                                ;Isolate piece type
    mov bx, offset eval_db                     ;Set base values' address
    xlat                                       ;Get player's gain
    pop bx                                     ;Restore player's and opponent's colors
    jcxz sub_ret                               ;If depth is zero, return

next:
    mov ah, -64                                ;Initialize opponent's max value
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
    mov si, offset moves_wpawn - 1             ;Set base metadata address
    add si, ax                                 ;Calculate absolute metadata address
    lodsb                                      ;Read relative vectors address
    add si, ax                                 ;Calculate absolute vectors address

vec_loop:
    lodsb                                      ;Read vector
    or al, al                                  ;Zero vector?
    jz src_cont                                ;Yes, proceed to next source square
    
    mov di, bp                                 ;Clone source square address

dest_loop:
    cbw                                        ;Extend vector's sign
    add di, ax                                 ;Calculate destination square address
    mov ah, [di]                               ;Read destination square
    test ah, bh                                ;Player's piece or border?
    jnz vec_cont                               ;Yes, proceed to next vector
    
    cmp dl, 03h                                ;Black or white pawn?
    jge eval                                   ;No, proceed to evaluate move

pawn:
    test al, 01h                               ;Any file change?
    jnz pawn_cont                              ;Yes, proceed to check destination square

pawn_inv:
    xor ah, 30h                                ;Invert destination's color

pawn_cont:
    test ah, bl                                ;Opponent's piece (or border)?
    jz vec_cont                                ;No, proceed to next vector

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
    
    cmp dl, 04h                                ;Sliding piece?
    jg dest_loop                               ;Yes, move to next destination

vec_cont:
    jmp vec_loop                               ;Move to next vector
    
src_cont:
    dec bp                                     ;Decrement source square address
    loop src_loop                              ;Move to next source
    
    popa                                       ;Restore all GP registers
    sub al, ah                                 ;Calculate player's max value
    ret
play_end:

moves_db:
    moves_wpawn  db vec_wpawn  - moves_wpawn  - 1
    moves_bpawn  db vec_bpawn  - moves_bpawn  - 1
    moves_king   db vec_king   - moves_king   - 1
    moves_knight db vec_knight - moves_knight - 1
    moves_bishop db vec_bishop - moves_bishop - 1
    moves_rook   db vec_rook   - moves_rook   - 1
    moves_queen  db vec_king   - moves_queen  - 1

vec_pawn:
    vec_wpawn    db -11,  -9, -10,   0
    vec_bpawn    db   9,  11,  10,   0
    vec_knight   db  19,  21,   8,  12, -21, -12, -19,  -8,   0
    vec_bishop   db   9,  11, -11,  -9,   0
    vec_king     db   9,  11, -11,  -9
    vec_rook     db  10,   1,  -1, -10

eval_db: ;Fall through 1 byte
    db 0, 1, 1, 46, 3, 3, 5, 9

disp_db:
    db ".PpKNBRQ"
    
init_db:
    db 08h, 08h, 0A6h, 0A4h, 0A5h, 0A7h, 0A3h, 0A5h, 0A4h, 0A6h
    db 22h, 00h, 00h, 00h, 00h, 11h
    db 96h, 94h, 95h, 97h, 93h, 95h, 94h, 96h

board_db: ;Fall through 3 bytes

code ends
end start
