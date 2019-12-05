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

main_loop:
    mov cl, 74                                 ;Ranks * (files + 1) + LF * 2
    mov si, offset board_db + 20h              ;Grab preceding LF * 2
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
    
    mov ax, 0924h                              ;String terminator/write string
    stosb                                      ;Write string terminator
    int 21h                                    ;BIOS output function

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
    mov di, offset board_db + 0AEh - 15Fh      ;Bottom right corner + margin - ASCII offset
    mov ah, 01h                                ;Read character
    int 21h                                    ;DOS I/O function
    add di, ax                                 ;Add result to base address
    int 21h                                    ;DOS I/O function
    and ax, 0Fh                                ;Isolate number (expected 1-8)
    shl ax, 4                                  ;Multiply by 16
    sub di, ax                                 ;Subtract result from base address
    ret

;Calculate gain
;Input:
;  AL - Piece
;  CH - Total gain
;Output:
;  AL - Piece type
;  CH - Updated total gain
gain_sub:
    and al, 07h                                ;Isolate piece type
    pusha                                      ;Save piece type and colors
    mov bx, offset eval_db                     ;Set base values' address
    xlat                                       ;Get piece's value
    add ch, al                                 ;Add to total gain
    popa                                       ;Restore piece type and colors
    ret
read_end:

;Evaluate move
;Input:
;  AH - Promoted piece type
;  BL - Player's color + border
;  BH - Opponent's color + border
;  BP - Source square address
;  CX - Destination counter
;  DL - Source piece type
;  DH - Source square contents + moved
;  DI - Destination square address
;Output:
;  AH - Promoted piece type
;  BL - Player's color + border
;  BH - Opponent's color + border
;  CX - Destination counter
;  DL - Source piece type
;  DH - Source square contents + moved
;  BP - Source square address
;  DI - Destination square address
;  ZF = 0 if OK to move to next destination
eval_sub:
    pusha                                      ;Save all GP registers
    push bp                                    ;Save source square address
    push di                                    ;Save destination square address
    
    mov si, sp                                 ;Clone stack pointer
    mov cx, [si + 38]                          ;Read current depth
    dec cx                                     ;Decrement depth
    call auto_move_sub                         ;Perform move and find next best move
    mov ah, [si + 41]                          ;Read player's max value
    cmp al, ah                                 ;Current value exceeds max value?
    pop di                                     ;Restore destination square address
    pop bp                                     ;Restore source square address
    jl undo                                    ;No, proceed to undo move

best:
    mov [si + 41], al                          ;Write max value
    mov [si + 26], di                          ;Write destination square address
    mov [si + 30], bp                          ;Write source square address

undo:
    mov [di], dh                               ;Write original destination square
    mov [bp], dl                               ;Write original source square

undo_cont:
    test dh, bh                                ;Opponent's piece (or border)?
    popa                                       ;Restore all GP registers

sub_ret:
    ret
eval_end:

;Perform two moves
;Input:
;  BP - Source square address
;  DI - Destination square address
play_sub:
    mov bx, 1828h                              ;Set player's and opponent's colors
    mov cl, 3                                  ;Set search depth
    call move_sub                              ;Perform move and find best next move

auto_move_sub:
    mov ah, 06h                                ;Assume promotion to queen

;Perform move and find best next move
;Input:
;  AH - Promoted piece type
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
    mov dh, [di]                               ;Read destination square
    and byte ptr [bp], 80h                     ;Clear source square
    mov al, dl                                 ;Clone source square contents
    and al, 07h                                ;Isolate piece type
    cmp al, 03h                                ;Black or white pawn?
    jge move_cont                              ;No, proceed to kingdom check

move_pawn:
    test dh, 80h                               ;Promotion destination?
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

move_cont:
    cmp al, 07h                                ;King?
    jne move_dest                              ;No, proceed to write destination square

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
    sar ax, 1                                  ;Normalize king's vector
    add bp, ax                                 ;Calculate rook's destination address
    mov al, [di]                               ;Read rook's source square
    and byte ptr [di], 80h                     ;Clear rook's source square
    mov [bp], al                               ;Write rook's destination square

move_king_cont:
    popa                                       ;Restore king's move parameters
    
move_dest:
    or al, bh                                  ;Set color
    mov ah, dh                                 ;Clone destination contents
    and ax, 8037h                              ;Isolate piece type, color and promotion flag
    or al, 40h                                 ;Set moved flag
    or al, ah                                  ;Update promotion flag
    mov [di], al                               ;Write destination square

    mov al, dh                                 ;Clone destination square contents
    call gain_sub                              ;Calculate capture gain
    mov al, ch                                 ;Save total gain

    xchg bl, bh                                ;Swap player's and opponent's colors
    xor ch, ch                                 ;Clear depth's upper byte
    jcxz sub_ret                               ;If depth is zero, return
    cmp al, 46                                 ;King captured?
    jge sub_ret                                ;If yes, return

next:
    mov ah, -64                                ;Initialize opponent's max value
    pusha                                      ;Save all GP registers
    mov bp, offset board_db + 30               ;Start from top left corner
    mov cl, 78                                 ;Ranks * width - margin width

src_loop:
    push cx                                    ;Save source counter
    mov al, [bp]                               ;Read source square
    test al, bl                                ;Opponent's piece or border?
    jnz src_cont                               ;Yes, proceed to next source square
    
    mov dh, al                                 ;Save source square
    and ax, 07h                                ;Isolate piece type
    jz src_cont                                ;No piece, proceed to next source square
    
    mov dl, al                                 ;Save piece type
    shl ax, 1                                  ;Two bytes per piece type
    mov si, offset moves_wpawn - 2             ;Set base metadata address
    add si, ax                                 ;Calculate absolute metadata address
    lodsb                                      ;Read range
    mov cl, al                                 ;Clone range
    lodsb                                      ;Read relative vectors address
    add si, ax                                 ;Calculate absolute vectors address

vec_loop:
    lodsb                                      ;Read vector
    or al, al                                  ;Zero vector?
    jz src_cont                                ;Yes, proceed to next source square
    
    push cx                                    ;Save range
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

pawn_vert:
    test dh, 40h                               ;Source piece moved?
    jnz pawn_inv                               ;Yes, proceed to invert color
    or dh, 40h                                 ;Mark as moved
    inc cx                                     ;Increment range

pawn_inv:
    xor ah, 30h                                ;Invert destination's color

pawn_cont:
    test ah, bl                                ;Opponent's piece (or border)?
    jz vec_cont                                ;No, proceed to next vector

eval:
    call eval_sub                              ;Evaluate move
    loopz dest_loop                            ;No, move to next destination

vec_cont:
    pop cx                                     ;Restore range
    jmp vec_loop                               ;Move to next vector
    
src_cont:
    inc bp                                     ;Increment source square address
    pop cx                                     ;Restore source counter
    loop src_loop                              ;Move to next source
    
    popa                                       ;Restore all GP registers
    sub al, ah                                 ;Calculate player's max value
    ret
play_end:

init_db:
    db 08h, 08h, 08h, 0A5h, 0A3h, 0A4h, 0A6h, 0A7h, 0A4h, 0A3h, 0A5h
    db 22h, 00h, 00h, 00h, 00h, 11h
    db 95h, 93h, 94h, 96h, 97h, 94h, 93h, 95h, 08h, 08h

moves_db:
    moves_wpawn  db 1, vec_wpawn  - moves_wpawn  - 2
    moves_bpawn  db 1, vec_bpawn  - moves_bpawn  - 2
    moves_knight db 1, vec_knight - moves_knight - 2
    moves_bishop db 7, vec_bishop - moves_bishop - 2
    moves_rook   db 7, vec_rook   - moves_rook   - 2
    moves_queen  db 7, vec_king   - moves_queen  - 2
    moves_king   db 1, vec_king   - moves_king   - 2

vec_pawn:
    vec_wpawn    db 0EFh, 0F1h, 0F0h, 0
    vec_bpawn    db 00Fh, 011h, 010h, 0
    vec_knight   db 01Fh, 021h, 00Eh, 012h, 0DFh, 0E1h, 0EEh, 0F2h, 0
    vec_bishop   db 00Fh, 011h, 0EFh, 0F1h, 0
    vec_king     db 00Fh, 011h, 0EFh, 0F1h
    vec_rook     db 010h, 001h, 0FFh, 0F0h

eval_db: ;Fall through 1 byte
    db 0, 1, 1, 3, 3, 5, 9

disp_db: ;Fall through 1 byte
    db 46, "PpNBRQK", 0Ah
    
board_db:

code ends
end start
