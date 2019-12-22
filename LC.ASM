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
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

.model tiny
.186
code segment
    org 100h
    assume cs: code

_PAWN  equ 0001b
_KING  equ 0010b
ROOK   equ 0011b
KNIGHT equ 0100b
BISHOP equ 0101b
QUEEN  equ 0110b

CHANCE equ 1000b
ARCHIE equ 1101b
PIECE  equ 1111b

NULL   equ 00h
MOVED  equ 08h
WHITE  equ 10h
BLACK  equ 20h
BORDER equ 40h
PROMO  equ 80h

FILE_COUNT    equ 10
RANK_COUNT    equ  8
BOARD_WIDTH   equ 12
BOARD_HEIGHT  equ 13
MARGIN_WIDTH  equ  2
MARGIN_HEIGHT equ  2

DEPTH equ 4

start:
    mov cl, FILE_COUNT                         ;Set square counter
    mov si, 82h                                ;Set source address
    mov di, offset capa_db                     ;Set destination address

cli_loop:
    lodsb                                      ;Read piece
    and al, 5Fh                                ;Ignore case
    jz cli_done                                ;No piece, proceed to set display mode
    mov bx, offset disp_db                     ;Set base chars' address

cli_scan_loop:
    inc bx                                     ;Increment char address
    cmp [bx], al                               ;Chars match?
    jne cli_scan_loop                          ;No, proceed to next char

    mov al, bl                                 ;Clone char address
    sub al, offset disp_db - 200h              ;Subtract base address
    stosb                                      ;Write type
    loop cli_loop                              ;Move to next piece

cli_done:
    xor ax, ax                                 ;Display mode 0
    int 10h                                    ;BIOS display function
    mov cl, BOARD_HEIGHT                       ;Set row counter
    mov si, offset init_db                     ;Set row metadata address
    mov di, offset board_db                    ;Set board address

init_loop:
    push cx                                    ;Save row counter
    mov al, BORDER                             ;Border
    stosb                                      ;Write one byte
    stosb                                      ;Write one byte
    mov cl, FILE_COUNT                         ;Set square counter
    lodsb                                      ;Read one byte
    rep stosb                                  ;Write row
    test al, PROMO                             ;First rank?
    jz init_cont                               ;No, proceed to write row

init_row:
    push si                                    ;Save row metadata address
    mov cl, FILE_COUNT                         ;Set square counter
    mov si, offset capa_db                     ;Set row metadata address
    sub di, cx                                 ;Restore square address

init_row_loop:
    lodsb                                      ;Read piece type
    or [di], al                                ;Set piece type
    inc di                                     ;Increment square address
    loop init_row_loop                         ;Move to next square
    pop si                                     ;Restore row metadata address

init_cont:
    pop cx                                     ;Restore row counter
    loop init_loop                             ;Move to next row

main_loop:
    mov cl, RANK_COUNT * (FILE_COUNT + 1) + MARGIN_WIDTH
    mov si, offset board_db + BOARD_WIDTH * MARGIN_HEIGHT - MARGIN_WIDTH
    mov di, offset buffer_db                   ;Set buffer address
    mov dx, di                                 ;Clone buffer address

disp_loop:
    lodsb                                      ;Read square contents
    test al, BORDER                            ;Border?
    jz disp_piece                              ;No, proceed to display piece

disp_border:
    mov al, 0Ah                                ;LF
    inc si                                     ;Skip border square
    jmp disp_cont                              ;Proceed to write square

disp_piece:
    mov ah, al                                 ;Clone square contents
    and ax, 200Fh                              ;Isolate piece type and case
    mov bx, offset disp_db                     ;Set base chars' address
    xlat                                       ;Translate piece type to char
    or al, ah                                  ;Apply case

disp_cont:
    stosb                                      ;Write square
    loop disp_loop                             ;Move to next square

disp_done:
    mov ax, 0924h                              ;String terminator/Write string
    stosb                                      ;Write string terminator
    int 21h                                    ;DOS I/O function

play:
    mov dx, 5060h                              ;Set player's and opponent's colors
    mov cl, DEPTH                              ;Set search depth
    push offset main_loop                      ;Repeat forever
    mov ax, offset move_sub                    ;Perform two moves:
    push ax                                    ;Perform computer's move
    push ax                                    ;Perform human's move
    push offset read_sub                       ;Read destination square
main_end:

;Read square from input
;Output:
;  DI - Square address
read_sub:
    mov bp, di                                 ;Clone address
    mov di, offset board_db + BOARD_WIDTH * (RANK_COUNT + MARGIN_HEIGHT) + MARGIN_WIDTH - 161h
    mov ah, 01h                                ;Read character
    int 21h                                    ;DOS I/O function
    add di, ax                                 ;Add result to base address
    int 21h                                    ;DOS I/O function
    aaa                                        ;Isolate digit (expected 1-8)
    mov ah, BOARD_WIDTH                        ;Set board width
    mul ah                                     ;Multiply by width
    sub di, ax                                 ;Subtract result from base address
    ret
read_end:

eval_sub:
    pusha                                      ;Save all GP registers
    push bp                                    ;Save source square address
    push di                                    ;Save destination square address
    
    mov si, sp                                 ;Clone stack pointer
    mov cx, [si + 36]                          ;Read current depth
    dec cx                                     ;Decrement depth
    call move_sub                              ;Perform move and find best next move
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
    mov [di], bl                               ;Write original destination square
    mov [bp], bh                               ;Write original source square
    test [di], dh                              ;Opponent's piece (or border)?
    popa                                       ;Restore all GP registers
    ret

;Perform move and find best next move
;Input:
;  DL - Player's color + border
;  DH - Opponent's color + border
;  CX - Search depth
;  BP - Source square address
;  DI - Destination square address
;Output:
;  AL - Player's max value
;  AH - Opponent's max value
;  BL - Original destination square contents
;  BH - Original source square contents
;  DL - Opponent's color + border
;  DH - Player's color + border
;  BP - Opponent's best source square address
;  DI - Opponent's best destination square address
move_sub:
    mov al, [di]                               ;Read destination square
    mov ah, [bp]                               ;Read source square
    push ax                                    ;Save destination and source square contents
    sub [bp], ah                               ;Clear source square
    test ah, 04h                               ;Pawn/King/Rook?
    jnz move_cont                              ;No, proceed to write destination square
    or ah, MOVED                               ;Mark as moved

move_cont:
    mov [di], ah                               ;Write destination square
    xchg dl, dh                                ;Swap player's and opponent's colors

    and ax, PIECE                              ;Isolate piece type + clear max value
    mov bx, offset eval_db                     ;Set base values' address
    xlat                                       ;Get player's gain
    pop bx                                     ;Restore destination and source square contents
    jcxz move_ret                              ;If depth is zero, return

next:
    pusha                                      ;Save all GP registers
    mov bp, offset board_db + BOARD_WIDTH * MARGIN_HEIGHT + MARGIN_WIDTH
    mov cl, BOARD_WIDTH * RANK_COUNT - MARGIN_WIDTH

src_loop:
    mov al, [bp]                               ;Read source square
    test al, dl                                ;Opponent's piece or border?
    jnz src_cont                               ;Yes, proceed to next source square

comp_loop:
    mov bh, al                                 ;Save source square contents
    and ax, 07h                                ;Isolate base piece type
    jz src_cont                                ;No piece, proceed to next source square

    push cx                                    ;Save square counter
    mov bl, al                                 ;Save piece type
    mov si, offset moves_db                    ;Set base metadata address
    add si, ax                                 ;Calculate absolute metadata address
    lodsb                                      ;Read relative vectors address
    add si, ax                                 ;Calculate absolute vectors address

vec_loop:
    lodsb                                      ;Read vector

    xor cx, cx                                 ;Initialize range
    test bl, bl                                ;Check piece type
    jp sign_loop                               ;Slider, proceed to clone address
    inc cx                                     ;Crawler, limit range

sign_loop:
    mov di, bp                                 ;Clone source square address

dest_loop:
    cbw                                        ;Extend vector's sign
    add di, ax                                 ;Calculate destination square address
    mov ah, [di]                               ;Read destination square
    test ah, dh                                ;Player's piece or border?
    jnz vec_cont                               ;Yes, proceed to next vector

    cmp bl, _PAWN                              ;Pawn?
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

    test bh, MOVED                             ;Piece moved?
    jnz eval                                   ;Yes, proceed to evaluate move
    or bh, MOVED                               ;Mark as moved
    inc cx                                     ;Increase range

eval:
    call eval_sub                              ;Evaluate move
    loopz dest_loop                            ;No, move to next destination

vec_cont:
    neg al                                     ;Invert vector
    js sign_loop                               ;Negative, proceed to reset destination address
    jnz vec_loop                               ;Yes, move to next vector
    pop cx                                     ;Restore square counter

    mov al, bh                                 ;Restore source square contents
    and al, PIECE                              ;Isolate piece type
    cmp al, ARCHIE                             ;Archbishop?
    je comp_cont                               ;Yes, proceed to knight moves
    cmp al, CHANCE                             ;Chancellor?
    jne src_cont                               ;No, proceed to next source square

comp_cont:
    mov al, KNIGHT                             ;Revert to knight
    jmp comp_loop                              ;Proceed to generate knight's moves

src_cont:
    inc bp                                     ;Increment source square address
    loop src_loop                              ;Move to next source

move_done:
    popa                                       ;Restore all GP registers
    sub al, ah                                 ;Calculate player's max value

move_ret:
    ret
move_end:

moves_db:
    moves_chance db vec_rook   - moves_chance - 1
    moves_pawn   db vec_pawn   - moves_pawn   - 1
    moves_king   db vec_king   - moves_king   - 1
    moves_rook   db vec_rook   - moves_rook   - 1
    moves_knight db vec_knight - moves_knight - 1
    moves_bishop db vec_bishop - moves_bishop - 1
    moves_queen  db vec_king   - moves_queen  - 1

    vec_pawn     db  12
    vec_bishop   db  11,  13,   0
    vec_knight   db  10,  14,  23,  25,   0
    vec_king     db  11,  13
    vec_rook     db  12,   1

eval_db: ;Fall through 1 byte
    db 0, 1, 46, 5, 3, 3, 9, ?
    db 9, 1, 46, 5, ?, 8

disp_db: ;Fall through 1 byte
    db ".PKRNBQ?"
    db "CPKR?A"

capa_db:
    db ROOK, KNIGHT, ARCHIE, BISHOP, QUEEN, _KING, BISHOP, CHANCE, KNIGHT, ROOK

init_db:
    db BORDER, BORDER, BLACK OR PROMO, BLACK OR _PAWN
    db NULL, NULL, NULL, NULL, WHITE OR _PAWN, WHITE OR PROMO

board_db: ;Fall through 3 bytes
    db BOARD_HEIGHT * BOARD_WIDTH dup(?)

buffer_db:

code ends
end start
