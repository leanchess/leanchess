; LeanChess (c) 2019 Dmitry Shechtman
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
    mov ax, 0A0Dh                              ;AL=CR, AH=LF
    stosw                                      ;Write two bytes
    pop cx                                     ;Restore row counter
    loop init_loop                             ;Move to next row
main_loop:
    mov cl, 92                                 ;(Ranks + 1) * width + margin width + CRLF
    mov si, offset board_db + 18               ;Grab preceding CR/LF
    mov dx, di                                 ;Clone buffer address
disp_loop:
    lodsb                                      ;Read square contents
    test al, 30h                               ;Piece?
    jz disp_cont                               ;No, proceed to write square
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
read_move_sub:
    call read_square_sub                       ;Read source square
    mov bp, di                                 ;Clone address
read_square_sub:
    mov di, offset board_db + 109 - 15Fh       ;Bottom right corner + margin - ASCII offset
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
play_sub:
    mov bx, 1828h                              ;Set player's and opponent's colors
    mov cl, 3                                  ;Set search depth
    call move_sub                              ;Perform move and find best next move
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
    mov ah, -64                                ;Initialize opponent's max value
    pusha                                      ;Save all GP registers
    mov bp, offset board_db + 107              ;Start from bottom right corner
    mov cl, 78                                 ;Ranks * width - margin width
src_loop:
    push cx                                    ;Save source counter
    mov al, [bp]                               ;Read source square
    test al, bl                                ;Opponent's piece or border?
    jnz src_cont                               ;Yes, proceed to next source square
    and al, 07h                                ;Isolate piece type
    jz src_cont                                ;No piece, proceed to next source square
    mov dl, al                                 ;Save piece type
    mov ah, 3                                  ;Three bytes per piece type
    mul ah                                     ;Calculate metadata offset
    mov si, offset moves_knight - 6            ;Set base metadata address
    add si, ax                                 ;Calculate absolute metadata address
    lodsb                                      ;Read vector count
    mov cl, al                                 ;Clone vector count
    lodsb                                      ;Read range
    mov dh, al                                 ;Save range
    lodsb                                      ;Read relative vectors address
    add si, ax                                 ;Calculate absolute vectors address
vec_loop:
    push cx                                    ;Save vector counter
    lodsb                                      ;Read vector
    mov di, bp                                 ;Clone source square address
    mov cl, dh                                 ;Restore range
dest_loop:
    cbw                                        ;Extend vector's sign
    add di, ax                                 ;Calculate destination square address
    mov ah, [di]                               ;Read destination square
    test ah, bh                                ;Player's piece or border?
    jnz vec_cont                               ;Yes, proceed to next vector
    cmp dl, 04h                                ;Black or white pawn?
    jne eval                                   ;No, proceed to evaluate move
    test al, 01h                               ;Any file change?
    jnz pawn_cont                              ;Yes, proceed to check destination square
    xor ah, 30h                                ;Invert destination's color
pawn_cont:
    test ah, bl                                ;Opponent's piece (or border)?
    jz vec_cont                                ;No, proceed to next vector
    and al, 80h                                ;Isolate sign bit
    shr al, 2                                  ;Align with black bit
    xor al, bh                                 ;Flip color and border bits
    jnp vec_cont                               ;Odd, proceed to next vector
eval:
    pusha                                      ;Save all GP registers
    push bp                                    ;Save source square address
    push di                                    ;Save destination square address
    mov si, sp                                 ;Clone stack pointer
    mov cx, [si + 36]                          ;Read current depth
    dec cx                                     ;Decrement depth
    call move_sub                              ;Recursively call self
    mov ah, [si + 39]                          ;Read player's max value
    cmp al, ah                                 ;Current value exceeds max value?
    pop di                                     ;Restore destination square address
    pop bp                                     ;Restore source square address
    jle undo                                   ;No, proceed to undo move
    mov [si + 39], al                          ;Write max value
    mov [si + 24], di                          ;Write destination square address
    mov [si + 28], bp                          ;Write source square address
undo:
    mov [di], dh                               ;Write original destination square
    mov [bp], dl                               ;Write original source square
    test dh, bh                                ;Opponent's piece (or border)?
    popa                                       ;Restore all GP registers
    loopz dest_loop                            ;No, move to next destination
vec_cont:
    pop cx                                     ;Restore vector counter
    loop vec_loop                              ;Move to next vector
src_cont:
    dec bp                                     ;Decrement source square address
    pop cx                                     ;Restore source counter
    loop src_loop                              ;Move to next source
    popa                                       ;Restore all GP registers
    sub al, ah                                 ;Calculate player's max value
    ret
init_db:
    db 08h, 08h, 08h, 0A6h, 0A2h, 0A3h, 0A5h, 0A7h, 0A3h, 0A2h, 0A6h
    db 24h, 00h, 00h, 00h, 00h, 14h
    db 96h, 92h, 93h, 95h, 97h, 93h, 92h, 96h, 08h
moves_db: ;Fall through 1 byte
    moves_knight db 08h, 1, vec_knight - moves_knight - 3
    moves_bishop db   4, 7, vec_bishop - moves_bishop - 3
    moves_pawn   db   6, 1, vec_pawn   - moves_pawn   - 3
    moves_queen  db   8, 7, vec_bishop - moves_queen  - 3
    moves_rook   db   4, 7, vec_rook   - moves_rook   - 3
    moves_king   db   8, 1, vec_bishop - moves_king   - 3
vec_pawn:
    vec_wpawn    db -11,  -9, -10
    vec_bpawn    db   9,  11,  10
    vec_knight   db  19,  21,   8,  12, -21, -12, -19,  -8
    vec_bishop   db   9,  11, -11,  -9
    vec_rook     db  10,   1,  -1, -10
eval_db:
    db 0, 0, 3, 5, 1, 9, 5, 46
board_db:
code ends
end start
