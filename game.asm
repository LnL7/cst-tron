.MODEL large

include game.inc
include screen.inc
include input.inc

TAG_kColor     equ 00h
TAG_kDirection equ 02h
TAG_kPosition  equ 04h
TAG_kIndex     equ 06h
TAG_kTail      equ 08h

.FARDATA

Player_left       dw 1024 dup(0)
Player_directions dw 5 dup(0) ; Jump table with label offsets

.CODE

assume ds:@fardata, es:@fardata

Game_run proc far ; IO () {{{1
  ; position :: Int
  push bp
  mov  bp, sp
  push bx
  push ds

  mov ax, @fardata
  mov ds, ax

@@:
  call Player_input

  mov  ax, offset Player_left
  push ax
  call Player_update ; (player)

  mov  ax, offset Player_left
  push ax
  call Player_render ; (player)

  call Screen_update

  mov ax, 2
  dec ax
  jnz @B

  pop ds
  pop bx
  mov sp, bp
  pop bp
  retf
Game_run endp

Game_setup proc far ; IO () {{{1
  push bp
  mov  bp, sp

  call Player_alloc

  mov sp, bp
  pop bp
  retf
Game_setup endp

Player_alloc proc near ; IO () {{{1
  push bp
  mov  bp, sp
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov al, Screen_kWhite
  mov ah, Screen_kWhite
  mov [Player_left][TAG_kColor], ax ; color = (White:White)

  mov ax, Input_kNone
  mov [Player_left][TAG_kDirection], ax ; direction = None

  mov ax, offset [Player_left][TAG_kTail]
  mov [Player_left][TAG_kIndex], ax ; index = Tail[0]

  ; Initialize starting position
  mov ax, Screen_kWidth
  inc ax
  mov [Player_left][TAG_kPosition], ax

  ; Initialize Jump Table
  mov ax, offset Player_moveNone
  mov [Player_directions][Input_kNone], ax
  mov ax, offset Player_moveUp
  mov [Player_directions][Input_kUp], ax
  mov ax, offset Player_moveDown
  mov [Player_directions][Input_kDown], ax
  mov ax, offset Player_moveLeft
  mov [Player_directions][Input_kLeft], ax
  mov ax, offset Player_moveRight
  mov [Player_directions][Input_kRight], ax

  pop ds
  mov sp, bp
  pop bp
  ret
Player_alloc endp

Player_input proc near ; {{{1
  push bp
  mov  bp, sp
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  push ds ; Segment Player (direction)
  mov  ax, offset [Player_left][TAG_kDirection]
  push ax
  call Input_arrowKeys ; (segment, direction)

  pop ds
  mov sp, bp
  pop bp
  ret
Player_input endp

Player_render proc near ; (player) {{{1
  ; player :: Offset Player
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov  ax, [Player_left][TAG_kPosition]
  push ax
  call Screen_setPixel ; (position)

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_render endp

Player_update proc near ; (player) {{{1
  ; player :: Offset Player
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  mov ax, [bx][TAG_kDirection]
  mov bx, ax

  mov  ax, [bp + 4][0] ; player
  push ax
  call [Player_directions][bx] ; call Player_move{Direction} (player)

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_update endp

Player_moveNone proc near ; (player) -> IO () {{{1
  push bp
  mov  bp, sp
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveNone endp

Player_moveUp proc near ; (player) -> IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  sub word ptr [bx][TAG_kPosition], Screen_kWidth

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveUp endp

Player_moveDown proc near ; (player) -> IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  add word ptr [bx][TAG_kPosition], Screen_kWidth

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveDown endp

Player_moveLeft proc near ; (player) -> IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  dec word ptr [bx][TAG_kPosition]

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveLeft endp

Player_moveRight proc near ; (player) -> IO () {{{1
  ; player :: Offset Player
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  inc word ptr [bx][TAG_kPosition]

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveRight endp

; }}}1

END
