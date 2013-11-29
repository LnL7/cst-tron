.MODEL large

include game.inc
include screen.inc

.FARDATA

Player_data dw 1024 dup(0)

.CODE

assume ds:@fardata, es:@fardata

Game_run proc far ; IO () {{{1
  ; position :: Int
  push bp
  mov  bp, sp
  push ds
  push es

  mov ax, @fardata
  mov ds, ax
  mov es, ax

@@:
  mov  ax, 321 ; position
  push ax
  call Screen_setPixel ; (position)

  call Screen_update

  mov ax, 2
  dec ax
  jnz @B

  pop es
  pop ds
  mov sp, bp
  pop bp
  retf
Game_run endp

; }}}1

END
