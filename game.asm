.MODEL large

include game.inc
include screen.inc

.FARDATA

_playerTail dw 1024 dup(0)

.CODE

assume ds:@fardata, es:@fardata

Game_run proc far ; {{{1
  push bp
  mov  bp, sp
  push ds
  push es

  mov ax, @fardata
  mov ds, ax
  mov es, ax

@@:
  mov  ax, 321
  push ax
  call Screen_setPixel ; (321)

  call Screen_update

  ; TODO: use return value of Input
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
