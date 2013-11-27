.MODEL large

include game.inc
include screen.inc

.DATA

.CODE

Game_run proc far ; ax => (0x00) {{{1
  ; Tail recursive game loop

  call Screen_update

  ; TODO: use return value of Input
  mov ax, 2
  dec ax

  jnz Game_run
  ret
Game_run endp

; }}}1

END
