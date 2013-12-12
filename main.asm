.MODEL large
.STACK 2048

include game.inc
include screen.inc
include input.inc

DOS_kExit   equ 4ch
DOS_kStdout equ 09h
DOS_kStdin  equ 01h

.DATA

String_Help db "Tron: Light Cycles", 10,
               "==================", 10,
               "Right Player:",      10,
               "    Arrow keys",     10,
               "Left Player:",       10,
               "    Up:    w",       10,
               "    Down:  s",       10,
               "    Left:  a",       10,
               "    Right: d",       10,
                                     36

String_Done         db "Done $"
String_LeftCollide  db "Right Player wins! $"
String_RightCollide db "Left Player wins! $"

.CODE

assume ds:@data, es:@data

main proc near ; {{{1
  ; Data Segment & Extra Segment
  mov ax, @data
  mov ds, ax
  mov es, ax

  call Main_help

  call IO_getc

  call Screen_setup
  call Input_setup

  call Game_setup
  call Game_run ; bx <- collided player id
  mov  bx, ax

  call Input_teardown
  call Screen_teardown

  push bx
  call Main_winner ; (player)

  call Process_exit
main endp

Main_help proc near ; {{{1
  push bp
  mov  bp, sp

  lea  ax, String_Help
  push ax
  call IO_puts

  mov sp, bp
  pop bp
  ret
Main_help endp

Main_winner proc near ; (player) {{{1
  ; player :: ID
  push bp
  mov  bp, sp

  mov  ax, [bp + 4][0] ; player

  cmp ah, Game_kPlayerLeft
  je  @left
  cmp ah, Game_kPlayerRight
  je  @right
  jmp @none

@left:
  lea  ax, String_LeftCollide
  push ax
  call IO_puts
  jmp @return

@right:
  lea  ax, String_RightCollide
  push ax
  call IO_puts
  jmp @return

@none:
  lea  ax, String_Done
  push ax
  call IO_puts

@return:
  pop dx
  mov sp, bp
  pop bp
  ret 2 ; (string)
Main_winner endp

; }}}1

IO_puts proc near ; (string) -> IO () {{{1
  ; string :: Offset [Char]
  push bp
  mov  bp, sp
  push dx

  mov ax, [bp + 4][0] ; dx <- string
  mov dx, ax

  mov ah, DOS_kStdout ; string >>= Stdout (dx)
  int 21h

  pop dx
  mov sp, bp
  pop bp
  ret 2 ; (string)
IO_puts endp

IO_getc proc near ; {{{1
  push bp
  mov  bp, sp

  mov ah, DOS_kStdin
  int 21h

  mov sp, bp
  pop bp
  ret
IO_getc endp

Process_exit proc near ; IO () {{{1
  mov ah, DOS_kExit ; 0x00 >>= Exit ()
  xor al, al
  int 21h
Process_exit endp

; }}}1

END main

