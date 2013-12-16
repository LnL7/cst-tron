.MODEL large
.STACK 2048

include game.inc
include screen.inc
include input.inc

DOS_kExit   equ 4ch
DOS_kStdout equ 09h
DOS_kStdin  equ 01h

CHAR_kEndl equ 10
CHAR_kEos  equ 36

.DATA

String_HelpControls db  CHAR_kEndl,
  "Tron: Light Cycles", CHAR_kEndl,
  "==================", CHAR_kEndl,
                        CHAR_kEndl,
  "Right Player:",      CHAR_kEndl,
  "    Arrow keys",     CHAR_kEndl,
                        CHAR_kEndl,
  "Left Player:",       CHAR_kEndl,
  "    Up:    w",       CHAR_kEndl,
  "    Down:  s",       CHAR_kEndl,
  "    Left:  a",       CHAR_kEndl,
  "    Right: d",       CHAR_kEndl,
                        CHAR_kEos

String_HelpLevel db        CHAR_kEndl,
  "Select Level:",         CHAR_kEndl,
  "  1) horizontal ",      CHAR_kEndl,
  "  2) vertical ",        CHAR_kEndl,
  "  3) cross ",           CHAR_kEndl,
  "     otherwise empty ", CHAR_kEndl,
                           CHAR_kEos

String_Done         db "Done",               CHAR_kEos
String_LeftCollide  db "Right Player wins!", CHAR_kEos
String_RightCollide db "Left Player wins!",  CHAR_kEos

.CODE

assume ds:@data, es:@data

main proc near ; {{{1
  ; Data Segment & Extra Segment
  mov ax, @data
  mov ds, ax
  mov es, ax

  call Main_help

  call IO_getc
  mov  bx, ax

  call Screen_setup
  call Input_setup

  push bx
  call Game_setup ; (Char)
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

  lea  ax, String_HelpControls
  push ax
  call IO_puts

  lea  ax, String_HelpLevel
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

IO_getc proc near ; IO (char) {{{1
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

