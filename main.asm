.MODEL large
.STACK 2048

include game.inc
include screen.inc

.DATA

String_Ok db "OK. $"

.CODE

assume ds:@data, es:@data

main proc near ; {{{1
  ; Data Segment & Extra Segment
  mov ax, @data
  mov ds, ax
  mov es, ax

  call Screen_setup
  call Game_run
  call Screen_teardown

  mov  ax, offset String_Ok
  push ax
  call IO_puts ; ("OK. ")

  call Process_exit
main endp

; }}}1

IO_puts proc near ; (string) -> (0x09:) {{{1
  ; string :: Offset [Char]
  push bp
  mov  bp, sp
  push dx

  mov ax, [bp + 4][0] ; dx <- string
  mov dx, ax

  mov ah, 09h ; dx >>= stdout
  int 21h

  pop dx
  mov sp, bp
  pop bp
  ret 2 ; (string)
IO_puts endp

Process_exit proc near ; -> (0x00) {{{1
  mov ax, 4c00h ; 0x00 >>= exit
  int 21h
Process_Exit endp

; }}}1

END main

