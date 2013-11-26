.MODEL large
.STACK 2048

include string.inc

.DATA

_previousVideoMode db ?

.CODE

main proc near ; {{{1
  ; Data Segment & Extra Segment
  mov ax, @data
  mov ds, ax
  mov es, ax

  call Game_setup
  xor  ax, ax
  call Game_run
  call Game_teardown

  call Process_exit
main endp

; }}}1

Game_run proc near ; ax => (0x00) {{{1
  ; Tail recursive game loop

  jnz Game_run
  ret
Game_run endp

Game_setup proc near ; -> (:) {{{1
  push bp
  mov  bp, sp

  mov  ax, 13h
  push ax
  call Video_setMode ; (13h)

  mov [_previousVideoMode], al

  mov sp, bp
  pop bp
  ret
Game_setup endp

Game_teardown proc near ; -> (:) {{{1
  push bp
  mov  bp, sp

  mov  al, [_previousVideoMode]
  push ax
  call Video_setMode ; (previous)

  mov  ax, offset String_kOk
  push ax
  call IO_puts ; ("OK. ")

  mov sp, bp
  pop bp
  ret
Game_teardown endp

Video_setMode proc near ; (mode) -> (previous) {{{1
  ; mode     :: (:VideoMode)
  ; previous :: (:VideoMode)
  push bp
  mov  bp, sp
  push bx

  mov ah, 0fh ; al <- VideoMode
  int 10h

  mov bx, ax

  mov ax, [bp + 4][0] ; (:mode) >>= VideoMode
  int 10h

  mov ax, bx

  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (mode)
Video_setMode endp

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

