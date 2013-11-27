.MODEL small
.STACK 1024

include screen.inc

.DATA

_previousVideoMode db ?

.CODE

Screen_setup proc far ; -> (:) {{{1
  push bp
  mov  bp, sp

  mov  ax, 13h
  push ax
  call Video_setMode ; (13h)

  mov [_previousVideoMode], al

  mov sp, bp
  pop bp
  ret
Screen_setup endp

Screen_teardown proc far ; -> (:) {{{1
  push bp
  mov  bp, sp

  mov  al, [_previousVideoMode]
  push ax
  call Video_setMode ; (previous)

  mov sp, bp
  pop bp
  ret
Screen_teardown endp

; }}}1

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

; }}}1

END
