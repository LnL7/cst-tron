.MODEL large

include screen.inc

.DATA

_previousVideoMode db ?
_pixelBuffer       db 64000 dup(?)

.CODE

Screen_update proc far ; -> (:) {{{1
  push bp
  mov  bp, sp

  ; TODO: don't mess with the buffer
  mov [_pixelBuffer + 0],   Screen_kWhite
  mov [_pixelBuffer + 321], Screen_kWhite
  mov [_pixelBuffer + 642], Screen_kWhite
  mov [_pixelBuffer + 963], Screen_kWhite

  call Video_write
  call Video_clear

  mov sp, bp
  pop bp
  ret
Screen_update endp

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

Video_write proc near ; -> (:) {{{1
  push bp
  mov  bp, sp
  push cx
  push dx
  push si
  push di
  push es

  ; Source Index
  mov ax, offset _pixelBuffer
  mov si, ax

  ; Extra Segment
  mov ax, 0a000h ; video memory
  mov es, ax

  xor di, di ; pixel 0

  cld
  mov cx, 64000 / 2
  ; TODO: wait for VBlank
  rep movsw ; blit to screen

  pop es
  pop di
  pop si
  pop dx
  pop cx
  mov sp, bp
  pop bp
  ret
Video_write endp

Video_clear proc near ; -> (:) {{{1
  push bp
  mov  bp, sp
  push cx
  push di

  ; Destination Index
  mov ax, offset _pixelBuffer
  mov di, ax

  mov cx, 64000 / 2
  mov ax, Screen_kBlack
  rep stosw

  pop di
  pop cx
  mov sp, bp
  pop bp
  ret
Video_clear endp

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
