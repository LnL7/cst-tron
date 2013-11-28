.MODEL large

include screen.inc

.FARDATA?

_previousVideoMode db ?
_pixelBuffer       db 64000 dup(?)

.CODE

assume ds:@fardata?, es:@fardata?

Screen_setPixel proc far ; (position) -> IO () {{{1
  ; position :: Int
  push bp
  mov  bp, sp
  push bx
  push ds

  mov ax, @fardata?
  mov ds, ax
  mov es, ax

  mov ax, offset _pixelBuffer
  add ax, [bp + 6][0] ; position
  mov bx, ax

  mov al, Screen_kWhite
  mov [bx], al

  pop ds
  pop bx
  mov sp, bp
  pop bp
  retf 2 ; (position)
Screen_setPixel endp

Screen_update proc far ; -> (:) {{{1
  push bp
  mov  bp, sp

  ; TODO: don't mess with the buffer
  ; mov [_pixelBuffer + 0],   Screen_kWhite
  ; mov [_pixelBuffer + 321], Screen_kWhite
  ; mov [_pixelBuffer + 642], Screen_kWhite
  ; mov [_pixelBuffer + 963], Screen_kWhite

  call Video_write
  call Video_clear

  mov sp, bp
  pop bp
  retf
Screen_update endp

Screen_setup proc far ; -> (:) {{{1
  push bp
  mov  bp, sp
  push ds

  mov ax, @fardata?
  mov ds, ax

  mov  ax, 13h
  push ax
  call Video_setMode ; (13h)

  mov [_previousVideoMode], al

  pop ds
  mov sp, bp
  pop bp
  retf
Screen_setup endp

Screen_teardown proc far ; -> (:) {{{1
  push bp
  mov  bp, sp
  push ds

  mov ax, @fardata?
  mov ds, ax

  mov  al, [_previousVideoMode]
  push ax
  call Video_setMode ; (previous)

  pop ds
  mov sp, bp
  pop bp
  retf
Screen_teardown endp

; }}}1

Video_write proc near ; -> (:) {{{1
  push bp
  mov  bp, sp
  push cx
  push dx
  push si
  push di
  push ds
  push es

  mov ax, @fardata?
  mov ds, ax

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
  pop ds
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
  push es

  mov ax, @fardata?
  mov es, ax

  ; Destination Index
  mov ax, offset _pixelBuffer
  mov di, ax

  mov cx, 64000 / 2
  mov al, Screen_kBlack
  mov ah, Screen_kBlack
  rep stosw ; ax >>= (es:di)

  pop es
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
