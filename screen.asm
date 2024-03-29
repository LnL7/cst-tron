.MODEL large

include screen.inc

DOS_kMode  equ 0fh
DOS_kVideo equ 0a000h

.FARDATA?

Video_previousMode db ?
Video_buffer       db 64000 dup(?)

.CODE

assume ds:@fardata?, es:@fardata?

Screen_setPixel proc far ; (color, position) -> IO () {{{1
  ; color :: Color
  ; position :: Int
  push bp
  mov  bp, sp
  push bx
  push ds

  mov ax, @fardata?
  mov ds, ax

  mov ax, [bp + 6][0] ; position
  mov bx, ax
  lea bx, [Video_buffer][bx] ; Buffer[position]

  mov ax, [bp + 6][2] ; color
  mov [bx], al ; Buffer[position] <- color

  pop ds
  pop bx
  mov sp, bp
  pop bp
  retf 2 ; (position)
Screen_setPixel endp

Screen_setLine proc far ; (color, size, position) -> IO () {{{1
  push bp
  mov  bp, sp
  push cx
  push di
  push es

  ; Data Segment
  mov ax, @fardata?
  mov es, ax

  mov ax, [bp + 6][0] ; di <- position
  mov di, ax
  mov ax, [bp + 6][2] ; cx <- size
  mov cx, ax
  mov ax, [bp + 6][4] ; al <- color
  cld
  rep stosb ; al >>= (es:di)

  pop es
  pop di
  pop cx
  mov sp, bp
  pop bp
  ret 6 ; (color, size, position)
Screen_setLine endp

Screen_update proc far ; IO () {{{1
  push bp
  mov  bp, sp

  call Video_write
  call Video_clear

  mov sp, bp
  pop bp
  retf
Screen_update endp

Screen_setup proc far ; IO () {{{1
  ; mode     :: VideoMode
  ; previous :: VideoMode
  push bp
  mov  bp, sp
  push ds

  mov ax, @fardata?
  mov ds, ax

  mov  ax, 13h ; mode
  push ax
  call Video_setMode ; (mode)

  mov [Video_previousMode], al ; previous <- VideoMode (al)

  call Video_clear

  pop ds
  mov sp, bp
  pop bp
  retf
Screen_setup endp

Screen_teardown proc far ; IO () {{{1
  ; previous :: VideoMode
  push bp
  mov  bp, sp
  push ds

  mov ax, @fardata?
  mov ds, ax

  mov  al, [Video_previousMode] ; al <- VideoMode (previous)
  push ax
  call Video_setMode ; (previous)

  pop ds
  mov sp, bp
  pop bp
  retf
Screen_teardown endp

Video_write proc near ; IO () {{{1
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
  mov ax, offset Video_buffer
  mov si, ax

  ; Extra Segment
  mov ax, DOS_kVideo
  mov es, ax

  xor di, di ; pixel 0

  cld
  mov cx, 64000 / 02h
  rep movsw ; si >>= VideoMemory (es:di)

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

Video_clear proc near ; IO () {{{1
  push bp
  mov  bp, sp
  push cx
  push di
  push es

  mov ax, @fardata?
  mov es, ax

  ; Destination Index
  mov ax, offset Video_buffer
  mov di, ax

  mov cx, 64000 / 02h
  mov al, Screen_kBlack
  mov ah, Screen_kBlack
  rep stosw ; ax >>= VideoBuffer (es:di)

  pop es
  pop di
  pop cx
  mov sp, bp
  pop bp
  ret
Video_clear endp

Video_setMode proc near ; (mode) -> IO (previous) {{{1
  ; mode     :: VideoMode
  ; previous :: VideoMode
  push bp
  mov  bp, sp
  push bx

  mov ah, DOS_kMode ; al <- VideoMode
  int 10h

  mov bx, ax

  mov ax, [bp + 4][0] ; mode >>= VideoMode (ax)
  xor ah, ah
  int 10h

  mov ax, bx

  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (mode)
Video_setMode endp

; }}}1

END
