.MODEL large

include input.inc

DOS_kHandler    equ 3509h
DOS_kSetHandler equ 2509h

KEY_kBuffer  equ 60h
KEY_kControl equ 61h
KEY_kPicPort equ 20h

.FARDATA?

Keyboard_previousHandler dw 2 dup(?)
Keyboard_state           db 128 dup(?)
Keyboard_active          db ?
Keyboard_scanCode        db ?

.CODE

assume ds:@fardata?, es:@fardata?

Input_setup proc far ; IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push ds
  push es

  mov ax, @fardata?
  mov ds, ax

  call Keyboard_clearState

  mov ax, DOS_kHandler ; ax >>= InterruptHandler (es:bx)
  int 21h

  mov ax, es
  mov [Keyboard_previousHandler][2], ax ; previous <- Seg (InterruptHandler)
  mov [Keyboard_previousHandler][0], bx ; previous <- Offset (InterruptHandler)

  mov  ax, seg Keyboard_handler
  push ax
  mov  ax, offset Keyboard_handler
  push ax
  call Keyboard_setHandler ; (segment, offset)

  pop es
  pop ds
  pop bx
  mov sp, bp
  pop bp
  retf
Input_setup endp

Input_teardown proc far ; IO () {{{1
  push bp
  mov  bp, sp
  push dx
  push ds

  ; Data Segment
  mov ax, @fardata?
  mov ds, ax


  mov ax, [Keyboard_previousHandler][2] ; segment <- previous
  push ax
  mov ax, [Keyboard_previousHandler][0] ; offset <- previous
  push ax
  call Keyboard_setHandler ; (segment, offset)

  pop ds
  pop dx
  mov sp, bp
  pop bp
  retf
Input_teardown endp

Keyboard_clearState proc near ; IO () {{{1
  push bp
  mov  bp, sp
  push cx
  push di
  push ds
  push es

  mov ax, @fardata?
  mov ds, ax
  mov es, ax

  mov cx, (128 / 2) + 1
  mov di, offset Keyboard_state
  xor ax, ax
  cld
  rep stosw ; 0x00 >>= KeyState (es:di)

  pop es
  pop ds
  pop di
  pop cx
  mov sp, bp
  pop bp
  ret
Keyboard_clearState endp

Keyboard_setHandler proc near ; (segment, offset) -> IO () {{{1
  ; segment :: Seg InterruptHandler
  ; offset  :: Offset InterruptHandler
  push bp
  mov  bp, sp
  push dx
  push ds

  ; Data Segment
  mov ax, [bp + 4][2] ; segment
  mov ds, ax

  mov ax, [bp + 4][0] ; offset
  mov dx, ax

  mov ax, DOS_kSetHandler
  int 21h ; ax >>= InterruptHandler (ds:dx)

  pop ds
  pop dx
  mov sp, bp
  pop bp
  ret 4 ; (segment, offset)
Keyboard_setHandler endp

Keyboard_handler proc near ; Interrupt () {{{1
  push bx
  push si
  push ds

  mov ax, @fardata?
  mov ds, ax

  sti
  in  al, KEY_kBuffer
  mov bl, al
  mov [Keyboard_scanCode], al
  in  al, KEY_kControl
  or  al, 82h
  out KEY_kControl, al
  and al, 7fh
  out KEY_kControl, al
  mov al, 20h
  out KEY_kPicPort, al

  mov al, bl
  shl ax, 1
  not ah
  and ah, 1
  shr al, 1
  xor bx, bx
  mov bl, al
  lea si, [Keyboard_state][bx]
  mov al, [si]

  neg al
  add al, ah
  add [Keyboard_active], al
  mov al, ah
  mov [si], al

  pop ds
  pop si
  pop bx
  iret
Keyboard_handler endp

; }}}1

END
