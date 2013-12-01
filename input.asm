.MODEL large

include input.inc

DOS_kHandler    equ 3509h
DOS_kSetHandler equ 2509h

KEY_kBuffer  equ 60h
KEY_kControl equ 61h
KEY_kPicPort equ 20h

KEY_kEsc       equ 01h
KEY_kOne       equ 02h
KEY_kTwo       equ 03h
KEY_kThree     equ 04h
KEY_kFour      equ 05h
KEY_kFive      equ 06h
KEY_kSix       equ 07h
KEY_kSeven     equ 08h
KEY_kEight     equ 09h
KEY_kNine      equ 0ah
KEY_kZero      equ 0bh
KEY_kBackspace equ 0eh
KEY_kTab       equ 0fh
KEY_kUp        equ 48h
KEY_kDown      equ 50h
KEY_kLeft      equ 4bh
KEY_kRight     equ 4dh

.FARDATA?

Keyboard_previousHandler dw 2 dup(?)
Keyboard_state           db 128 dup(?)
Keyboard_active          db ?
Keyboard_scanCode        db ?

.CODE

assume ds:@fardata?, es:@fardata?

Input_isActive proc far ; IO (active) {{{1
  ; active :: Bool
  push bp
  mov  bp, sp
  push ds

  ; Data Segment
  mov ax, @fardata?
  mov ds, ax

  mov al, [Keyboard_active] ; al <- Keyboard (active)
  xor ah, ah

  pop ds
  mov sp, bp
  pop bp
  retf
Input_isActive endp

Input_arrowKeys proc far ; (segment, direction) -> IO () {{{1
  ; segment   :: Seg Player (direction)
  ; direction :: Offset Player (direction)
  push bp
  mov  bp, sp
  push bx
  push dx
  push ds

  ; Data Segment
  mov ax, @fardata?
  mov ds, ax

  mov dx, Input_kNone

  mov al, [Keyboard_state][KEY_kUp]
  cmp al, 0
  jz @F
  mov dx, Input_kUp
@@:

  mov al, [Keyboard_state][KEY_kDown]
  cmp al, 0
  jz @F
  mov dx, Input_kDown
@@:

  mov al, [Keyboard_state][KEY_kLeft]
  cmp al, 0
  jz @F
  mov dx, Input_kLeft
@@:

  mov al, [Keyboard_state][KEY_kRight]
  cmp al, 0
  jz @F
  mov dx, Input_kRight
@@:

  cmp dx, Input_kNone
  je  @F

  ; Data Segment
  mov ax, [bp + 6][2] ; segment
  mov ds, ax

  mov ax, [bp + 6][0] ; direction
  mov bx, ax
  mov [bx], dx
@@:

  pop ds
  pop dx
  pop bx
  mov sp, bp
  pop bp
  retf 4 ; (segment, direction)
Input_arrowKeys endp

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
  push ax
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
  pop ax
  iret
Keyboard_handler endp

; }}}1

END
