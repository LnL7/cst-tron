.MODEL large
.STACK 2048

include keyboard.inc
include random.inc
include scancode.inc
include video.inc

; Other constants
SCREEN_WIDTH    equ 320
SCREEN_HEIGHT   equ 200
RECT_SIZE       equ 5
DIRECTION_LEFT  equ 0001b
DIRECTION_RIGHT equ 0010b
DIRECTION_DOWN  equ 0100b
DIRECTION_UP    equ 1000b

.DATA ; {{{1

oldVideoMode  db ?

hardOffset    dw 0     ; rectangle position offset
hardDirection dw 0000b ; rectangle direction

.FARDATA? ; {{{1

palette      db 768 dup(0)
screenBuffer db 64000 dup(?) ; the 64000 bytes for the screen

.CODE ; {{{1

main proc near ; {{{1
  mov ax, @data ; get data segment address
  mov ds, ax    ; set DS to data segment

  call Random_init             ; initialize random number generator
  call Keyboard_installHandler ; install our own keyboard handler
  call fadeToBlack             ; fade to black
  call clearScreenBuffer       ; clear video buffer
  call updateScreen            ; draw the screen buffer

  ; Set mode 13h
  mov  ax, 13h
  push ax
  call Video_setMode
  mov  [oldVideoMode], al

@@:
  call updateWorld ; updates the world
  call renderWorld ; draws the world
  call handleInput ; handle user input
  cmp  al, 0
  jz   @B

  ; Restore original keyboard handler
  call Keyboard_uninstallHandler

  xor  ah, ah
  push ax
  call Video_setMode

  call Process_exit
main endp

fadeToBlack proc near ; {{{1
  ; Fades the active colors to black
  push bp
  mov  bp, sp

  mov  ax, seg palette
  push ax
  mov  ax, offset palette
  push ax
  call Palette_initFade

@@:
  call Video_wait
  call Palette_nextFade
  test ax, ax
  jnz  @B

  mov sp, bp
  pop bp
  ret 0
fadeToBlack endp

clearScreenBuffer proc near ; {{{1
  ; Clears the screen buffer to color 0
  push bp
  mov  bp, sp

  push cx
  push di
  push es

  cld
  mov ax, seg screenBuffer
  mov es, ax
  mov di, offset screenBuffer
  mov cx, 64000 / 2
  xor ax, ax
  rep stosw

  pop es
  pop di
  pop cx

  mov sp, bp
  pop bp
  ret
clearScreenBuffer endp

updateScreen proc near ; {{{1
  ; Updates the screen (copies contents from screenBuffer to screen)
  push bp
  mov  bp, sp

  push cx
  push dx
  push si
  push di
  push ds
  push es

  ; Setup source and dest segments
  mov ax, seg screenBuffer
  mov ds, ax
  mov si, offset screenBuffer
  mov ax, 0a000h ; video memory
  mov es, ax
  xor di, di     ; start at pixel 0

  cld
  mov  cx, 64000 / 2
  call Video_wait ; wait for a VB (modifies AX and DX)
  rep  movsw      ; blit to screen

  pop es
  pop ds
  pop di
  pop si
  pop dx
  pop cx
  pop ax


  mov sp, bp
  pop bp
  ret
updateScreen endp

updateWorld proc near ; {{{1
  push bp
  mov  bp, sp

  mov ax, [hardDirection]

  cmp ax, DIRECTION_LEFT
  jnz @F
  dec [hardOffset]
@@:

  cmp ax, DIRECTION_RIGHT
  jnz @F
  inc [hardOffset]
@@:

  cmp ax, DIRECTION_UP
  jnz @F
  mov ax, SCREEN_WIDTH
  sub [hardOffset], ax
@@:

  cmp ax, DIRECTION_DOWN
  jnz @F
  mov ax, SCREEN_WIDTH
  add [hardOffset], ax
@@:

  mov sp, bp
  pop bp
  ret
updateWorld endp

renderWorld proc near ; {{{1
  push bp
  mov  bp, sp

  call clearScreenBuffer

  ; Draw a random rectangle
  mov  ax, RECT_SIZE
  push ax

  call Random_dw
  or   ax, 01h
  and  ax, 0fh
  push ax

  call drawRect

  ; Draw the screen buffer
  call updateScreen

  mov sp, bp
  pop bp
  ret
renderWorld endp

handleInput proc near ; {{{1
  ; Reads keyboard buffer and acts (returns non-zero if loop should end, 0 otherwise)
  push bp
  mov  bp, sp

  push  es

  mov ax, seg Keyboard_ActiveKeys
  mov es, ax

  xor ah, ah
  mov al, es:[Keyboard_ActiveKeys]
  cmp al, 0
  jz  @done ; no key pressed

  ; Handle keys
  mov al, es:[Keyboard_State][SCANCODE_UP] ; test UP key
  cmp al, 0
  jz  @F ; jump next

  ; Call some function to handle this key
  mov [hardDirection], DIRECTION_UP
@@:

  mov al, es:[Keyboard_State][SCANCODE_DOWN] ; test DOWN key
  cmp al, 0
  jz  @F ; jump next
  ; Call some function to handle this key
  mov [hardDirection], DIRECTION_DOWN
@@:

  mov al, es:[Keyboard_State][SCANCODE_LEFT]  ; test LEFT key
  cmp al, 0
  jz  @F ; jump next
  ; Call some function to handle this key
  mov [hardDirection], DIRECTION_LEFT
@@:

  mov al, es:[Keyboard_State][SCANCODE_RIGHT] ; test RIGHT key
  cmp al, 0
  jz  @F ; jump next
  ; Call some function to handle this key
  mov [hardDirection], DIRECTION_RIGHT
@@:

  ; Finally, let's put the ESC key status as return value in AX
  mov al, es:[Keyboard_State][SCANCODE_ESC] ; test ESC

@done:
  pop es

  mov sp, bp
  pop bp
  ret
handleInput endp

drawRect proc near ; (WIDTH, HEIGHT) {{{1
  ; Draw a rectangle at the center of the screen buffer.
  ; W, H passed on stack.
  push  bp
  mov bp, sp

  push  ax
  push  bx
  push  cx
  push  dx
  push  di
  push  es

  ; Set segment
  mov ax, seg screenBuffer
  mov es, ax

  ; Calculate posX
  mov ax, [bp + 4][2]
  neg ax
  add ax, SCREEN_WIDTH
  shr ax, 1
  mov bx, ax ; posX is in BX now

  ; Calculate posY
  mov ax, [bp + 4][0]
  neg ax
  add ax, SCREEN_HEIGHT
  shr ax, 1 ; and posY is in AX

  ; Calculate offset of top-left corner
  mov dx, SCREEN_WIDTH
  mul dx     ; AX = posY * SCREEN_WIDTH
  add ax, bx ; AX now contains start offset of rectangle
  add ax, offset screenBuffer
  add ax, [hardOffset]

  ; Draw upper horizontal line
  mov di, ax
  mov cx, [bp + 4][2] ; rect W
  mov al, 15          ; color
  cld
  rep stosb           ; draw

  ; Draw right vertical line
  mov cx, [bp + 4][0] ; rect H
  dec di

@@:
  mov es:[di], al ; set pixel
  add di, SCREEN_WIDTH ; jump to next pixel (on next line)
  loop  @B

  ; Draw bottom horizontal line
  mov cx, [bp+4][2] ; rect W
  std ; count backwards
  rep stosb ; draw

  ; Draw left vertical line
  mov cx, [bp + 4][0] ; rect H
  inc di

@@:
  mov es:[di], al ; set pixel
  sub di, SCREEN_WIDTH ; jump to next pixel (on next line)
  loop  @B

  ; We are done
  pop es
  pop di
  pop dx
  pop cx
  pop bx
  pop ax

  mov sp, bp
  pop bp
  ret 4
drawRect endp

Process_exit proc near ; {{{1
  ; Exit to DOS
  push bp
  mov  bp, sp

  mov ax, 4c00h
  int 21h

  mov sp, bp
  pop bp
  ret
Process_exit endp

; }}}1

END main
