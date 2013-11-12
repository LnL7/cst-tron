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

player1 dw 3 dup(0)

player1_hardDirection   dw 0000b       ; rectangle direction
player1_position      dw 8192 dup(0) ; rectangle position offset
player1_lastOffsetIndex dw 2

.FARDATA? ; {{{1

palette      db 768 dup(0)
screenBuffer db 64000 dup(?) ; the 64000 bytes for the screen

; }}}1

.CODE

send macro object, procedure ; {{{1
  ; Call the given procedure with the object as the last argument
  ; [bp + 4][0] should be the object pointer
  mov  ax, offset object
  push ax
  call procedure
endm

arg macro value ; {{{1
  ; Pushes the given value on the stack
  ; used for procedure arguments
  mov ax, value
  push ax
endm

; }}}1

main proc near ; {{{1
  mov ax, @data ; get data segment address
  mov ds, ax    ; set DS to data segment

  call Random_Init        ; initialize random number generator
  call Keyboard_Install   ; install our own keyboard handler
  call Screen_fadeBlack   ; fade to black
  call Screen_clearBuffer ; clear video buffer
  call Screen_update      ; draw the screen buffer
  call Player_new1        ; create player1

  ; Set mode 13h
  mov  ax, 13h
  push ax
  call Video_SetMode
  mov  [oldVideoMode], al

@@:
  call World_update ; updates the world
  call World_render ; draws the world
  call Tail_increase
  call Input_update ; handle user input

  cmp  al, 0
  jz   @B

  ; Restore original keyboard handler
  call Keyboard_Uninstall

  xor  ah, ah
  push ax
  call Video_SetMode

  call Process_exit
main endp

; }}}1

Screen_fadeBlack proc near ; {{{1
  ; Fades the active colors to black
  push bp
  mov  bp, sp

  mov  ax, seg palette
  push ax
  mov  ax, offset palette
  push ax
  call Palette_Init

@@:
  call Video_Wait
  call Palette_Next
  test ax, ax
  jnz  @B

  mov sp, bp
  pop bp
  ret 0
Screen_fadeBlack endp

Screen_clearBuffer proc near ; {{{1
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
Screen_clearBuffer endp

Screen_update proc near ; {{{1
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
  call Video_Wait ; wait for a VB (modifies AX and DX)
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
Screen_update endp

; }}}1

World_update proc near ; {{{1
  push bp
  mov  bp, sp

  send player1, Player_getDirection

  cmp ax, DIRECTION_LEFT
  jnz @F

  dec [player1_position]
@@:

  cmp ax, DIRECTION_RIGHT
  jnz @F
  inc [player1_position]
@@:

  cmp ax, DIRECTION_UP
  jnz @F
  mov ax, SCREEN_WIDTH
  sub [player1_position], ax
@@:

  cmp ax, DIRECTION_DOWN
  jnz @F
  mov ax, SCREEN_WIDTH
  add [player1_position], ax
@@:

  mov sp, bp
  pop bp
  ret
World_update endp

World_render proc near ; {{{1
  push bp
  mov  bp, sp

  call Screen_clearBuffer

  ; Draw a random rectangle
  mov  ax, RECT_SIZE
  push ax
  push ax

  call Rectangle_render
  call Tail_render

  ; Draw the screen buffer
  call Screen_update

  mov sp, bp
  pop bp
  ret
World_render endp

; }}}1

Input_update proc near ; {{{1
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
  mov [player1_hardDirection], DIRECTION_UP
@@:

  mov al, es:[Keyboard_State][SCANCODE_DOWN] ; test DOWN key
  cmp al, 0
  jz  @F ; jump next
  ; Call some function to handle this key
  mov [player1_hardDirection], DIRECTION_DOWN
@@:

  mov al, es:[Keyboard_State][SCANCODE_LEFT]  ; test LEFT key
  cmp al, 0
  jz  @F ; jump next
  ; Call some function to handle this key
  mov [player1_hardDirection], DIRECTION_LEFT
@@:

  mov al, es:[Keyboard_State][SCANCODE_RIGHT] ; test RIGHT key
  cmp al, 0
  jz  @F ; jump next
  ; Call some function to handle this key
  mov [player1_hardDirection], DIRECTION_RIGHT
@@:

  ; Finally, let's put the ESC key status as return value in AX
  mov al, es:[Keyboard_State][SCANCODE_ESC] ; test ESC

@done:
  pop es

  mov sp, bp
  pop bp
  ret
Input_update endp

; }}}1

Tail_increase proc near ; {{{1
  push bp
  mov  bp, sp

  push bx
  push dx
  push di

  send player1, Player_position ; bx, di = offset POSITION
  mov bx, ax
  mov di, ax

  send player1, Player_getLastIndex ; bx += lastIndex
  add bx, ax

  mov ax, [di] ; POSITION[lastIndex] = POSITION[0]
  mov [bx], ax

  add player1_lastOffsetIndex, 2 ; lastIndex++

  send player1, Player_getLastIndex
  mov bx, 8192
  xor dx, dx
  div bx

  send player1, Player_lastIndex
  mov bx, ax
  mov [bx], dx

  pop di
  pop dx
  pop bx

  mov sp, bp
  pop bp
  ret
Tail_increase endp

Tail_render proc near ; {{{1
  push bp
  mov  bp, sp

  send player1, Player_position
  mov bx, ax

@loop:
  add bx, 2

  mov ax, [bx]
  cmp ax, 0
  jz @done

  push bx
  call Point_render
  jmp @loop
@done:

  mov sp, bp
  pop bp
  ret
Tail_render endp

; }}}1

Player_new1 proc near ; {{{1
  ; Initialize the player1 variable
  push bp
  mov  bp, sp

  mov ax, offset player1_hardDirection
  mov [player1][0], ax

  mov ax, offset player1_position
  mov [player1][2], ax

  mov ax, offset player1_lastOffsetIndex
  mov [player1][4], ax

  mov [player1_position], SCREEN_WIDTH
  inc [player1_position]

  mov sp, bp
  pop bp
  ret
Player_new1 endp

Player_getDirection proc near ; (offset PLAYER) {{{1
  push bp
  mov  bp, sp

  mov ax, [bp + 4][0] ; PLAYER
  mov bx, ax
  mov ax, [bx][0] ; offset DIRECTION
  mov bx, ax
  mov ax, [bx] ; DIRECTION

  mov sp, bp
  pop bp
  ret 2
Player_getDirection endp

Player_position proc near ; (offset PLAYER) {{{1
  push bp
  mov  bp, sp

  push bx

  mov ax, [bp + 4][0] ; PLAYER
  mov bx, ax
  mov ax, [bx][2] ; offset POSITION

  pop bx

  mov sp, bp
  pop bp
  ret 2
Player_position endp

Player_getLastIndex proc near ; (offset PLAYER) {{{1
  push bp
  mov  bp, sp

  push bx

  mov ax, [bp + 4][0] ; offset PLAYER
  mov bx, ax
  mov ax, [bx][4] ; offset INDEX
  mov bx, ax
  mov ax, [bx] ; INDEX

  pop bx

  mov sp, bp
  pop bp
  ret 2
Player_getLastIndex endp

Player_lastIndex proc near ; (offset PLAYER) {{{1
  push bp
  mov  bp, sp

  push bx

  mov ax, [bp + 4][0] ; offset PLAYER
  mov bx, ax
  mov ax, [bx][4] ; offset INDEX

  pop bx

  mov sp, bp
  pop bp
  ret
Player_lastIndex endp

; }}}1

Point_render proc near ; (OFFSET) {{{1
  push bp
  mov  bp, sp

  push bx
  push es

  mov ax, seg screenBuffer
  mov es, ax

  mov ax, [bp + 4][0] ; OFFSET
  mov bx, ax
  mov di, offset screenBuffer
  add di, [bx]

  mov al, 15
  mov es:[di], al ; set pixel

  pop es
  pop bx

  mov sp, bp
  pop bp
  ret 2
Point_render endp

Line_renderHorizontal proc near ; (OFFSET, WIDTH) {{{1
  push bp
  mov  bp, sp

  push cx
  push di
  push es

  mov ax, seg screenBuffer
  mov es, ax

  mov ax, [bp + 4][2] ; OFFSET
  mov bx, ax
  mov ax, offset screenBuffer
  add ax, [bx]

  ; Draw upper horizontal line
  mov di, ax
  mov cx, [bp + 4][0] ; WIDTH
  mov al, 15          ; color
  cld
  rep stosb           ; draw

  pop es
  pop di
  pop cx
 
  mov sp, bp
  pop bp
  ret 4
Line_renderHorizontal endp

Rectangle_render proc near ; (WIDTH, HEIGHT) {{{1
  ; Draw a rectangle at the center of the screen buffer.
  ; W, H passed on stack.
  push  bp
  mov bp, sp

  push  cx
  push  di
  push  es

  ; Set segment
  mov ax, seg screenBuffer
  mov es, ax

  mov ax, offset screenBuffer
  add ax, [player1_position]

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
  pop cx

  mov sp, bp
  pop bp
  ret 4
Rectangle_render endp

; }}}1

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
