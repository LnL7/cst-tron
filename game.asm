.MODEL large

include game.inc
include screen.inc
include input.inc

CHAR_kOne   equ 49 ; 1 :: ASCII
CHAR_kTwo   equ 50 ; 2 :: ASCII
CHAR_kThree equ 51 ; 3 :: ASCII

TAG_kColor        equ 00h
TAG_kDirection    equ 02h
TAG_kPosition     equ 04h
TAG_kIndex        equ 06h
TAG_kPlayerSize   equ 08h
TAG_kBorderSize   equ 2*02h*Screen_kWidth + 2*02h*Screen_kHeight ; words
TAG_kMaxLevelSize equ 02h*Screen_kWidth + 02h*Screen_kHeight ; words

TAG_kPlayerLeft  equ 0*TAG_kPlayerSize
TAG_kPlayerRight equ 1*TAG_kPlayerSize
TAG_kLevel       equ 2*TAG_kPlayerSize
TAG_kTails       equ TAG_kLevel + TAG_kBorderSize + TAG_kMaxLevelSize

Player_kSize equ 1

.FARDATA

Game_data         dw 32000 dup(0)
Player_directions dw 5 dup(0) ; Jump table with label offsets

.CODE

Player_left macro dest ; dest <- Offset player {{{1
  lea dest, [Game_data + TAG_kPlayerLeft]
endm

Player_leftGet macro tag ; ax <- Player[tag] {{{1
  mov ax, [Game_data + TAG_kPlayerLeft][tag]
endm

Player_leftSet macro tag, src ; Player[tag] <- src {{{1
  mov [Game_data + TAG_kPlayerLeft][tag], src
endm

Player_right macro dest ; dest <- Offset player {{{1
  lea dest, [Game_data + TAG_kPlayerRight]
endm

Player_rightGet macro tag ; ax <- Player[tag] {{{1
  mov ax, [Game_data + TAG_kPlayerRight][tag]
endm

Player_rightSet macro tag, src ; Player[tag] <- src {{{1
  mov [Game_data + TAG_kPlayerRight][tag], src
endm

; }}}1

assume ds:@fardata, es:@fardata

Game_run proc far ; (speed) -> IO (player) {{{1
  ; speed :: Int
  push bp
  mov  bp, sp
  push bx
  push ds

  mov ax, @fardata
  mov ds, ax

@@:
  call Player_input

  mov  ax, [bp + 6][0] ; speed
  push ax
  call Game_update ; ax <- (count)
  cmp ax, 0 ; collision?
  jne @done

  Player_left ax
  push ax
  call Player_render ; (player)
  Player_right ax
  push ax
  call Player_render ; (player)

  call Level_render

  call Screen_update

  call Input_escapeKey
  cmp ax, 0
  je @B

@done:

  pop ds
  pop bx
  mov sp, bp
  pop bp
  retf 2;
Game_run endp

Game_update proc near ; (speed) -> IO (bool) {{{1
  ; speed :: Int
  push bp
  mov  bp, sp
  push cx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; speed
  mov cx, ax

@@:
  Player_left ax
  push ax
  call Player_update ; (player)
  Player_right ax
  push ax
  call Player_update ; (player)
  call Game_collide
  cmp ax, 0
  jne @done

  dec cx
  jnz @B
@done:

  pop ds
  pop cx
  mov sp, bp
  pop bp
  ret 2 ; (speed)
Game_update endp

Game_setup proc far ; (char) -> IO () {{{1
  ; char :: ASCII
  push bp
  mov  bp, sp

  call Player_alloc
  call Player_init
  call Level_init ; borders

  ; Horizontal
  mov ax, [bp + 6][0] ; char
  cmp al, CHAR_kOne
  jne @F
  call Level_initHorizontal
@@:

  ; Vertical
  mov ax, [bp + 6][0] ; char
  cmp al, CHAR_kTwo
  jne @F
  call Level_initVertical
@@:

  ; Cross
  mov ax, [bp + 6][0] ; char
  cmp al, CHAR_kThree
  jne @F
  call Level_initHorizontal
  call Level_initVertical
@@:

  mov sp, bp
  pop bp
  retf 2 ; (char)
Game_setup endp

Game_collide proc near ; IO (Bool) {{{1
  push bp
  mov  bp, sp
  push bx
  push cx
  push dx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  Player_leftGet TAG_kPosition
  mov cx, ax
  Player_rightGet TAG_kPosition
  mov dx, ax

  Player_rightGet TAG_kIndex
  mov bx, ax

@@:
  cmp [bx], cx ; Tail[index] == position
  mov ah, Game_kPlayerLeft
  je  @done

  cmp [bx], dx ; Tail[index] == position
  mov ah, Game_kPlayerRight
  je  @done

  sub bx, 1*02h
  cmp bx, [Game_data + TAG_kLevel]
  jne @B

  xor ax, ax ; return false

@done:
  ; return true

  pop ds
  pop dx
  pop cx
  pop bx
  mov sp, bp
  pop bp
  ret
Game_collide endp

Player_alloc proc near ; IO () {{{1
  push bp
  mov  bp, sp
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  ; Allocate left player
  mov al, Screen_kCyan ; Color (tail)
  mov ah, Screen_kBlue ; Color (head)
  Player_leftSet TAG_kColor, ax ; color = (White:White)

  mov ax, Input_kRight
  Player_leftSet TAG_kDirection, ax ; direction = None

  mov ax, 10*Screen_kWidth + 10
  Player_leftSet TAG_kPosition, ax

  lea ax, [Game_data + TAG_kTails][0*02h]
  Player_leftSet TAG_kIndex, ax ; index = Tail[0]

  ; Allocate right player
  mov al, Screen_kYellow ; Color (tail)
  mov ah, Screen_kRed ; Color (head)
  Player_rightSet TAG_kColor, ax ; color = (White:White)

  mov ax, Input_kLeft
  Player_rightSet TAG_kDirection, ax ; direction = None

  mov ax, 11*Screen_kWidth - 9
  Player_rightSet TAG_kPosition, ax

  lea ax, [Game_data + TAG_kTails][1*02h]
  Player_rightSet TAG_kIndex, ax ; index = Tail[1]

  pop ds
  mov sp, bp
  pop bp
  ret
Player_alloc endp

Player_init proc near ; IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  xor bh, bh

  ; Initialize Jump Table
  lea ax, Player_moveNone
  mov [Player_directions][Input_kNone], ax

  mov al, Input_kUp
  and al, Input_kVerticalMask ; remove vertical flag
  mov bl, al
  lea ax, Player_moveUp
  mov [Player_directions][bx], ax

  mov al, Input_kDown
  and al, Input_kVerticalMask ; remove vertical flag
  mov bl, al
  lea ax, Player_moveDown
  mov [Player_directions][bx], ax

  lea ax, Player_moveLeft
  mov [Player_directions][Input_kLeft], ax
  lea ax, Player_moveRight
  mov [Player_directions][Input_kRight], ax

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret
Player_init endp

Player_input proc near ; IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  call Input_isActive
  cmp al, 0 ; false
  je  @done

@left:
  call Input_wsdaKeys
  cmp al, Input_kNone
  je  @right

  push ax
  Player_leftGet TAG_kDirection
  mov bx, ax
  pop ax

  and bl, Input_kVerticalFlag ; current vertical?
  mov bh, al
  and bh, Input_kVerticalFlag ; input vertical?
  cmp bl, bh
  je  @right ; did not change horizontal/vertical

  Player_leftSet TAG_kDirection, ax

@right:
  call Input_arrowKeys
  cmp al, Input_kNone
  je  @done

  push ax
  Player_rightGet TAG_kDirection
  mov bx, ax
  pop ax

  and bl, Input_kVerticalFlag ; current vertical?
  mov bh, al
  and bh, Input_kVerticalFlag ; input vertical?
  cmp bl, bh
  je  @done ; did not change horizontal/vertical

  Player_rightSet TAG_kDirection, ax

@done:

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret
Player_input endp

Player_render proc near ; (player) -> IO () {{{1
  ; player :: Offset Player
  push bp
  mov  bp, sp

  mov  ax, [bp + 4][0] ; player
  push ax
  call Player_renderTail

  mov  ax, [bp + 4][0] ; player
  push ax
  call Player_renderHead

  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_render endp

Player_renderTail proc near ; (player) -> IO () {{{1
  ; player :: Offset Player
  push bp
  mov  bp, sp
  push bx
  push dx
  push si
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  mov ax, [bx][TAG_kColor]
  mov dx, ax ; dl <- Player (_:color)

  push bx
  lea bx, [bx][TAG_kIndex]
  mov ax, [bx]
  mov si, ax ; si <- Player[index]
  pop bx

  lea bx, [Game_data + TAG_kTails]
  sub si, 2*02h

@@:
  push dx
  mov  ax, [si]
  push ax
  call Screen_setPixel ; (color, position)

  ; Increment word offset (= 2 bytes)
  ; Skip over other player
  sub si, 2*02h

  cmp si, bx
  jae @B ; unless(bx == END) unsigned interger

  pop ds
  pop si
  pop dx
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_renderTail endp

Player_renderHead proc near ; (player) -> IO () {{{1
  ; TODO: cleanup
  ; player :: Offset Player
  push bp
  mov  bp, sp
  push bx
  push cx
  push dx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  mov ax, [bx][TAG_kColor]
  mov al, ah ; al <- Player (color:_)
  mov dx, ax

  mov ax, [bx][TAG_kPosition]
  sub ax, Player_kSize*Screen_kWidth
  sub ax, Player_kSize - 1
  mov bx, ax ; bx <- top left position
  mov cx, Player_kSize*2 + 1 ; height

@@:

  push dx
  mov  ax, Player_kSize*2 + 1 ; width
  push ax
  push bx
  call Screen_setLine ; (color, size, position)

  add bx, Screen_kWidth
  dec cx
  jnz @B

  pop ds
  pop dx
  pop cx
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_renderHead endp

Player_update proc near ; (player) {{{1
  ; player :: Offset Player
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  push bx
  call Player_setTail ; (player)

  add word ptr [bx][TAG_kIndex], 2*02h ; index++

  mov ax, [bx][TAG_kDirection]
  mov bx, ax
  and bl, Input_kVerticalMask ; remove vertical flag

  mov  ax, [bp + 4][0] ; player
  push ax
  call [Player_directions][bx] ; call Player_move{Direction} (player)

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_update endp

Player_moveNone proc near ; (player) -> IO () {{{1
  push bp
  mov  bp, sp
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveNone endp

Player_moveUp proc near ; (player) -> IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  sub word ptr [bx][TAG_kPosition], Screen_kWidth

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveUp endp

Player_moveDown proc near ; (player) -> IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  add word ptr [bx][TAG_kPosition], Screen_kWidth

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveDown endp

Player_moveLeft proc near ; (player) -> IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  dec word ptr [bx][TAG_kPosition]

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveLeft endp

Player_moveRight proc near ; (player) -> IO () {{{1
  ; player :: Offset Player
  push bp
  mov  bp, sp
  push bx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  inc word ptr [bx][TAG_kPosition]

  pop ds
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_moveRight endp

Player_setTail proc near ; (player) -> IO () {{{1
  ; player :: Offset Player
  push bp
  mov  bp, sp
  push bx
  push dx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov ax, [bp + 4][0] ; player
  mov bx, ax

  mov dx, [bx][TAG_kPosition]
  mov ax, [bx][TAG_kIndex]
  mov bx, ax

  mov [bx], dx ; Player[index] = position

  pop ds
  pop dx
  pop bx
  mov sp, bp
  pop bp
  ret 2 ; (player)
Player_setTail endp

Level_init proc near ; IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push cx
  push dx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  lea bx, [Game_data + TAG_kLevel]

  mov cx, Screen_kWidth
@@:
  mov [bx], cx

  add bx, 1*02h
  dec cx
  cmp cx, 0
  jne @B

  mov cx, Screen_kWidth*Screen_kHeight
@@:
  mov [bx], cx

  add bx, 1*02h ; next
  dec cx
  cmp cx, Screen_kWidth*Screen_kHeight - Screen_kWidth
  jne @B

  mov cx, Screen_kWidth*Screen_kHeight - Screen_kWidth
@@:
  mov [bx], cx

  add bx, 1*02h ; next
  sub cx, Screen_kWidth
  cmp cx, 0
  jne @B

  mov cx, Screen_kWidth*Screen_kHeight - 1
@@:
  mov [bx], cx

  add bx, 1*02h
  sub cx, Screen_kWidth
  cmp cx, Screen_kWidth - 1
  jne @B

  pop ds
  pop dx
  pop cx
  pop bx
  mov sp, bp
  pop bp
  ret
Level_init endp

Level_initHorizontal proc near ; IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push cx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  lea bx, [Game_data + TAG_kLevel][TAG_kBorderSize]
  mov cx, 100*Screen_kWidth - 2*10

@@:
  mov [bx], cx

  add bx, 1*02h ; next
  dec cx
  cmp cx, 99*Screen_kWidth + 2*10 + 1
  jne @B

  pop ds
  pop cx
  pop bx
  mov sp, bp
  pop bp
  ret
Level_initHorizontal endp

Level_initVertical proc near ; IO () {{{1
  push bp
  mov  bp, sp
  push bx
  push cx
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  lea bx, [Game_data + TAG_kLevel][TAG_kBorderSize + 02h*Screen_kWidth]
  mov cx, Screen_kHeight*Screen_kWidth + Screen_kWidth/2 - 21*Screen_kWidth

@@:
  mov [bx], cx

  add bx, 1*02h ; next
  sub cx, Screen_kWidth
  cmp cx, 20*Screen_kWidth + Screen_kWidth/2
  jne @B

  pop ds
  pop cx
  pop bx
  mov sp, bp
  pop bp
  ret
Level_initVertical endp

Level_render proc near ; {{{1
  push bp
  mov  bp, sp
  push bx
  push dx
  push si
  push ds

  ; Data Segment
  mov ax, @fardata
  mov ds, ax

  mov dx, Screen_kWhite
  lea si, [Game_data + TAG_kTails - 1*02h] ; end
  lea bx, [Game_data + TAG_kLevel] ; begin

  ; sub si, 1*02h ; skip Tails[0]

@@:
  push dx
  mov  ax, [si]
  push ax
  call Screen_setPixel ; (color, position)

  sub si, 1*02h ; next

  cmp si, bx
  jae @B ; unless(bx == END)

  pop ds
  pop si
  pop dx
  pop bx
  mov sp, bp
  pop bp
  ret
Level_render endp

; }}}1

END
