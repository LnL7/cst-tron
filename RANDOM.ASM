.MODEL small
.STACK 1024

INCLUDE random.inc

.DATA ; {{{1

__seed       dw Random_SEED ; initial random seed
__r250Index  dw 0           ; R250 index
__r250Buffer dw 250 dup(?)  ; buffer for R250 random generator

; }}}1

.CODE

Random_Init proc far ; {{{1
  ; Initializes the R250 random number generator
  ; See PDF file
  push bp
  mov  bp, sp

  push cx

  ; Clear index
  mov [__r250Index], 0

  mov  ax, offset __r250Buffer
  push ax
  mov  cx, 250
  call Random_fillBuffer ; 250 (offset __r250Buffer)

  mov  ax, offset __r250Buffer
  add  ax, 6
  push ax
  mov  ax, 0ffffh
  push ax
  mov  ax, 08000h
  push ax
  mov  cx, 16
  call Random_fixBuffer ; 16 (offset __r250Buffer + 6, 0ffffh, 08000h)

  pop cx

  mov sp, bp
  pop bp
  ret
Random_Init endp

Random_Word proc far ; {{{1
  ; Use R250 random number generator and return a value in AX
  ; See PDF file
  push bp
  mov  bp, sp

  push bx
  push di

  mov di, offset __r250Buffer

  call Random_nextIndex
  mov bx, ax

  mov ax, [bx][di] ; ax = __r250Buffer[bx]
  mov bx, [__r250Index]
  shl bx, 1
  add di, bx
  xor [di], ax ; __r250Buffer[__r250Index] ^= ax (new rand)

  ; Generate new index
  mov ax, bx
  inc ax
  mov bl, 250
  div bl
  mov al, ah
  xor ah, ah
  mov [__r250Index], ax

  mov ax, [di] ; return new rand

  pop di
  pop bx

  mov sp, bp
  pop bp
  ret
Random_Word endp

; }}}1

Random_fillBuffer proc near ; CX (OFFSET) {{{1
  ; Fill the __r250Buffer with simple rand values

  push bp
  mov  bp, sp

  push di

  mov di, [bp + 4][0] ; OFFSET

  call Random_linear ; get a simple rand
  mov [di], ax

  ; Update arguments for recursive call
  add di, 2
  mov [bp + 4][0], di ; OFFSET + 2

  pop di

  mov sp, bp
  pop bp
  loop Random_fillBuffer ; CX-- (OFFSET + 2)
  ret 2
Random_fillBuffer endp

Random_fixBuffer proc near ; CX (OFFSET, AND, OR) {{{1
  ; Correct values
  push bp
  mov  bp, sp

  push dx
  push di

  mov di, [bp + 4][0] ; OFFSET
  mov ax, [bp + 4][2] ; AND
  mov bx, [bp + 4][4] ; OR

  and [di], ax
  or  [di], bx

  ; Update arguments for recursive call
  add di, 22
  mov [bp + 4][0], di ; OFFSET + 22
  shr ax, 1
  mov [bp + 4][2], ax ; AND >> 1
  shr bx, 1
  mov [bp + 4][4], bx ; OR >> 1

  pop di
  pop bx

  mov sp, bp
  pop bp
  loop Random_fixBuffer ; CX-- (OFFSET + 22, AND >> 1, OR >> 1)
  ret 6
Random_fixBuffer endp

Random_linear proc near ; {{{1
  ; Generate a random number in AX (16 bits).
  push bp
  mov  bp, sp

  push cx

  ; Lehmer linear congruential random number generator
  ; z = (a*z+b) mod m = (31*z+13)%19683
  ; Rules for a, b, m by D. Knuth:
  ; 1. b and m must be relatively prime
  ; 2. a-1 must be divisible without remainder by all prime factors of m
  ;    (19683 = 3^9), (31-1)%3=0
  ; 3. if m is divisible by 4, a-1 must also be divisible by 4,
  ;    19683%4 != 0, ok
  ; 4. if conditions 1 to 3 met, period of {z1, z2, z3,...} is
  ;    m-1 = 19682 (Donald says)

  mov ax, [__seed]

  ; First generated number
  push ax
  call Random_generate ; ([__seed])

  ; Keep the number in cx and do some mixing
  mov  cx, ax
  xchg cl, ch ; exchange low and right registers
  rol  cx, 1  ; logical right shift

  ; Second generated number
  push ax
  call Random_generate ; (First)

  mov [__seed], ax ; store for next rand call
  xor ax, cx       ; combine the two rands into a 16-bit number

  pop cx

  mov sp, bp
  pop bp
  ret
Random_linear endp

Random_generate proc near ; (SEED) {{{1
  push bp
  mov  bp, sp

  push bx
  push dx

  mov ax, [bp + 4] ; SEED

  ; Prepare generated number (range [0;19683[)
  mov bx, 31 ; 31D
  mul bx     ; 31 * SEED

  ; Result dx:ax, higher value in dx, lower value in ax
  add ax, 13
  adc dx, 0
  mov bx, 19683
  div bx ; div by 19683

  ; Result ax:dx, quotient in ax, remainder in dx
  mov ax, dx

  pop dx
  pop bx

  mov sp, bp
  pop bp
  ret 2
Random_generate endp

Random_nextIndex proc near ; {{{1
  ; Calculate next index
  push bp
  mov  bp, sp

  mov ax, [__r250Index]

  cmp ax, 147
  jl  @else

  sub ax, 147
  jmp @endif

@else:
  add ax, 103

@endif:
  shl ax, 1

  mov sp, bp
  pop bp
  ret
Random_nextIndex endp

; }}}1

END
