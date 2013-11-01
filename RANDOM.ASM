.MODEL small
.STACK 1024

INCLUDE random.inc

.DATA ; {{{1

__seed    dw Random_SEED ; initial random seed
__r250Idx dw 0           ; R250 index
__r250Buf dw 250 dup(?)  ; buffer for R250 random generator

.CODE ; {{{1

Random_linear proc near ; {{{2
  ; Generate a random number in AX (16 bits).
  push bp     ; save dynamic link
  mov  bp, sp ; update bp

  push bx ; save used registers
  push cx
  push dx

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

  ; Prepare first generated number (range [0;19683[)
  mov bx, 31 ; 31D
  mul bx     ; 31 * z

  ; Result dx:ax, higher value in dx, lower value in ax
  add ax, 13
  adc dx, 0
  mov bx, 19683
  div bx ; div by 19683

  ; Result ax:dx, quotient in ax, remainder in dx
  mov ax, dx

  ; Keep the number in cx and do some mixing
  mov  cx, ax
  xchg cl, ch
  rol  cx, 1

  ; Prepare second generated number (range [0;19683[)
  mov bx, 31 ; 31D
  mul bx     ; 31 * z

  ; Result dx:ax, higher value in dx, lower value in ax
  add ax, 13
  adc dx, 0
  mov bx, 19683
  div bx ; div by 19683

  ; Result ax:dx, quotient in ax, remainder in dx
  mov ax, dx

  mov [__seed], ax ; store for next rand call
  xor ax, cx       ; combine the two rands into a 16-bit number

  pop dx
  pop cx
  pop bx

  mov sp, bp
  pop bp
  ret
Random_linear endp

; }}}2

Random_init proc far ; {{{1
  ; Initializes the R250 random number generator
  ; See PDF file
  push bp     ; save dynamic link
  mov  bp, sp ; update bp

  push bx
  push cx
  push di

  ; Clear index
  mov [__r250Idx], 0

  ; Fill the __r250Buf with simple rand values
  mov cx, 250
  mov di, offset __r250Buf

@@:
  call Random_linear ; get a simple rand
  mov [di], ax
  add di, 2
  loop @B

  ; Correct values
  mov cx, 16
  mov di, offset __r250Buf
  add di, 6
  mov ax, 0ffffh
  mov bx, 08000h

@@:
  and  [di], ax
  or   [di], bx
  shr  ax, 1
  shr  bx, 1
  add  di, 22
  loop @B

  ; And we are done
  pop di
  pop cx
  pop bx

  mov sp, bp
  pop bp
  ret
Random_init endp

Random_dw proc far ; {{{1
  ; Use R250 random number generator and return a value in AX
  ; See PDF file
  push bp ; save dynamic link
  mov  bp, sp ; update bp

  push bx
  push di

  mov di, offset __r250Buf

  ; Calculate next index
  mov ax, [__r250Idx]
  cmp ax, 147
  jl  @else
  sub ax, 147
  jmp @endif
@else:
  add ax, 103
@endif:
  mov bx, ax
  shl bx, 1

  mov ax, [bx][di] ; ax = __r250Buf[bx]
  mov bx, [__r250Idx]
  shl bx, 1
  add di, bx
  xor [di], ax ; __r250Buf[__r250Idx] ^= ax (new rand)

  ; Generate new index
  mov ax, bx
  inc ax
  mov bl, 250
  div bl
  mov al, ah
  xor ah, ah
  mov [__r250Idx], ax

  mov ax, [di] ; return new rand

  pop di
  pop bx

  mov sp, bp
  pop bp
  ret
Random_dw endp

; }}}1

END
