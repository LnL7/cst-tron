.MODEL large

INCLUDE video.inc

; VGA DAC ports
PORT_VGA_DAC_READ  equ 03c7h ; write
PORT_VGA_DAC_WRITE equ 03c8h ; write
PORT_VGA_DAC_DATA  equ 03c9h ; read/write

FADESTEPS  equ 32
FADESTEPSB equ 5  ; 5 bits

.DATA? ; {{{1

; The following buffer is used to keep 16-bit fixed point palette values during fading
__fader  dw 2 * 768 dup(?)
__stepsC dw ?

.CODE ; {{{1

Video_setMode proc far ; (REQUESTED_MODE) {{{1
  ; Set the video mode and returns the previous mode in AL.
  push bp     ; save dynamic link
  mov  bp, sp ; update bp
  push bx

  mov ah, 0fh     ; get current video mode in al
  int 10h

  mov bx, ax

  mov ax, [bp + 6][0] ; get requested mode
  xor ah, ah          ; function 0
  int 10h

  mov   ax, bx

  pop  bx

  mov  sp, bp
  pop  bp
  retf 2
Video_setMode endp

Video_wait proc far ; {{{1
  ; Waits for the next start of a vertical blank (VB) period by the video card
  push bp
  mov  bp, sp

  push dx
  mov  dx, 03dah ; VGA status port

@@:          ; if already in VB, then wait until VB finishes
  in  al, dx ; read status
  and al, 8  ; test bit 3
  jnz @B     ; busy wait if in VB

@@:          ; wait until begin of a new VB
  in  al, dx ; read status
  and al, 8  ; test bit 3
  jz  @B     ; busy wait if NOT in VB

  pop dx

  mov sp, bp
  pop bp
  ret
Video_wait endp

Palette_set proc far ; (DATA_SEGMENT, OFFSET) {{{1
  ; Immediately set a new VGA palette
  ; Arguments are SEGMENT and OFFSET of destination palette (points to 768 bytes)
  push bp
  mov  bp, sp

  push cx
  push dx
  push si
  push ds

  mov ax, [bp + 6][2] ; get data segment argument
  mov ds, ax          ; and set it as DS

  mov ax, [bp + 6][0] ; get the offset argument
  mov si, ax          ; and set it as source index

  cli ; disable interrupts

  mov dx, PORT_VGA_DAC_WRITE
  xor ax, ax
  out dx, al                ; start at color 0
  mov dx, PORT_VGA_DAC_DATA
  mov cx, 768               ; we need to write 768 channel color bytes

@@:
  lodsb      ; load next channel color byte into al
  out dx, al ; write the byte in al to DAC DATA port
  dec cx     ; decrement loop counter
  jnz @B     ; loop if more todo (@B is the first @@ label before this point)

  sti ; enable interrupts

  pop ds
  pop si
  pop dx
  pop cx

  mov  sp, bp
  pop  bp
  retf 4
Palette_set endp

Palette_initFade proc far ; {{{1
  ; Call this function to start a palette fade,
  ; starting with the currently active palette
  ; (uses __fader and paletteSteps)
  ; (defaults to 32 steps)
  ; Arguments are SEGMENT and OFFSET of destination palette (points to 768 bytes)
  push bp
  mov  bp, sp

  push bx
  push cx
  push dx
  push si
  push di
  push ds
  push es

  cld

  ; Setup segment registers
  mov ax, [bp + 6][2] ; get data segment
  mov ds, ax

  ; One palette color uses 6 bits, these are shifted 9 left to get
  ; a 15 bit fixed point number with an extra sign bit (= 16 bits total)

  ; __fader contains 16 bit palette RGB values and steps used for
  ; fading (interleaved)

  ; Get the active palette into __fader (shifted left 9 bits)
  ; and create the array of delta (step values) in 16-bit FP arithm.
  mov ax, seg __fader
  mov es, ax
  mov ax, FADESTEPS           ; initialize steps
  mov es:[__stepsC], ax
  mov si, [bp + 6][0]         ; get the offset of target
  mov di, offset __fader
  xor ch, ch                  ; 256 RGB colors (uses wrapping)

  cli ; disable interrupts

  xor ax, ax
  mov dx, PORT_VGA_DAC_READ
  out dx, al                ; set color index 0
  mov dx, PORT_VGA_DAC_DATA

@red:
  lodsb              ; al := ds:[si] (load dest value in al)
  mov bl, al         ; put it in bl
  in  al, dx         ; read active color in al
  sub bl, al         ; get difference (can be negative)
  mov cl, 9          ; prepare for 9 bit shift left
  sal bx, cl         ; make 16 bit fixed point
  shl ax, cl         ; make 16 bit fixed point
  mov cl, FADESTEPSB ; prepare for step division
  sar bx, cl         ; steps (div by number of steps)
  stosw              ; store active color (16 bit)
  mov ax, bx
  stosw              ; store step value (16 bit)

@green:
  lodsb              ; al := ds:[si] (load dest value in al)
  mov bl, al         ; put it in bl
  in  al, dx         ; read active color in al
  sub bl, al         ; get difference (can be negative)
  mov cl, 9          ; prepare for 9 bit shift left
  sal bx, cl         ; make 16 bit fixed point
  shl ax, cl         ; make 16 bit fixed point
  mov cl, FADESTEPSB ; prepare for step division
  sar bx, cl         ; steps (div by number of steps)
  stosw              ; store active color (16 bit)
  mov ax, bx
  stosw              ; store step value (16 bit)

@blue:
  lodsb              ; al := ds:[si] (load dest value in al)
  mov bl, al         ; put it in bl
  in  al, dx         ; read active color in al
  sub bl, al         ; get difference (can be negative)
  mov cl, 9          ; prepare for 9 bit shift left
  sal bx, cl         ; make 16 bit fixed point
  shl ax, cl         ; make 16 bit fixed point
  mov cl, FADESTEPSB ; prepare for step division
  sar bx, cl         ; steps (div by number of steps)
  stosw              ; store active color (16 bit)
  mov ax, bx
  stosw              ; store step value (16 bit)

  inc ch
  jnz   @red

  sti ; enable interrupts

  pop es
  pop ds
  pop di
  pop si
  pop dx
  pop cx
  pop bx

  mov  sp, bp
  pop  bp
  retf 4
Palette_initFade endp

Palette_nextFade proc far ; {{{1
  ; Perform the next palette fading step
  ; More steps are needed when return value (AX) is non-zero
  push bp
  mov  bp, sp

  push bx
  push cx
  push dx
  push di
  push ds
  push es

  cld

  mov ax, seg __fader
  mov ds, ax
  mov es, ax

  cli ; disable interrupts

  ; Add deltas and set new palette
  xor ax, ax
  mov dx, PORT_VGA_DAC_WRITE
  out dx, al                  ; start at index 0
  mov si, offset __fader
  mov di, si
  xor ch, ch                  ; 256 color indexes
  mov dx, PORT_VGA_DAC_DATA
  mov cl, 9

@red:
  lodsw       ; load active color in ax
  mov bx, ax
  lodsw       ; load step in ax
  add ax, bx  ; add active color and step
  stosw       ; store new active color
  add di, 2
  add ax, 256 ; rounding (9 bits fraction, so add 0.5 in decimals = 2^8 = 256)
  shr ax, cl  ; convert 16 bit FP to 6 bit int
  out dx, al  ; write active color

@green:
  lodsw       ; load active color in ax
  mov bx, ax
  lodsw       ; load step in ax
  add ax, bx  ; add active color and step
  stosw       ; store new active color
  add di, 2
  add ax, 256 ; rounding
  shr ax, cl  ; convert 16 bit FP to 6 bit int
  out dx, al  ; write active color

@blue:
  lodsw       ; load active color in ax
  mov bx, ax
  lodsw       ; load step in ax
  add ax, bx  ; add active color and step
  stosw       ; store new active color
  add di, 2
  add ax, 256 ; rounding
  shr ax, cl  ; convert 16 bit FP to 6 bit int
  out dx, al  ; write active color
  inc ch
  jnz @red

  sti ; enable interrupts

  ; One step less todo
  mov ax, [__stepsC]
  dec ax
  mov [__stepsC], ax

  pop es
  pop ds
  pop di
  pop dx
  pop cx
  pop bx

  mov  sp, bp
  pop  bp
  retf
Palette_nextFade endp

END
