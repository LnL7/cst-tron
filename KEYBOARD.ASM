.MODEL large

INCLUDE keyboard.inc

.DATA? ; {{{1

PUBLIC Keyboard_State, Keyboard_ScanCode, Keyboard_ActiveKeys

__originalHandler   dw 2 dup(?)   ; SEG/OFF of original keyboard handler
Keyboard_State      db 128 dup(?) ; state for all 128 keys
Keyboard_ScanCode   db ?          ; scan code of last pressed key
Keyboard_ActiveKeys db ?          ; number of actively pressed keys

.CODE ; {{{1

Keyboard_installHandler proc far ; {{{1
  ; Installs the custom keyboard handler
  push bp
  mov  bp, sp

  push bx
  push cx
  push dx
  push di
  push ds
  push es

  mov ax, seg __originalHandler
  mov ds, ax
  mov es, ax

  ; Clear state buffer and the two state bytes
  cld
  mov cx, (128 / 2) + 1
  mov di, offset Keyboard_State
  xor ax, ax
  rep stosw

  ; Store current handler
  mov ax, 3509h ; get current interrupt handler 09h
  int 21h       ; in ES:BX

  mov ax, es
  mov [__originalHandler][0], ax ; store SEG
  mov [__originalHandler][2], bx ; store OFF

  ; Set new handler
  mov ax, seg Keyboard_handler    ; new SEG
  mov dx, offset Keyboard_handler ; new OFF
  mov ds, ax

  mov ax, 2509h ; set custom interrupt handler 09h
  int 21h       ; uses DS:DX

  pop es
  pop ds
  pop di
  pop dx
  pop cx
  pop bx

  mov sp, bp
  pop bp
  ret
Keyboard_installHandler endp

Keyboard_uninstallHandler proc far ; {{{1
  ; Restores the original keyboard handler
  push bp
  mov  bp, sp

  push dx
  push ds

  mov ax, seg __originalHandler
  mov ds, ax

  mov ax, [__originalHandler][0] ; retrieve SEG
  mov dx, [__originalHandler][2] ; retrieve OFF
  mov ds, ax

  mov ax, 2509h ; set original interrupt handler 09h
  int 21h       ; uses DS:DX

  pop ds
  pop dx

  mov sp, bp
  pop bp
  ret
Keyboard_uninstallHandler endp

Keyboard_handler proc far ; {{{1
  ; Keyboard handler (Interrupt function, DO NOT CALL MANUALLY!)
  __KEY_BUFFER  equ 60h ; the port of the keyboard buffer
  __KEY_CONTROL equ 61h ; the port of the keyboard controller
  __PIC_PORT    equ 20h ; the port of the peripheral

  push ax
  push bx
  push si
  push ds

  ; Setup DS for access to data variables
  mov ax, seg __originalHandler
  mov ds, ax

  ; Handle the keyboard input
  sti                     ; re-enable CPU interrupts
  in  al, __KEY_BUFFER    ; get the key that was pressed from the keyboard
  mov bl, al              ; store scan code for later use
  mov [Keyboard_ScanCode], al ; store the key in global variable
  in  al, __KEY_CONTROL   ; set the control register to reflect key was read
  or  al, 82h             ; set the proper bits to reset the keyboard flip flop
  out __KEY_CONTROL, al   ; send the new data back to the control register
  and al, 7fh             ; mask off high bit
  out __KEY_CONTROL, al   ; complete the reset
  mov al, 20h             ; reset command
  out __PIC_PORT, al      ; tell PIC to re-enable interrupts

  ; Process the retrieved scan code and update Keyboard_State and Keyboard_ActiveKeys
  ; scan codes of 128 or larger are key release codes
  mov al, bl                    ; put scan code in al
  shl ax, 1                     ; bit 7 is now bit 0 in ah
  not ah
  and ah, 1                     ; ah now contains 0 if key released, and 1 if key pressed
  shr al, 1                     ; al now contains the actual scan code ([0;127])
  xor bx, bx
  mov bl, al                    ; bl now contains the actual scan code ([0;127])
  lea si, [Keyboard_State][bx] ; load address of key relative to Keyboard_State in bx
  mov al, [si]                  ; load the keyboard state of the scan code in al

  ; al = tracked state (0 or 1) of pressed key (the value in memory)
  ; ah = physical state (0 or 1) of pressed key
  neg al
  add al, ah             ; al contains -1, 0 or +1 (-1 on key release, 0 on no change and +1 on key press)
  add [Keyboard_ActiveKeys], al ; update Keyboard_ActiveKeys counter
  mov al, ah
  mov [si], al           ; update tracked state

  pop ds
  pop si
  pop bx
  pop ax

  iret
Keyboard_handler endp

END
