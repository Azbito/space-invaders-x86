
; [NOTE] Assuming direction flag is clear, StackPointer initialised to 6EF0h, BasePointer = 0

org 7C00h

; DEFINED VARIABLES AFTER SCREEN MEMORY - 320*200 = 64000 or FA00h
sprites         equ 0FA00h  ; Base address of the sprite data
alien1          equ 0FA00h  ; Address of the first alien sprite
alien2          equ 0FA04h  ; Address of the second alien sprite
ship            equ 0FA08h  ; Address of the player's ship sprite
barrierArr      equ 0FA0Ch  ; Address of the barrier array
alienArr        equ 0FA20h  ; Address of the alien array (each alien occupies 32 bits)
playerX         equ 0FA24h  ; Player's X position
shotsArr        equ 0FA25h  ; Address of the shots array (4 Y/X shot values)
alienY          equ 0FA2Dh  ; Alien's Y position
alienX          equ 0FA2Eh  ; Alien's X position
num_aliens      equ 0FA2Fh  ; Number of aliens still alive
direction       equ 0FA30h  ; Number of pixels that aliens move in the X direction
move_timer      equ 0FA31h  ; 2 bytes (use BP) - number of game loops (or timer ticks) to wait before aliens move
change_alien    equ 0FA33h  ; Use alternate sprite true/false


; CONSTANTS ===========================================================================================
SCREEN_WIDTH    equ 320
SCREEN_HEIGHT   equ 200
VIDEO_MEMORY    equ 0A000h
TIMER           equ 046Ch
VGA_MODE        equ 0013h
BARRIERX        equ 22
BARRIERY        equ 85
PLAYERY         equ 93
SPRITEH         equ 4
SPRITEW         equ 8   ; width in bits/data pixels
SPRITEWP        equ 16  ; width in screen pixels

; Define color constants
        ; black = 00h
        ; dark blue = 01h
        ; dark green = 02h
        ; dark cyan = 03h
        ; dark red = 04h
        ; dark magenta = 05h
        ; brown = 06h
        ; light grey = 07h
        ; dark grey = 08h
        ; light blue = 09h
        ; light green = 0Ah
        ; light cyan = 0Bh
        ; light red = 0Ch
        ; light magenta = 0Dh
        ; yellow = 0Eh
        ; white = 0Fh
        ; red = 27h
        
ALIEN_COLOR equ 02h  ; Green
PLAYER_COLOR equ 07h ; Grey
BARRIER_COLOR equ 27h ; Red
PLAYER_SHOT_COLOR equ 0Bh ; Cyan
ALIEN_SHOT_COLOR equ 0Eh ; Yellow

; SETUP ============================
; Set up video mode - VGA mode 13h, 320x200, 256 colours, 8bpp, linear framebuffer at address A0000h
mov ax, VGA_MODE     ; Set AX register to 13h, which represents VGA mode 13h
int 10h              ; Call BIOS interrupt 10h to set video mode

; Set up video memory
push VIDEO_MEMORY
pop es               ; ES -> A0000h

; Move initial sprite data into memory
mov di, sprites
mov si, sprite_bitmaps 
mov cl, 6
rep movsw

lodsd       ; Store 5 barriers in memory for barrierArr
mov cl, 5
rep stosd ; store string dword

; Set initial variables
mov cl, 5
rep movsb

xor ax, ax  ; Shots array - 8 bytes Y/X values
mov cl, 4
rep stosw

mov cl, 7   ; Alien Y/X, # of aliens, direction, move_timer, change_alien
rep movsb

push es
pop ds      ; ES = DS
; GAME LOOP ===========================================================================================

game_loop:
    xor ax, ax           ; Clear screen to black first
    xor di, di           ; Clear DI register, which will be used as the offset for screen memory
    mov cx, SCREEN_WIDTH*SCREEN_HEIGHT     ; Calculate total number of pixels (320x200) and store in CX register
    rep stosb            ; [ES:DI], al cx # of times

    ; ES:DI now points to AFA00H
    
    ; Draw aliens ------------------------------------------------------------------------------------
    mov si, alienArr
    mov bl, ALIEN_COLOR
    mov ax, [si+13] ; AL = alienY, AH = alienX
    cmp byte [si+19], cl ;   change alien. CL = 0 from above
    mov cl, 4           ;   IT DOES NOT AFFECT THE CL VALUE ABOVE
    jg  draw_next_alien_row ;   if greater, use normal sprite
    add di, cx          ;   or else, use alternate sprite
    
    draw_next_alien_row:
        pusha
        mov cl, 8       ; # of aliens to check per row
        .check_next_alien:
            pusha
            dec cx
            bt [si], cx ; bit test - copy bit to carry flag
            jnc .next_alien ; not set, skip
            
            mov si, di  ; SI = alien sprite to draw
            call draw_sprite
            
            .next_alien:
                popa
                add ah, SPRITEW+4
        loop .check_next_alien
           
        popa
        add al, SPRITEH+2
        inc si
    loop draw_next_alien_row
       
    
   
    ; Delay timer - 1 tick delay (1 tick = 18.2/second)
    
    delay_timer: ; global label
    mov ax, [CS:TIMER] ; # of timer ticks since midnight
    inc ax
    .wait: ; private label. In order to access it, you have to JMP delayer_timer.wait c:
        cmp [CS:TIMER], ax
        JL .wait
    

jmp game_loop

game_over:
cli                  ; Clear interrupt flag to disable interrupts
hlt                  ; Halt the CPU

; Draw a sprite to the screen
; Input parameters:
; SI = address of sprite to draw
; AL = Y value of sprite
; AH = X value of sprite
; BL = color

draw_sprite:
    call get_screen_position    ; get X/Y position in DI to draw at
    mov cl, SPRITEH
    .next_line:
        push cx
        lodsb                   ; AL = next byte of sprite data
        xchg ax, dx             ; save off sprite data
        mov cl, SPRITEW         ; # of pixels to draw in sprite
        .next_pixel:
            xor ax, ax          ; if drawing blank/black pixel
            dec cx
            bt dx, cx           ; Is bit in sprite set? Copy to carry
            cmovc ax, bx        ; bit is set true, move BX into AX (BL = color)
            mov ah, al         ; Copy color to fill out AX
            mov [di+SCREEN_WIDTH], ax
            stosw
        jnz .next_pixel
        
        add di, SCREEN_WIDTH*2-SPRITEWP
        pop cx
    loop .next_line
    
    ret
    
; GET X/Y screen position in DI
; Input parameters:
; AL = Y value of sprite
; AH = X value of sprite
; Clobbers:
; DX
; DI

get_screen_position:
    mov dx, ax  ; save Y/X values
    cbw         ; covert byte to word - sign extend AL into AH, AH = 0 if AL < 128
    imul di, ax, SCREEN_WIDTH*2  ; DI = Y value
    mov al, dh  ; AX = X value
    shl ax, 1   ; AX * 2
    add di, ax  ; DI = Y value + X value or X/Y position
    ret

; CODE SEGMENT DATA =============================================================================

sprite_bitmaps:
    db 10011001b    ; Alien 1 bitmap
    db 01011010b
    db 00111100b
    db 01000010b
    
    db 00011000b    ; Alien 2 bitmap
    db 01011010b
    db 10111101b
    db 00100100b
    
    db 00011000b    ; Player ship bitmap
    db 00111100b
    db 00100100b
    db 01100110b

    db 00011000b    ; Barrier bitmap
    db 00111100b
    db 01100110b
    db 11000011b
    
    dw 0FFFFh   ; Alien array
    dw 0FFFFh
    db 70       ; player X
    ; times 6 db 0 ; Shots array
    dw 230Ah    ; alienY & alien X | 10 = y, 32 = x
    db 20h      ; # of aliens - 32
    
    db 0FBh     ; Direction -5
    dw 18       ; move timer
    db 1        ; change alien - toggle between 0/1
    
    
    
times 510-($-$$) db 0  ; Fill the remaining bytes in the boot sector with zeros

.signature dw 0AA55h            ; Boot signature indicating a valid boot sector
