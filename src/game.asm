; ===============================================================
;          ***      PAC-MAN for the Commodore 128    *****
;        *** ***        (ATARI 2600 version)       *** * ***
;        ***                                       *********
;         *****        Scott Hutter / xlar54       * * * * *
; ===============================================================

.include "vic.asm"
.include "sid.asm"

SCREEN_RAM				= $0400
SPRITE_0_POINTER    	= $07F8 		; Last 8 Bytes of Screen RAM
SPRITE_1_POINTER		= $07F9
SPRITE_2_POINTER		= $07FA
SPRITE_3_POINTER		= $07FB
SPRITE_4_POINTER		= $07FC
SPRITE_5_POINTER		= $07FD
SPRITE_6_POINTER		= $07FE
SPRITE_7_POINTER		= $07FF

SPRITE_0_DATA       	= $0e00
SPRITE_1_DATA       	= $0e40
SPRITE_2_DATA       	= $0e80
SPRITE_3_DATA       	= $0ec0
SPRITE_4_DATA       	= $0f00
SPRITE_5_DATA       	= $0f40
SPRITE_6_DATA       	= $0f80
SPRITE_7_DATA       	= $0fc0

BASIC_IRQ_FLAG			= $12FD

COLOR_RAM				= $D800
IO_PORT_DATA_REG_A		= $DC00
CIA2_PORT_REG_A			= $DD00
MMUCR               	= $FF00     ; Configuration register

PP1_OFFSET = $51
PP2_OFFSET = $75
PP3_OFFSET = $2a9
PP4_OFFSET = $2cd

DIR_STOPPED	= $00
DIR_UP		= $01
DIR_DOWN	= $02
DIR_RIGHT	= $03
DIR_LEFT	= $04

CHARS_SRC	= $60
CHARS_DEST 	= $62

SPRITE_0_FRAME_COUNT	= $03

; temp ZP Variables
sprite_num    	= $C0
sprite_x      	= $C1
sprite_y      	= $C2
sprite_msb2    	= $C3
screen_addr_lo	= $C4
screen_addr_hi	= $C5
temp          	= $C6
column			= $c7
row				= $c8

; ====================================================================
; BASIC LOADER
; ====================================================================
; 10 SYS 7182
*=$1c01
	.byte $1c, $1c, $0A, $00, $9E, $20, $37, $31, $38, $32
    .byte $00, $00, $00

; ====================================================================
; main startup
; ====================================================================
main:

	lda #$3e
	sta MMUCR				; All RAM1 with IO block

	jsr screen_init
	jsr chars_init
	jsr sprite_init
;fvr: jmp fvr
	jsr irq_init

main_loop:
	lda VIC_SPR_FG_COL_REG
	sta collision_flg

	; check joystick
	lda IO_PORT_DATA_REG_A
	cmp joy_cache
	beq main_collision_check
	sta joy_cache
	jsr joystick_handler

main_collision_check:
	lda collision_flg
	and #$01
	cmp #$01
    beq collide
	;inc VIC_BACKGROUND_COLOR

main_loop_end:
	jmp main_loop

collide:
	;inc VIC_BORDER_COLOR
	;jsr play_chomp_sound
	jmp main_loop
	
; ====================================================================
; common data
; ====================================================================
delay_game:
	.byte $ff
delay_pp:
	.byte $ff
delay_plyr_anim:
	.byte $ff
delay_plyr_move:
	.byte $ff
sprite_0_fr_count:
	.byte SPRITE_0_FRAME_COUNT
collision_flg:
	.byte $00
player_x:
	.byte $00, $b0
player_y:
	.byte $b0
player_direction:
	.byte DIR_STOPPED
player_prev_direction:
	.byte DIR_RIGHT
ghost1_direction:
	.byte $00
ghost2_direction:
	.byte $00
ghost3_direction:
	.byte $00
ghost4_direction:
	.byte $00
joy_cache:
	.byte $00
prev_joy_cache:
	.byte $00

; ====================================================================
; initialize the screen
; ====================================================================
screen_init:
	; init screen
	; =======================================================================
	; take control from BASIC for direct access to the VIC-II registers
	; otherwise we would need to talk to the shadow registers like $0a2c
	
	; prevents BASIC's overwriting VIC registers multicolor and bitmap registers
	lda #$ff
	sta $d8			
	
	; prevents BASIC's overwriting VIC registers for SPRITE, MOVSPR, etc commands
	lda #$01
	sta BASIC_IRQ_FLAG		

	; tell vic-ii which 16k bank we want to use
	; we are using the default bank 0 ($0000-$3fff)
	lda CIA2_PORT_REG_A
	and #%11111100
	ora #%00000011	; last two bits determine bank

	; disable SHIFT-Commodore
	;lda #$80
	;sta $f7

	; set screen memory ($0400) and charset bitmap offset ($1000)
	lda #$1c
	sta VIC_SCREEN_BASE_REG

	; set border color
	lda #BLACK
	sta VIC_BORDER_COLOR
	
	; set background color
	lda #BLUE
	sta VIC_BACKGROUND_COLOR

	; draw screen
	lda #<scr_data
	ldx #>scr_data
	sta $64
	stx $65

	lda #<SCREEN_RAM
	ldx #>SCREEN_RAM
	sta $66
	stx $67

	lda #<scr_color
	ldx #>scr_color
	sta $62
	stx $63
	
	lda #<COLOR_RAM
	ldx #>COLOR_RAM
	sta $60
	stx $61

	ldx #$00
screen_l0:
	ldy #$00

-	lda ($64),y
	sta ($66),y
	lda ($62),y
	sta ($60),y
	iny
	bne -

	inc $67
	inc $61
	inc $65
	inc $63
	
	inx
	cpx #$04
	bne screen_l0
	rts

; ====================================================================
; initialize the irq vector
; ====================================================================
irq_init:
	sei
	lda #>irq_handler
	sta $315
	lda #<irq_handler
	sta $314
	cli
	rts

; ====================================================================
; initialize the sprites
; ====================================================================
sprite_init:

  	lda #56					; use default spr 0 pointer location (56x64=3584=$0e00)
  	sta SPRITE_0_POINTER
	lda #$01              	; enable...Sprite 0 => %0000 0001 (all sprites off except Sprite 0)
  	sta VIC_SPRITE_ENBL
	lda #YELLOW				; set sprite 0 foreground color
  	sta SPRITE_0_COLOR
 
	; Copy sprite data to sprite pointer, frame 1
	ldx #$00
-	lda pacman_r_fr1, x
	sta SPRITE_0_DATA,x
	inx
	cpx #$3F
	bne -

	; Copy sprite data to sprite pointer, frame 2
	ldx #$00
-	lda pacman_r_fr2, x
	sta SPRITE_1_DATA,x
	inx
	cpx #$3F
	bne -

	; Copy sprite data to sprite pointer, frame 3
	ldx #$00
-	lda pacman_r_fr3, x
	sta SPRITE_2_DATA,x
	inx
	cpx #$3F
	bne -

	;starting sprite location
	lda player_x
	sta SPRITE_XMSB
	lda player_x+1
	sta SPRITE_0_X_POSITION
	lda player_y
	sta SPRITE_0_Y_POSITION
	
	rts

; ====================================================================
; initialise the custom characters
; ====================================================================
chars_init:

	sei					; best to disable interrupts
	
	; setup the screen memory and char data offsets
	lda #$1c
	sta VIC_SCREEN_BASE_REG

	; prevent overwriting VIC multicolor and bitmap registers during BASIC IRQ
	lda #$ff
	sta $d8			
	
	; prevents overwriting VIC registers for SPRITE, MOVSPR, etc commands during BASIC IRQ
	lda #$01
	sta BASIC_IRQ_FLAG

	; setup the copy source and dest memory locations
	lda #>$3000     		; move chars to $3000
	sta CHARS_DEST+1
	lda #>char_data     	; from $d800 for rom (or start of cust char set) (lower-case)
	sta CHARS_SRC+1
	ldy #<char_data
	sty CHARS_SRC
	ldy #$00 
	sty CHARS_DEST

	; copy chars from rom to ram
	ldx #$10        	; copy 2k of data.
- 	lda (CHARS_SRC),y	; copy byte.
	sta (CHARS_DEST),y
	iny
	bne -           	; continue until .Y = 0.
	inc CHARS_SRC+1     ; increase source & dest by 256
	inc CHARS_DEST+1
	dex             	; decrease .X count.
	bne -           	; if non-zero then continue copying, else

	; finalize
	lda $01				; turn bit 2 of $01 on
	ora #$04			; which tells system to use
	sta $01         	; VIC ram-based character set instead of rom

	cli
	rts             	; and return.



; ====================================================================
; IRQ interrupt service routine
; ====================================================================
irq_handler:

	dec delay_pp
	dec delay_plyr_anim
	dec delay_plyr_move

irq_pp_anim_check:
	; check if its time to do power pellet animation
	lda delay_pp
	cmp #$f0
	bpl irq_plyr_anim_check
	lda #$ff
	sta delay_pp

    ; flash the power pellets if they exist
irq_pp_anim:
	lda COLOR_RAM + PP1_OFFSET
	cmp #BROWN
	beq +
	lda #BROWN
	jmp irq_pp_flash
+	lda #YELLOW
irq_pp_flash:
	sta COLOR_RAM + PP1_OFFSET
	sta COLOR_RAM + PP1_OFFSET + 1
	sta COLOR_RAM + PP1_OFFSET + 40
	sta COLOR_RAM + PP1_OFFSET + 41
	sta COLOR_RAM + $75
	sta COLOR_RAM + $75 + 1
	sta COLOR_RAM + $75 + 40
	sta COLOR_RAM + $75 + 41
	sta COLOR_RAM + $2a9
	sta COLOR_RAM + $2a9 + 1
	sta COLOR_RAM + $2a9 + 40
	sta COLOR_RAM + $2a9 + 41
	sta COLOR_RAM + $2cd
	sta COLOR_RAM + $2cd + 1
	sta COLOR_RAM + $2cd + 40
	sta COLOR_RAM + $2cd + 41
irq_plyr_anim_check:
	; time to do player anim?
	lda delay_plyr_anim
	cmp #$f5
	bpl irq_player_move_check
	lda #$ff
	sta delay_plyr_anim

irq_player_anim:
	; do player anim.  change sprite pointer to next frame
	dec sprite_0_fr_count
	bne +

	lda #56						; reset to first anim frame
	sta SPRITE_0_POINTER
	ldx #SPRITE_0_FRAME_COUNT
	stx sprite_0_fr_count

	jmp irq_end
+	inc SPRITE_0_POINTER

irq_player_move_check:

	; time to do player movement?
	;lda delay_plyr_move
	;cmp #$ff
	;bpl irq_end
	;lda #$ff
	;sta delay_plyr_move

	; move the sprite
	lda player_direction
	cmp #DIR_UP
	bne +
	jsr player_move_up
	jmp irq_end

+	lda player_direction
	cmp #DIR_DOWN
	bne +
	jsr player_move_down
	jmp irq_end

+	lda player_direction
	cmp #DIR_RIGHT
	bne +
	jsr player_move_right
	jmp irq_end

+	lda player_direction
	cmp #DIR_LEFT
	bne irq_end
	jsr player_move_left

irq_end
	jmp $fa65

; ====================================================================
joystick_handler:
;
; This routine is called when a direction is changed by the player
; it will set the player_direction to the new direction value which
; is used in the irq routine for movement and if left or right, will 
; switch to the appropriate face / animation
; ====================================================================

	lda joy_cache
	and #$08
	bne +

	; move right
	lda #DIR_RIGHT
	sta player_direction
	jsr select_player_r_anim
	jmp joystick_end

+	lda joy_cache
	and #$04
	bne +

	; move left
	lda #DIR_LEFT
	sta player_direction
	jsr select_player_l_anim
	jmp joystick_end

+	lda joy_cache
	and #$01
	bne +

	; move up
	lda #DIR_UP
	sta player_direction
	jmp joystick_end

+	lda joy_cache
	and #$02
	bne joystick_end

	; move down
	lda #DIR_DOWN
	sta player_direction

joystick_end:
	rts

; ====================================================================
; Sprite movement routines
; These routines are sprite agnostic.  Setting 'sprite_num' to the
; appropriate sprite #, we can move any of the characters including
; the player.  The routines check the bounds around the sprite's
; character screen matrix location for walls
; ====================================================================

; ====================================================================
player_move_right:
; ====================================================================

	; check sprite collision
	lda #$00
	sta sprite_num
	jsr sprite_to_screen_address

	; we are looking two spaces to the right, so add 2 to the address
	lda screen_addr_lo
	clc
	adc #$02
	sta screen_addr_lo
	bcc +
	inc screen_addr_hi

+	ldx #$00
	ldy #$00
pmr_l0:
	lda (screen_addr_lo),y
	cmp #$20
	beq player_move_right_yes
	cmp #$45
	beq player_move_right_yes
	cmp #$52
	beq player_move_right_yes
	cmp #$6C
	beq player_move_right_yes
	cmp #$7c
	beq player_move_right_yes
	cmp #$6f
	beq player_move_right_yes
	cmp #$77
	beq player_move_right_yes
	cmp #$D1
	beq player_move_right_yes
	rts
player_move_right_yes:
	inx
	cpx #$02
	beq player_move_right_yes2
	ldy #$28
	jmp pmr_l0
	rts								; nope - exit

player_move_right_yes2:

	; move the sprite
	clc 
	lda SPRITE_0_X_POSITION
	adc #$01
	bcc +
	ldx #$01
	stx SPRITE_XMSB
+	sta SPRITE_0_X_POSITION
	rts

; ====================================================================
player_move_left:
; ====================================================================

	; check sprite collision
	lda #$00
	sta sprite_num
	jsr sprite_to_screen_address

	; we are looking one spaces to the left, so subtract 1 to the address
	lda screen_addr_lo
	sec
	sbc #$01
	sta screen_addr_lo
	bcs +
	dec screen_addr_hi

+	ldx #$00
	ldy #$00
pml_l0:
	lda (screen_addr_lo),y
	cmp #$20
	beq player_move_left_yes
	cmp #$45
	beq player_move_left_yes
	cmp #$52
	beq player_move_left_yes
	cmp #$6C
	beq player_move_left_yes
	cmp #$7c
	beq player_move_left_yes
	cmp #$6f
	beq player_move_left_yes
	cmp #$77
	beq player_move_left_yes
	cmp #$D1
	beq player_move_left_yes
	rts								; nope - exit
player_move_left_yes:
	inx
	cpx #$02
	beq player_move_left_yes2
	ldy #$28
	jmp pml_l0
	rts	

player_move_left_yes2:

	; move the sprite
	sec 
	lda SPRITE_0_X_POSITION
	sbc #$01
	bcs +
	ldx #$00
	stx SPRITE_XMSB
+	sta SPRITE_0_X_POSITION
	rts

; ====================================================================
player_move_up:
; ====================================================================
	; check sprite collision
	lda #$00
	sta sprite_num
	jsr sprite_to_screen_address

	ldx #$00
	ldy #$00
pmu_l0:
	lda (screen_addr_lo),y	
	cmp #$20
	beq player_move_up_yes
	cmp #$45
	beq player_move_up_yes
	cmp #$52
	beq player_move_up_yes
	cmp #$6C
	beq player_move_up_yes
	cmp #$7c
	beq player_move_up_yes
	rts								; nope - exit
player_move_up_yes:
	inx
	cpx #$02
	beq player_move_up_yes2
	ldy #$01
	jmp pmu_l0
	rts	

player_move_up_yes2:
	; move the sprite
	dec SPRITE_0_Y_POSITION
	rts

; ====================================================================
player_move_down:
; ====================================================================
	; check sprite collision
	lda #$00
	sta sprite_num
	jsr sprite_to_screen_address

	ldx #$00
	ldy #$00
pmd_l0:
	lda (screen_addr_lo),y
	cmp #$20
	beq player_move_down_yes
	cmp #$45
	beq player_move_down_yes
	cmp #$52
	beq player_move_down_yes
	cmp #$6C
	beq player_move_down_yes
	cmp #$7c
	beq player_move_down_yes
	rts								; nope - exit
player_move_down_yes:
	inx
	cpx #$02
	beq player_move_down_yes2
	ldy #$01
	jmp pmd_l0
	rts	

player_move_down_yes2
	; move the sprite
	inc SPRITE_0_Y_POSITION
	rts


; ====================================================================
select_player_r_anim:
; ====================================================================
	; change sprites for animation
	ldx #$00
-	lda pacman_r_fr2, x
	sta SPRITE_1_DATA,x
	inx
	cpx #$3F
	bne -

	ldx #$00
-	lda pacman_r_fr3, x
	sta SPRITE_2_DATA,x
	inx
	cpx #$3F
	bne -
	rts

; ====================================================================
select_player_l_anim:
; ====================================================================
	; change sprites for animation
	ldx #$00
-	lda pacman_l_fr1, x
	sta SPRITE_1_DATA,x
	inx
	cpx #$3F
	bne -

	ldx #$00
-	lda pacman_l_fr2, x
	sta SPRITE_2_DATA,x
	inx
	cpx #$3F
	bne -
	rts

; ====================================================================
sprite_to_screen_address:
; ====================================================================

	; calculate screen column

	lda sprite_num        	; Load sprite number
	asl                   	; Multiply sprite_num by 2 to get the correct index for X and Y
	tax                   	; Transfer the result to X

	lda BASE_SPRITE_X, X  	; Load sprite X-coordinate
	clc
	adc #$06	;  <=============================================adjusting spritex and y based on face direction.  ugh
	sta sprite_x          	; Store sprite X-coordinate
	lda BASE_SPRITE_Y, X  	; Load sprite Y-coordinate
	clc
	adc #$02				; add 2 as a Y adjustment (because sprite bit starts 2 down)
	sta sprite_y          	; Store sprite Y-coordinate

	lda sprite_x			; 16bit subtraction
	sec            
	sbc #$18				; x-offset, visible screen area starts at x=$18
	sta temp

	lda SPRITE_XMSB        	; Load sprite X-coordinate MSB
    ldx sprite_num        	; Load sprite number into X
shift_msb:
    lsr                   	; Shift the bits in the SPRITE_XMSB register to the right
    dex                   	; Decrement X
    bne shift_msb         	; If X is not 0, continue shifting
    and #$01               	; Perform a bitwise AND with 1 to isolate the sprite MSB
    sta sprite_msb2        	; Store sprite MSB

	lda temp
	ror
	lsr
	lsr
	sta column

	lda sprite_msb2
	beq +
	lda column
	clc
	adc #$20
	sta column

	; calculate the screen row
+	lda sprite_y
	sec
	sbc #$32				; y-offset, visible screen area starts at y=$32
	lsr						; value / 8
	lsr
	lsr
	sta row

	; calculate screen matrix address
scr_addr_calc_addr
	lda row
	ldy #40   						; Number of columns per row
	jsr multiply_8b_by_8b
	
	ldx #$00
	stx screen_addr_hi

	clc        						; Clear carry flag
	adc column    					; Add the column value
	sta screen_addr_lo
	bcc +
	
	inc screen_addr_hi

+	lda #<SCREEN_RAM 				; Low byte of screen memory address
	adc screen_addr_lo
	sta screen_addr_lo
	
	lda #>SCREEN_RAM 				; High byte of screen memory address
	adc screen_addr_hi						
	sta screen_addr_hi

	lda result
	clc
	adc screen_addr_hi
	sta screen_addr_hi

	;ldy #$00
	;lda #$01
	;sta (screen_addr_lo),y

	rts

	



; ====================================================================
multiply_8b_by_8b:

num1Hi = $70
; ====================================================================
	sta num1
	sty num2
	lda #$00
	tay
	sty num1Hi  		; remove this line for 16*8=16bit multiply
	beq enterLoop

doAdd:
	clc
	adc num1
	tax
	tya
	adc num1Hi
	tay
	txa

loop:
	asl num1
	rol num1Hi
enterLoop:  			; accumulating multiply entry point (enter with .A=lo, .Y=hi)
	lsr num2
	bcs doAdd
	bne loop

	sta result+1
	sty result
    RTS					; Return from subroutine

num1:
	.byte $00			; 8 bit multiplication
num2:
	.byte $00
result:
	.byte $00, $00		; 16 bit result

; ====================================================================
play_chomp_sound:
; ====================================================================
	lda playing_chomp
	beq play_chomp_sound_yes
	rts

play_chomp_sound_yes:
	lda #$01
	sta playing_chomp
	
	jsr sidclr
	lda #15			; set volume
	sta SID_VOL
	lda #$00		; set attack / decay
	sta SID_ATDCY1
	lda #$10		; set sustain / release
	sta SID_SUREL1
	lda #132		; voice 1 freq (lo)
	sta SID_FRELO1
	lda #10			; voice 1 freq (hi)
	sta SID_FREHI1
	lda #%00010001	; triangle waveform and gate sound
	sta SID_VCREG1
	;lda #2			; cause a delay of two jiffies
	;adc JIFFLO		; add current jiffy reading
delay:
	;cmp JIFFLO		; and wait for two jiffies to elapse
	;bne delay
	lda #%00010000	; ungate sound
	sta SID_VCREG1

	lda #$00
	sta playing_chomp

	rts

sidclr:
	lda #$00			; fill with zero
	ldy #24
sidlop:
	sta SID_FRELO1,y 	; store zero in sid chip address
	dey					; for next lower byte
	bpl sidlop			; fill 25 bytes
	rts

playing_chomp:
	.byte $00

*= $3800
.include "screen_data.asm"
.include "sprite_data.asm"