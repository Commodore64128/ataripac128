; ===============================================================
;          ***      PAC-MAN for the Commodore 128    *****
;        *** ***        (ATARI 2600 version)       *** * ***
;        ***                                       *********
;         *****        Scott Hutter / xlar54       * * * * *
; ===============================================================

.include "vic.asm"
.include "sid.asm"

SCREEN_RAM				= $0400

; BANK 0 pointers to the sprite definitions below
SPRITE_0_POINTER    	= $07F8 		; Last 8 Bytes of Screen RAM
SPRITE_1_POINTER		= $07F9
SPRITE_2_POINTER		= $07FA
SPRITE_3_POINTER		= $07FB
SPRITE_4_POINTER		= $07FC
SPRITE_5_POINTER		= $07FD
SPRITE_6_POINTER		= $07FE
SPRITE_7_POINTER		= $07FF

; BANK 0 RAM containing sprite definitions
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

; temp ZP Variables
current_actor   = $C0 ; determines who is moving / current sprite (0-3 - pacman, ghost1, ghost2, ghost3, ghost4)
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
; 10 SYS 8142
*=$1c01
	.byte $1c, $1c, $0A, $00, $9E, $20, $38, $31, $39, $32
    .byte $00, $00, $00

;sprite definitions need to start at a 64 byte boundary
;so that sprite pointers work properly (bank * 64 + x)
*=$1c40
.include "sprite_data.asm"

; ====================================================================
; main startup
; ====================================================================
main:

	lda #$3e
	sta MMUCR				; All RAM1 with IO block

	jsr screen_init
	jsr chars_init
	jsr sprite_init
	jsr irq_init

	lda #$00
	sta current_actor

main_loop:
	lda VIC_SPR_FG_COL_REG
	sta collision_flg

	; check joystick
	lda IO_PORT_DATA_REG_A
	;cmp joy_cache
	;beq main_collision_check
	sta joy_cache
	jsr joystick_handler

	jsr actor_move
	jsr power_pellet_anim

main_spr_spr_collision_check:
	lda VIC_SPR_SPR_COL_REG					; check sprite to sprite collision register
	and #%00000001							; was the player (sprite 0) involved?
	beq main_collision_check				; no, skip ahead

	lda #$01								; set game state to player_dies
	sta game_state

main_wait_for_reset:						; loop while die animation and sound
	lda game_state
	beq main_loop_end
	jmp main_wait_for_reset


main_collision_check:
	lda collision_flg
	and #$01
	cmp #$01
    beq collide
	;inc VIC_BACKGROUND_COLOR

main_loop_end:

waitforvblank:
  	;LDA $D012      ; Read current raster line
  	;CMP #$F0       ; Check if it's past a specific line (e.g., line 240)
  	;BCC waitforvblank ; If not, keep waiting

-	inc delay
	lda delay
	cmp #$ff
	bne -
	inc delay+1
	lda delay+1
	cmp #$07
	bne -
	lda #00
	sta delay
	sta delay+1

	jmp main_loop

collide:
	;inc VIC_BORDER_COLOR
	;jsr play_chomp_sound
	jmp main_loop


	
; ====================================================================
; common data
; ====================================================================
delay_pp:
	.byte $00
delay:
	.byte $00, $00
delay_anim:
	.byte $00
collision_flg:
	.byte $00
player_x:
	.byte $00, $b0
player_y:
	.byte $b0

actor_directions:
	.byte DIR_STOPPED, DIR_RIGHT, DIR_RIGHT, DIR_LEFT, DIR_LEFT

actor_bytes_moved:
	.byte $00, $00, $00, $00, $00 	;tracks how many bytes the sprite has moved

actor_anim_frames:
	.byte $00, $00, $00, $00, $00	; how many animation frames per actor

joy_cache:
	.byte $00

game_state:
	.byte $00						; 0=running, 1=player dies

player_lives:
	.byte $03


; ====================================================================
; reset
; ====================================================================
reset:

	jsr sprite_init

	lda #$00
	sta actor_bytes_moved
	sta actor_bytes_moved+1
	sta actor_bytes_moved+2
	sta actor_bytes_moved+3
	sta actor_bytes_moved+4
	sta actor_anim_frames
	sta actor_anim_frames+1
	sta actor_anim_frames+2
	sta actor_anim_frames+3
	sta actor_anim_frames+4
	sta joy_cache
	sta delay_pp
	sta delay
	sta delay_anim
	sta pacman_die_anim_frame

	ldx #$00
	lda pacman_anim_pointers,x
	sta SPRITE_0_POINTER

	lda DIR_STOPPED
	sta actor_directions
	;lda DIR_RIGHT
	;sta actor_directions+1
	lda DIR_RIGHT
	sta actor_directions+2
	;lda DIR_LEFT
	;sta actor_directions+3
	;lda DIR_LEFT
	;sta actor_directions+4

	jsr select_player_r_anim

	lda #$00
	sta current_actor
	sta game_state
	rts


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
	; other options include
	; ...bank 0 - $0000 - $3fff
	; ...bank 1 - $4000 - $7fff
	; ...bank 2 - $8000 - $bfff
	; ...bank 3 - $c000 - $ffff
	;
	; bit 6 of the mmu register ($d506) determines if these banks are in first 64k block or second
	lda CIA2_PORT_REG_A
	and #%11111100
	ora #%00000011	; last two bits determine bank
	sta CIA2_PORT_REG_A

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

	; use default spr 0 pointer location (56x64=3584=$0e00)
	; LOCATION = (BANK * 16384) + (SPRITE POINTER VALUE * 64)
  	lda #113					
  	sta SPRITE_0_POINTER	; pacman (0 * 16384 + 113 x 64 = $1c40)
	lda #124
	sta SPRITE_1_POINTER	; ghost 1 (0 * 16384 + 124 x 64 = $1f00)
	sta SPRITE_2_POINTER	; ghost 2
	sta SPRITE_3_POINTER	; ghost 3
	sta SPRITE_4_POINTER	; ghost 4

	; sprite colors
	lda #YELLOW
  	sta SPRITE_0_COLOR
	lda #L_RED
	sta SPRITE_1_COLOR
	lda #L_BLUE
	sta SPRITE_2_COLOR
	lda #PURPLE
	sta SPRITE_3_COLOR
	lda #GREEN
	sta SPRITE_4_COLOR
 
	;starting player sprite location
	lda player_x
	sta SPRITE_XMSB
	lda player_x+1
	sta SPRITE_0_X_POSITION
	lda player_y
	sta SPRITE_0_Y_POSITION

	;starting ghost sprite location
	lda #$b0
	sta SPRITE_1_X_POSITION
	lda #$60
	sta SPRITE_1_Y_POSITION

	lda #$b0
	sta SPRITE_2_X_POSITION
	lda #$60;73
	sta SPRITE_2_Y_POSITION

	lda #$b0
	sta SPRITE_3_X_POSITION
	lda #$60;73
	sta SPRITE_3_Y_POSITION

	lda #$b0
	sta SPRITE_4_X_POSITION
	lda #$60;73
	sta SPRITE_4_Y_POSITION
	
	; turn on the sprites
	lda #%00011111          	; enable sprites 0,1,2,3,4
  	sta VIC_SPRITE_ENBL

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
; advance actor animation to the next frame
; ====================================================================
anim_actor:
	ldy current_actor
	cpy #$00
	bne anim_actor_ghost

	lda actor_anim_frames, y
	
	tax						
	LDA pacman_anim_pointers, x  	; Load data from the address
	sta SPRITE_0_POINTER
	inx
	txa
	cmp #$03
	bne +
	lda #$00
+	sta actor_anim_frames, Y
	rts

anim_actor_ghost:
	rts

pacman_anim_pointers:
.byte 113, 114, 115

; ====================================================================
anim_player_dies:
; ====================================================================

	; play the die animation
	lda pacman_die_anim_frame
	
	tax						
	LDA pacman_die_pointers, x  	; Load data from the address
	sta SPRITE_0_POINTER
	inx
	txa
	cmp #$06
	bne +
	
	jsr reset						; animation is done.  reset to play again
	rts

+	sta pacman_die_anim_frame
	rts

pacman_die_pointers:
	.byte 118, 119, 120, 121, 122, 123
pacman_die_anim_frame:
	.byte $00

; ====================================================================
; flash the power pellets if they exist
; ====================================================================
power_pellet_anim:
	lda COLOR_RAM + PP1_OFFSET
	cmp #BROWN
	beq +
	lda #BROWN
	jmp power_pellet_anim_flash
+	lda #YELLOW
power_pellet_anim_flash:
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
	rts

; ====================================================================
; IRQ interrupt service routine
; ====================================================================
irq_handler:
	php
	pha 
	tya
	pha
	txa
	pha

	lda game_state					; animation is different depending on game state
	bne irq_next					; player killed?

	;lda delay_pp
	;clc
	;adc #$01
	;sta delay_pp
	;cmp #$08
	;bne +
	
	;lda #$00
	;sta delay_pp

	lda delay_anim
	clc
	adc #$01
	sta delay_anim
	cmp #$08
	bne irq_end
	jsr anim_actor
	lda #$00
	sta delay_anim
	jmp irq_end

irq_next:
	cmp #$01					; player killed?
	bne irq_end

	lda delay_anim
	clc
	adc #$01
	sta delay_anim
	cmp #$08
	bne irq_end
	jsr anim_player_dies
	lda #$00
	sta delay_anim
	jmp irq_end

irq_end:	
	pla
	tax
	pla
	tay
	pla
	plp 
	jmp $fa65
	;jmp $ff33

; ====================================================================
actor_move:
; ====================================================================

	; loops through all four actors
	ldy current_actor

	; move the sprite
	lda actor_directions, y
	cmp #DIR_UP
	bne +
	jsr sprite_move_up
	jmp actor_move_loop

+	lda actor_directions, y
	cmp #DIR_DOWN
	bne +
	jsr sprite_move_down
	jmp actor_move_loop

+	lda actor_directions, y
	cmp #DIR_RIGHT
	bne +
	jsr sprite_move_right
	jmp actor_move_loop

+	lda actor_directions, y
	cmp #DIR_LEFT
	bne actor_move_loop
	jsr sprite_move_left

actor_move_loop:
	inc current_actor
	lda current_actor
	cmp #$05
	bne +
	lda #$00
	sta current_actor
	jmp actor_move_end
+	jmp actor_move

actor_move_end:
	rts

; ====================================================================
joystick_handler:
;
; This routine is called when a direction is changed by the player
; it will set the player_direction to the new direction value which
; is used in the irq routine for movement and if left or right, will 
; switch to the appropriate face / animation
; ====================================================================

	; we can only change directions if the player is at a pixel boundary
	ldy #$00
	lda actor_bytes_moved,y
	bne joystick_end

	lda joy_cache
	and #$08
	bne +

	; move right
	lda #DIR_RIGHT
	ldy #$00
	sta actor_directions, y
	jsr select_player_r_anim
	jmp joystick_end

+	lda joy_cache
	and #$04
	bne +

	; move left
	lda #DIR_LEFT
	ldy #$00
	sta actor_directions, y
	jsr select_player_l_anim
	jmp joystick_end

+	lda joy_cache
	and #$01
	bne +

	; move up
	lda #DIR_UP
	ldy #$00
	sta actor_directions, y
	jmp joystick_end

+	lda joy_cache
	and #$02
	bne joystick_end

	; move down
	lda #DIR_DOWN
	ldy #$00
	sta actor_directions, y

joystick_end:
	rts

; ====================================================================
; Sprite movement routines
; These routines are sprite agnostic.  Setting 'current_actor' to the
; appropriate sprite #, we can move any of the characters
; The routines check the bounds around the sprite's
; character screen matrix location for walls
; ====================================================================

; ====================================================================
ghost_change_dir:
; ====================================================================
	lda current_actor
	beq ghost_change_dir_done		; if current actor is player, exit
	tay
-	lda $a2							; read jiffy timer
	and #%00000011					; just the last 2 bits (0-3)
	clc
	adc #$01						; add 1 (0 is stopped)
	cmp actor_directions,Y			; compare it to the current direction
	beq -							; if its the same, do it again
	sta actor_directions,y			; store the new direction

ghost_change_dir_done:
	rts	

; ====================================================================
sprite_move_right:
; ====================================================================

	; if we are already moving, keep doing so
	ldy current_actor
	lda actor_bytes_moved,y
	bne pmr_move

	; calculate the sprite screen address
	jsr sprite_to_screen_address

	; we are looking one space to the right
	; so add 1 to the location
    lda screen_addr_lo 
    clc 
    adc #$01
    sta screen_addr_lo
    lda screen_addr_hi
    adc #$00
    sta screen_addr_hi

	; barrier check	
	ldx #$02						; loop twice to check both above and below 8bit quad
	ldy #$00
pmr_l0:
	jsr barrier_check
	beq +
	jsr ghost_change_dir
	jmp pmr_done					; nope - exit
+	dex
	beq pmr_ok2move
	ldy #$28
	jmp pmr_l0

pmr_ok2move:
	ldy current_actor
	lda #$08
	sta actor_bytes_moved,y

pmr_move:
	; move the sprite
	lda current_actor        	; Load sprite number
	asl                   	; Multiply current_actor by 2 to get the correct index for X and Y
	tay                   	; Transfer the result to X
	lda BASE_SPRITE_X, Y  	; Load sprite X-coordinate

	clc 
	adc #$01
	sta BASE_SPRITE_X,Y
	bcc +
	jsr set_xmsb

+	ldy current_actor
	lda actor_bytes_moved,Y
	sec
	sbc #$01
	sta actor_bytes_moved,Y

pmr_done:
	rts

; ====================================================================
sprite_move_left:
; ====================================================================

	; if we are already moving, keep doing so
	ldy current_actor
	lda actor_bytes_moved,y
	bne pml_move

	; calculate the sprite screen address
	jsr sprite_to_screen_address

	; we are looking one spaces to the left
	; so subtract 1 to the address
	lda screen_addr_lo
	sec
	sbc #$01
	sta screen_addr_lo
	lda screen_addr_hi
    sbc #$00
    sta screen_addr_hi

	; barrier check	
	ldx #$02						; loop twice to check both above and below 8bit quad
	ldy #$00
pml_l0:
	jsr barrier_check
	beq +
	jsr ghost_change_dir
	jmp pml_done					; nope - exit
+	dex
	beq pml_ok2move
	ldy #$28
	jmp pml_l0

pml_ok2move:
	ldy current_actor
	lda #$08
	sta actor_bytes_moved,Y

pml_move:
	; move the sprite
	lda current_actor        	; Load sprite number
	asl                   	; Multiply current_actor by 2 to get the correct index for X and Y
	tay                   	; Transfer the result to X
	lda BASE_SPRITE_X, Y  	; Load sprite X-coordinate

	sec 
	sbc #$01
	sta BASE_SPRITE_X,Y
	bcs +
	jsr clear_xmsb

+	ldy current_actor
	lda actor_bytes_moved,Y
	sec
	sbc #$01
	sta actor_bytes_moved,Y

pml_done:
	rts

; ====================================================================
sprite_move_up:
; ====================================================================

	; if we are already moving, keep doing so
	ldy current_actor
	lda actor_bytes_moved,y
	bne pmu_move

	; calculate the sprite screen address
	jsr sprite_to_screen_address

	; we are looking one space up, one to the left
	; so subtract 41 from the address
	lda screen_addr_lo
	sec
	sbc #$29
	sta screen_addr_lo
	lda screen_addr_hi
	sbc #$00
	sta screen_addr_hi

	; barrier check	
	ldx #$02						; loop twice to check both above and below 8bit quad
	ldy #$00
pmu_l0:
	jsr barrier_check
	beq +
	jsr ghost_change_dir
	jmp pmu_done					; nope - exit
+	dex
	beq pmu_ok2move
	ldy #$01
	jmp pmu_l0
	rts	

pmu_ok2move:
	ldy current_actor
	lda #$0a
	sta actor_bytes_moved,Y

pmu_move:
	; move the sprite
	lda current_actor        	; Load sprite number
	asl                   	; Multiply current_actor by 2 to get the correct index for X and Y
	tay                   	; Transfer the result to X
	lda BASE_SPRITE_Y, Y  	; Load sprite X-coordinate
	sec
	sbc #$01
	sta BASE_SPRITE_Y, Y

	ldy current_actor
	lda actor_bytes_moved,Y
	sec
	sbc #$01
	sta actor_bytes_moved,Y

pmu_done:
	rts

; ====================================================================
sprite_move_down:
; ====================================================================

	; if we are already moving, keep doing so
	ldy current_actor
	lda actor_bytes_moved,y
	bne pmd_move

	; calculate the sprite screen address
	jsr sprite_to_screen_address

	; we are looking one space down, one to the left
	; so add 39 to the address
	lda screen_addr_lo
	clc
	adc #$4f ;27
	sta screen_addr_lo
	lda screen_addr_hi
	adc #$00
	sta screen_addr_hi

	; barrier check	
	ldx #$02						; loop twice to check both above and below 8bit quad
	ldy #$00
pmd_l0:
	jsr barrier_check
	beq +
	jsr ghost_change_dir
	jmp pmd_done					; nope - exit
+	dex
	beq pmd_ok2move
	ldy #$01
	jmp pmd_l0
	rts	

pmd_ok2move:
	ldy current_actor
	lda #$0a
	sta actor_bytes_moved,y

pmd_move:
	; move the sprite
	lda current_actor        	; Load sprite number
	asl                   	; Multiply current_actor by 2 to get the correct index for X and Y
	tay                   	; Transfer the result to X
	lda BASE_SPRITE_Y, Y  	; Load sprite X-coordinate
	clc
	adc #$01
	sta BASE_SPRITE_Y, Y
	
	ldy current_actor
	lda actor_bytes_moved,Y
	sec
	sbc #$01
	sta actor_bytes_moved,Y

pmd_done:
	rts

; ====================================================================
set_xmsb:
;	Y = sprite number
; ====================================================================
	; set the XMSB for this sprite (Y = sprite number)
	LDY current_actor
    LDA #1          		; Load 1 into the accumulator as the bitmask
	sta temp				; store bitmask
	cpy #$00
	beq set_xmsb_skip
set_xmsb_loop:
	ASL temp
    DEY             		; Decrement Y
    BNE set_xmsb_loop   	; ...looping, counting down Y
set_xmsb_skip:
    LDA SPRITE_XMSB   		; Load the sprite X MSB
    ORA temp   				; OR with the bitmask to turn on the bit
    STA SPRITE_XMSB   		; Store the result back, turning on the X MSB
	rts

; ====================================================================
clear_xmsb:
;	Y = sprite number
; ====================================================================
	; set the XMSB for this sprite (Y = sprite number)
	LDY current_actor
	LDA SPRITE_XMSB   		; Load the byte you want to modify
	AND clear_xmsb_masks, Y	; Apply the bitmask using the AND instruction
	STA SPRITE_XMSB   		; Store the result back to the byte
	rts

clear_xmsb_masks:
	.byte $FE, $FD, $FB, $F7, $EF, $DF, $BF, $7F

; ====================================================================
barrier_check:
; ====================================================================

	; look at address.  if A=0 if we can move there
	lda (screen_addr_lo),y
	cmp #$20
	beq barrier_check_ok
	cmp #$45
	beq barrier_check_ok
	cmp #$52
	beq barrier_check_ok

	cmp #$6C
	beq barrier_check_ok
	cmp #$7B
	beq barrier_check_ok
	cmp #$7c
	beq barrier_check_ok
	cmp #$7e
	beq barrier_check_ok

	cmp #$6f
	beq barrier_check_ok

	cmp #$77
	bne barrier_check_not_ok
	lda actor_directions, y
	cmp #DIR_RIGHT
	bcs barrier_check_ok
	jmp barrier_check_not_ok
	
	;cmp #$D1
	;beq barrier_check_ok

barrier_check_not_ok:
	rts


barrier_check_ok:
	lda #$00
	rts

; ====================================================================
select_player_r_anim:
; ====================================================================
	; change sprites for animation
	lda #114
	sta pacman_anim_pointers+1
	lda #115
	sta pacman_anim_pointers+2
	rts

; ====================================================================
select_player_l_anim:
; ====================================================================
	; change sprites for animation
	lda #116
	sta pacman_anim_pointers+1
	lda #117
	sta pacman_anim_pointers+2
	rts

; ====================================================================
sprite_to_screen_address:
; ====================================================================

	; the sprite is a 16bit x 16bit character, taking 4 screen addresses.
	; Determining the screen address it is over is more complicated than 
	; if it were just a simple 8x8 character. 
	;
	; The direction it is going, determines which address is returned.  
	;
	; For up, we want the very top left of the sprite which is offset by +1
	; For down, we want +14
	; For right, +14
	; For left, +1

spr2scr_calc_x_offset:

	ldy current_actor
	lda actor_directions,y
	cmp #DIR_LEFT
	bne +
	lda #$07
	sta spr2scr_offset_x+1
	jmp spr2scr_begin_calc

+	lda #$08
	sta spr2scr_offset_x+1

spr2scr_begin_calc:
	; calculate screen column

	lda current_actor        	; Load sprite number
	asl                   	; Multiply current_actor by 2 to get the correct index for X and Y
	tax                   	; Transfer the result to X

	lda BASE_SPRITE_X, X  	; Load sprite X-coordinate
	clc

spr2scr_offset_x:
	adc #$08				; 
	sta sprite_x          	; Store sprite X-coordinate
	lda BASE_SPRITE_Y, X  	; Load sprite Y-coordinate
	clc
	adc #$02				; add 2 as a Y adjustment (because sprite bit starts 2 down)
	sta sprite_y          	; Store sprite Y-coordinate


	; extract the selected sprite's X MSB
	; equivalent to  "sprite_msb2 = getSpriteXMSB(int current_actor);"

	lda SPRITE_XMSB        	; Load sprite X-coordinate MSB
    ldx current_actor        	; Load sprite number into X
	cpx #$00
	beq spr2scr_done_shift_msb
spr2scr_shift_msb:
    lsr                   	; Shift the bits in the SPRITE_XMSB register to the right
	dex                   	; Decrement X
    bne spr2scr_shift_msb  	; If X is not 0, continue shifting
spr2scr_done_shift_msb:
    and #$01               	; Perform a bitwise AND with 1 to isolate the sprite MSB
    sta sprite_msb2        	; Store sprite MSB

	; equivalent to  "A = sprite_x - 24;" 
	; x-offset, visible screen area starts at x=$18(24)
	; but, we have to account for the X msb

	beq +					; jump ahead if the msb is zero
	lda sprite_x			; get the sprite x value
	cmp #$18
	bcs +					; jump ahead if it is >= 24
	lda #$00				; put 0 in the msb since the subtraction
	sta sprite_msb2			; ..that will happen ahead would cause it to become 0

+	lda sprite_x
	sec         
	sbc #$18

	; equivalent to  "column = A / 8;"
	lsr
	lsr
	lsr
	sta column

	; if the X MSB is not zero, add 32 to the column result
	; equivalent to "if(sprite_msb2 != 0) { column = column + 32; }"

	lda sprite_msb2
	beq +
	lda column
	clc
	adc #$20
	sta column

	; calculate the screen row
	; equivalent to "row = (sprite_y - 50) / 8; "
+	lda sprite_y
	sec
	sbc #$32				; y-offset, visible screen area starts at y=$32
	lsr						; value / 8
	lsr
	lsr
	sta row

	; at this point, row and column (C8 and C7) contain the proper values for this sprite
	; now we can calculate screen matrix address

	; equivalent to " result = row * 40;"
	lda row
	ldy #40   						; Number of columns per row
	jsr mult_8b_by_8b
	
	ldx #$00
	stx screen_addr_hi

	; equivalent to " address = screen_ram + (row * 40 + column);"

	clc        						; Clear carry flag
	adc column    					; Add the column value
	sta screen_addr_lo
	lda screen_addr_hi				; add the carry to the hi address
	adc #$00
	sta screen_addr_hi
	
	lda #<SCREEN_RAM 				; Low byte of screen memory address
	adc screen_addr_lo
	sta screen_addr_lo
	
	lda #>SCREEN_RAM 				; High byte of screen memory address
	adc screen_addr_hi						
	sta screen_addr_hi

	lda result
	clc
	adc screen_addr_hi
	sta screen_addr_hi

	; visual debug
	;ldy #$00
	;lda #$01
	;sta (screen_addr_lo),y

	rts

	
; ====================================================================
mult_8b_by_8b:
num1Hi = $70
; ====================================================================
	sta num1
	sty num2
	lda #$00
	tay
	sty num1Hi  		; remove this line for 16*8=16bit multiply
	beq mult_enterLoop

mult_doAdd:
	clc
	adc num1
	tax
	tya
	adc num1Hi
	tay
	txa

mult_loop:
	asl num1
	rol num1Hi
mult_enterLoop:  		; accumulating multiply entry point (enter with .A=lo, .Y=hi)
	lsr num2
	bcs mult_doAdd
	bne mult_loop
	sta result+1
	sty result
    rts

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
sdelay:
	;cmp JIFFLO		; and wait for two jiffies to elapse
	;bne sdelay
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
