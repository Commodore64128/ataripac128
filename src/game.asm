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
DIR_RIGHT	= $04
DIR_LEFT	= $08

GAME_STATE_DEMO 	= $00
GAME_STATE_NEWGAME 	= $01
GAME_STATE_RESET	= $02
GAME_STATE_RUNNING	= $03
GAME_STATE_DYING	= $04
GAME_STATE_QUIT		= $05

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
	sta MMUCR								; All RAM1 with IO block

	jsr chars_init
	jsr irq_init

	jsr new_game

main_loop:
	lda IO_PORT_DATA_REG_A					; capture joystick
	sta joy_cache

	jsr player_input_handler
	jsr actor_move
	jsr power_pellet_anim

main_spr_spr_collision_check:
	lda VIC_SPR_SPR_COL_REG					; check sprite to sprite collision register
	and #%00000001							; was the player (sprite 0) involved?
	beq main_loop_end						; no, skip ahead

	lda #GAME_STATE_DYING					; set game state to player_dies
	sta game_state
	jsr play_die_sound

main_wait:
	lda game_state							; wait for game state to change
	cmp #GAME_STATE_DYING
	bne main_loop_end
	jmp main_wait

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
	cmp #$05
	bne -
	lda #00
	sta delay
	sta delay+1

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
delay_enemy_mode:
	.byte $00, $00
delay_enemy_move:
	.byte $00, $00
delay_enemy_flash:
	.byte $00, $00
collision_flg:
	.byte $00
player_x:
	.byte $00, $b0
player_y:
	.byte $b0


actor_directions:
	.byte DIR_STOPPED, DIR_UP, DIR_UP, DIR_UP, DIR_UP

actor_bytes_moved:
	.byte $00, $00, $00, $00, $00 	;tracks how many bytes the sprite has moved

actor_anim_frames:
	.byte $00, $00, $00, $00, $00	; how many animation frames per actor

actor_rows:
	.byte $00, $00, $00, $00, $00	; character row for sprite (y axis)

actor_cols:
	.byte $00, $00, $00, $00, $00	; character col for sprite (X axis)

actor_position_matrix:				; location of player in position matrix
	.byte <(postion_matrix+37*14+18), >(postion_matrix+37*14+18)
	.byte <(postion_matrix+37*8+18), >(postion_matrix+37*8+18)
	.byte <(postion_matrix+37*8+18), >(postion_matrix+37*8+18)
	.byte <(postion_matrix+37*8+18), >(postion_matrix+37*8+18)
	.byte <(postion_matrix+37*8+18), >(postion_matrix+37*8+18)



joy_cache:
	.byte $00

game_state:
	.byte $00						; 0=demo mode, 1=new game, 2=resetting, 3=running, 4=player dies 

player_lives:
	.byte $03

player_score:
	.word $0000

enemy_mode:
	.byte $00

; ====================================================================
; new game
; ====================================================================
new_game:
	lda #GAME_STATE_NEWGAME
	sta game_state
	
	lda #$03
	sta player_lives

	jsr screen_init
	jsr reset

	; start new game "music"
	rts

; ====================================================================
; reset
; ====================================================================
reset:
	lda #GAME_STATE_RESET
	sta game_state

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

	ldx #$00
	lda ghost_anim_pointers,x
	sta SPRITE_1_POINTER
	sta SPRITE_2_POINTER
	sta SPRITE_3_POINTER
	sta SPRITE_4_POINTER

	lda #DIR_STOPPED
	sta actor_directions
	lda #DIR_UP
	sta actor_directions+1
	sta actor_directions+2
	sta actor_directions+3
	sta actor_directions+4

	lda #<(postion_matrix+37*14+18)
	sta actor_position_matrix
	lda #>(postion_matrix+37*14+18)
	sta actor_position_matrix+1

	lda #<(postion_matrix+37*8+18)
	sta actor_position_matrix+2
	sta actor_position_matrix+4
	sta actor_position_matrix+6
	sta actor_position_matrix+8

	lda #>(postion_matrix+37*8+18)
	sta actor_position_matrix+3
	sta actor_position_matrix+5
	sta actor_position_matrix+7
	sta actor_position_matrix+9

	jsr select_player_r_anim

	lda #$00
	sta current_actor
	sta enemy_mode
	sta delay_enemy_mode
	sta delay_enemy_mode+1

	jsr play_reset_sound

	lda #GAME_STATE_RUNNING
	sta game_state
	rts

; ====================================================================
; quit the game
; ====================================================================
quit:
	sei
	lda irq_original
	sta $314
	lda irq_original+1
	sta $315
	cli

	rts

; ====================================================================
; initialize the irq vector
; ====================================================================
irq_init:
	sei

	lda $314
	sta irq_original
	lda $315
	sta irq_original+1

	lda #>irq_handler
	sta $315
	lda #<irq_handler
	sta $314

	cli
	rts

irq_original:
	.byte $00, $00

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
	
	;inc $d020						; debug
	;lda irq_sound_playing
	;bne +
	;lda dot_eaten
	;bne +
	;jsr play_chomp_sound
	;lda #$00
	;sta dot_eaten

	lda delay_enemy_flash		; flash the enemies (for authentic look)
	inc delay_enemy_flash
	cmp #$02
	bne +
	lda #%00000001            	; enable sprites 0,1,2,3,4
  	sta VIC_SPRITE_ENBL
	lda #$00
	sta delay_enemy_flash
	jmp irq_check_state
+	lda #%00011111          	; disable sprites 0,1,2,3,4
  	sta VIC_SPRITE_ENBL
	jmp irq_check_state

irq_check_state:
	; takes different actions depending on game state
	lda game_state
	cmp #GAME_STATE_DEMO
	bne irq_check_state1
	jmp irq_end						; if game_state = 0, demo mode.  Do nothing here

irq_check_state1:
	cmp #GAME_STATE_NEWGAME
	bne irq_check_state2
	jmp irq_end						; if game_state = 1, new game.  Do nothing here

irq_check_state2:
	cmp #GAME_STATE_RESET
	bne irq_check_state3			
	jmp irq_end						; if game_state = 2, resetting.  Do nothing here

irq_check_state3:
	cmp #GAME_STATE_RUNNING
	bne irq_check_state4			

	lda delay_anim					; if game_state = 3, running. show actor anim frames
	clc
	adc #$01
	sta delay_anim
	cmp #$08
	bne irq_end
	jsr anim_actor
	lda #$00
	sta delay_anim

	lda delay_enemy_mode			; countdown enemy mode change
	clc
	adc #$01
	sta delay_enemy_mode
	lda delay_enemy_mode+1
	adc #$00
	sta delay_enemy_mode+1

	lda delay_enemy_mode
	cmp #$2c
	bne +
	lda delay_enemy_mode+1
	cmp #$00
	bne +
	lda #$01
	sta enemy_mode
+	jmp irq_end


irq_check_state4:
	cmp #GAME_STATE_DYING
	bne irq_end						

	lda delay_anim					; if game_state = 4, player killed. show dying anim frames
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
	lda #$74 ;60
	sta SPRITE_1_Y_POSITION

	lda #$b0
	sta SPRITE_2_X_POSITION
	lda #$74; 60;73
	sta SPRITE_2_Y_POSITION

	lda #$b0
	sta SPRITE_3_X_POSITION
	lda #$74; 60;73
	sta SPRITE_3_Y_POSITION

	lda #$b0
	sta SPRITE_4_X_POSITION
	lda #$74; 60;73
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
player_input_handler:
;
; This routine is called when a direction is changed by the player
; it will set the player_direction to the new direction value which
; is used in the irq routine for movement and if left or right, will 
; switch to the appropriate face / animation
; ====================================================================

	; we can only change directions if the player is at a pixel boundary
	ldy #$00
	lda actor_bytes_moved,y
	bne player_input_end

	; stash current direction (incase we cant change directions due to wall)
	lda actor_directions, y
	sta player_current_direction

	lda joy_cache
	and #$08
	bne +

	; move right
	lda #DIR_RIGHT
	ldy #$00
	sta actor_directions, y
	jsr select_player_r_anim
	jmp player_input_end

+	lda joy_cache
	and #$04
	bne +

	; move left
	lda #DIR_LEFT
	ldy #$00
	sta actor_directions, y
	jsr select_player_l_anim
	jmp player_input_end

+	lda joy_cache
	and #$01
	bne +

	; move up
	lda #DIR_UP
	ldy #$00
	sta actor_directions, y
	jmp player_input_end

+	lda joy_cache
	and #$02
	bne player_input_end

	; move down
	lda #DIR_DOWN
	ldy #$00
	sta actor_directions, y

player_input_end:
	rts

player_current_direction:
	.byte DIR_STOPPED

; ====================================================================
; advance actor animation to the next frame
; ====================================================================
anim_actor:
	ldy current_actor
	lda actor_anim_frames, y
	
	tax						
	LDA pacman_anim_pointers, x  	; Load data from the address
	sta SPRITE_0_POINTER

	LDA ghost_anim_pointers, x  	; Load data from the address
	sta SPRITE_1_POINTER
	sta SPRITE_2_POINTER
	sta SPRITE_3_POINTER
	sta SPRITE_4_POINTER

	inx
	txa
	cmp #$03
	bne +
	lda #$00
+	sta actor_anim_frames, Y
	rts

pacman_anim_pointers:
.byte 113, 114, 115

ghost_anim_pointers:
.byte 124,125,126,127

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

; data to represent possible directions  
; this allows us to skip wall collision detection
; There are 37 positions horizontally that an actor can be in,
; and 15 vertically.
;
; each byte represents a position.  the lower four bits represent
; possible movement in this location:
; ....0001 = up
; ....0010 = down
; ....0100 = right
; ....1000 = left
;
; 5th bit indicates if a dot is there.  this value changes to 0 when eaten
; 6th bit indicates if a power pellet is there.  this value changes to 0 when eaten
;
; this matrix makes up the actual maze.  each actor can move 8 bytes left/right on the screen, 10 bytes up/down from one matrix cell to another.
; the bit patterns determine which direction an actor may go in that cell
; its a lot of data, but prevents us from sprite to screen calculations and looking around the actor for walls
PM_BAD	= %00000000
PM_RL 	= %00011100
PM_DR 	= %00010110
PM_DL 	= %00011010
PM_UR	= %00010101
PM_UL 	= %00011001
PM_UD 	= %00010011
PM_UDL 	= %00011011
PM_UDR	= %00010111
PM_UDRL = %00011111
PM_DRL	= %00011110
PM_URL 	= %00011101
PM_GRL  = %10001100
PM_GR   = %10000100
PM_BTM  = %11000010
PM_TOP	= %11100010
PM_U    = %00000001
PM_D	= %00000010

postion_matrix:
;                                                                                                                                                     center
.byte PM_BAD,PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD,  PM_BAD, PM_BAD, PM_BTM, PM_BAD, PM_BAD, PM_BAD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD
.byte PM_BAD,PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD,  PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD
.byte PM_DR, PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_RL,  PM_RL,  PM_DL,  PM_BAD, PM_BAD, PM_DR,   PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_DL,   PM_BAD, PM_BAD, PM_DR,  PM_RL,  PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_DL
.byte PM_UD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD
.byte PM_UR, PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_UL,  PM_BAD, PM_BAD, PM_UDR, PM_RL,  PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_UDRL, PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_UDRL, PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_RL,  PM_UDL, PM_BAD, PM_BAD, PM_UR,  PM_RL,  PM_DRL,  PM_RL,  PM_RL,  PM_UL
.byte PM_BAD,PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD
.byte PM_DR, PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_DRL, PM_RL,  PM_RL,  PM_UDRL,PM_RL,  PM_RL,  PM_RL,  PM_DL,  PM_BAD, PM_BAD, PM_UDR,  PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_UDL,  PM_BAD, PM_BAD, PM_DR,  PM_RL,  PM_RL,  PM_RL,  PM_UDRL,PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_DL
.byte PM_UD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD
.byte PM_UR, PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_UL,  PM_BAD, PM_BAD, PM_UDR, PM_RL,  PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_UDL,  PM_BAD, PM_BAD, PM_GR,  PM_GRL, PM_GRL, PM_UDR,  PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_RL,  PM_UDL, PM_BAD, PM_BAD, PM_UR,  PM_RL,  PM_DRL, PM_RL,  PM_RL,  PM_UL
.byte PM_BAD,PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD
.byte PM_DR, PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_DRL, PM_RL,  PM_RL,  PM_UDRL,PM_RL,  PM_RL,  PM_RL,  PM_DL,  PM_BAD, PM_BAD, PM_UDR,  PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_UDL,  PM_BAD, PM_BAD, PM_DR,  PM_RL,  PM_RL,  PM_RL,  PM_UDRL,PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_DL
.byte PM_UD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD
.byte PM_UR, PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_UL,  PM_BAD, PM_BAD, PM_UDR, PM_RL,  PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_UDRL, PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_UDRL, PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_RL,  PM_UDL, PM_BAD, PM_BAD, PM_UR,  PM_RL,  PM_DRL,  PM_RL,  PM_RL,  PM_UL
.byte PM_BAD,PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD
.byte PM_DR, PM_RL,  PM_RL,  PM_URL, PM_RL, PM_DRL,  PM_RL,  PM_RL,  PM_UDRL,PM_RL,  PM_RL,  PM_RL,  PM_DL,  PM_BAD, PM_BAD, PM_UDR,  PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_UDL,  PM_BAD, PM_BAD, PM_DR,  PM_RL,  PM_RL,  PM_RL,  PM_UDRL,PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_DL
.byte PM_UD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_UD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_UD
.byte PM_UR, PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_UL,  PM_BAD, PM_BAD, PM_UR,  PM_RL,  PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_URL,  PM_RL,  PM_RL,  PM_DRL, PM_RL,  PM_RL,  PM_URL,  PM_RL,  PM_RL,  PM_URL, PM_RL,  PM_RL,  PM_RL,  PM_UL,  PM_BAD, PM_BAD, PM_UR,  PM_RL,  PM_RL,  PM_RL,  PM_RL,  PM_UL
.byte PM_BAD,PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD,  PM_BAD, PM_BAD, PM_UD,   PM_BAD, PM_BAD, PM_BAD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD
.byte PM_BAD,PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD,  PM_BAD, PM_BAD, PM_TOP, PM_BAD, PM_BAD, PM_BAD,  PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD, PM_BAD


; ====================================================================
actor_move:
; ====================================================================

	; loops through all four actors
	ldy current_actor
	beq actor_do_move
	jmp actor_check_intersection

	; if ghost, respect delay
	lda actor_bytes_moved,y
	bne actor_check_intersection

	lda delay_enemy_move			; countdown enemy mode change
	clc
	adc #$01
	sta delay_enemy_move
	lda delay_enemy_move+1
	adc #$00
	sta delay_enemy_move+1

	lda delay_enemy_move
	cmp #$0a
	bne actor_move_abort
	lda delay_enemy_move+1
	cmp #$00
	bne actor_move_abort
	jmp actor_move_reset_delay

actor_move_abort:
	jmp actor_move_loop
actor_move_reset_delay:
	lda #$00
	sta delay_enemy_move
	sta delay_enemy_move+1

	; if ghost, check if at an intersection
actor_check_intersection:
	lda actor_bytes_moved,y
	bne actor_do_move

	dec actor_move_ctr
	lda actor_move_ctr
	cmp #$00
	bne actor_do_move
	lda #$03
	sta actor_move_ctr
	
	; at intersection - find best path
	jsr ghost_change_dir

actor_do_move:
	ldy current_actor

	; move the sprite
	lda actor_directions, y
	and #DIR_UP
	beq +
	jsr sprite_move_up
	jmp actor_move_loop

+	lda actor_directions, y
	and #DIR_DOWN
	beq +
	jsr sprite_move_down
	jmp actor_move_loop

+	lda actor_directions, y
	and #DIR_RIGHT
	beq +
	jsr sprite_move_right
	jmp actor_move_loop

+	lda actor_directions, y
	and #DIR_LEFT
	beq actor_move_loop
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

actor_move_ctr:
	.byte $03


; ====================================================================
; Sprite movement routines
; These routines are sprite agnostic.  Setting 'current_actor' to the
; appropriate sprite #, we can move any of the characters
; The routines also check if movement is allowed based on the 
; position matrix (maze)
; ====================================================================

; ====================================================================
check_if_player_eat_dot:
; ====================================================================
	; if player, check if over pellet
	ldy current_actor
	beq +
	jmp ciped_done

	; temporarily hold screen locations
+	lda screen_addr_lo
	pha
	lda screen_addr_hi
	pha

	; check top quad
	jsr check_quad_for_dot

	; add 40 to location to check bottom quad
+	lda screen_addr_lo
	clc
	adc #$28
	sta screen_addr_lo
	lda screen_addr_hi
	adc #$00
	sta screen_addr_hi

	; check bottom quad
	jsr check_quad_for_dot

	; subtract 41 to location to check top left quad
+	lda screen_addr_lo
	sec
	sbc #$29
	sta screen_addr_lo
	lda screen_addr_hi
	sbc #$00
	sta screen_addr_hi

	; check top left quad
	jsr check_quad_for_dot

 	; add 40 to location to check bottom left quad
+	lda screen_addr_lo
	clc
	adc #$28
	sta screen_addr_lo
	lda screen_addr_hi
	adc #$00
	sta screen_addr_hi

	; check bottom left quad
	jsr check_quad_for_dot

ciped_restore:
	; restore screen location
	pla
	sta screen_addr_hi
	pla
	sta screen_addr_lo

ciped_done:
	rts

check_quad_for_dot:
	lda (screen_addr_lo),y
	cmp #$52
	bne +
	lda #$20
	sta (screen_addr_lo),y
	lda #$01
	sta dot_eaten
	jsr inc_player_score
	jmp dot_check_end
+	cmp #$45
	bne +
	lda #$20
	sta (screen_addr_lo),y
	lda #$01
	sta dot_eaten
	jsr inc_player_score
	jmp dot_check_end
+	cmp #$6c
	bne +
	lda #$20
	sta (screen_addr_lo),y
	lda #$01
	sta dot_eaten
	jmp dot_check_end
+	cmp #$7b
	bne +
	lda #$20
	sta (screen_addr_lo),y
	lda #$01
	sta dot_eaten
	jmp dot_check_end
+	cmp #$7c
	bne +
	lda #$20
	sta (screen_addr_lo),y
	lda #$01
	sta dot_eaten
	jmp dot_check_end
+	cmp #$7e
	bne dot_check_end
	lda #$20
	sta (screen_addr_lo),y
	lda #$01
	sta dot_eaten
	jmp dot_check_end
dot_check_end:
	jsr update_score
	rts

dot_eaten:
	.byte $00

; ====================================================================
inc_player_score:
; ====================================================================
	lda <player_score
	clc
	adc #$01
	sta <player_score
	lda >player_score
	adc #$00
	sta >player_score
	rts

; ====================================================================
update_score:
; Routine to convert a 16-bit binary number to ASCII and display it
; ====================================================================
	pha
	txa
	pha
	tya
	pha

	ldx <player_score
	ldy >player_score

	STX div_lo
	STY div_hi

	LDY #$04
-   JSR div10
	ORA #$30
	STA $0788,Y
	DEY
	BPL -

	pla
	tay
	pla
	tax
	pla
	RTS


div10:
	LDX #$11
	LDA #$00
	CLC
-   ROL
	CMP #$0A
	BCC +
	SBC #$0A
+	ROL div_lo
	ROL div_hi
	DEX
	BNE -
	RTS

div_lo:
	.byte $00
div_hi:
	.byte $00

; ====================================================================
ghost_change_dir:
; ====================================================================
	lda current_actor
	bne +							; if current actor is player, exit
	jmp ghost_change_dir_done

+	tay
 
	lda #$00
	sta gc_temp+1

	jsr check_position_matrix		; which directions are available?
	sta gc_temp						; save it

	lda current_actor       		; Load sprite number
	tay                   			; represents current ghost
	ldx #$00						; represent pacman

	lda enemy_mode					; are we in chase mode?
	beq +							; if so skip ahead
	
	lda actor_rows, y				; replace scatter target
	sta gc_rows, y					; with pacman target
	lda actor_cols, y
	sta gc_cols, y

	; determine if ghost is above target
+	lda gc_rows, y	  				; Load target row
	cmp actor_rows, x
	beq ghost_change_next1			; ghost row = target row (skip ahead)
	bcs ghost_change_row_below		; ghost row < target row
									; ghost row > target row
ghost_change_row_above:
	lda gc_temp+1
	ora #$02
	sta gc_temp+1					; turn on the DOWN bit
	jmp ghost_change_next1

ghost_change_row_below:
	lda gc_temp+1
	ora #$01
	sta gc_temp+1					; turn on the UP bit

ghost_change_next1:
	; determine if ghost is left or right of target
	lda gc_cols, Y					; Load target col
	cmp actor_cols, x
	beq ghost_change_next2			; ghost col = target col (skip ahead)
	bcs ghost_change_col_left		; ghost col > target col
									; ghost col < target col
ghost_change_col_right:
	lda gc_temp+1
	ora #$04
	sta gc_temp+1					; turn on the RIGHT bit
	jmp ghost_change_next2

ghost_change_col_left:
	lda gc_temp+1
	ora #$08
	sta gc_temp+1					; turn on the LEFT bit

ghost_change_next2:
	lda gc_temp
	and gc_temp+1
	sta gc_temp+1					; AND the possible directions with the desired directions
	beq ghost_change_random			; if there are no matches, pick a random direction

	lda gc_temp+1
	sta actor_directions, Y			; otherwise, use the selected direction
	jmp ghost_change_dir_done

ghost_change_random:
-	lda $a2							; read jiffy timer
	and #%00000011					; just the last 2 bits (0-3)
	tax
	lda ghost_chg_transform, x		; get the new direction
	cmp actor_directions,Y			; compare it to the current direction
	beq -							; if its the same, do it again
	sta actor_directions,y			; store the new direction

ghost_change_dir_done:

	lda #$00						; restore scatter mode values
	sta gc_rows+1
	sta gc_rows+4
	sta gc_cols+1
	sta gc_cols+3
	lda #$12
	sta gc_rows+2
	sta gc_rows+3
	lda #$27
	sta gc_cols+2
	sta gc_cols+4
	rts	

ghost_chg_transform:
	.byte 1,2,4,8

gc_temp:
	.byte $00, $00
gc_rows:
	.byte $00, $00, $12, $12, $00
gc_cols:
	.byte $00, $00, $27, $00, $27



; ====================================================================
check_position_matrix:
; ====================================================================
	lda current_actor        		; Load sprite number
	beq +
	asl                   			; Multiply current_actor by 2 to get the correct index
+	tay                   			; Transfer the result to Y

	lda actor_position_matrix, y	; get lo and hi byte of current position matrix
	sta $C4
	iny
	lda actor_position_matrix, y
	sta $c5

	ldy #$00
	lda ($c4), y					; get the actual value in the position matrix
	rts

; ====================================================================
update_actor_col:
; Given a sprite number, calculates the X axis cursor position of sprite
; ====================================================================
	lda current_actor		; Load sprite number
	beq +
	asl                   	; Multiply current_actor by 2 to get the correct index for X and Y
+	tay                   	; Transfer the result to y

	lda BASE_SPRITE_X, y  	; Load sprite X-coordinate
	sta acol_sprite_x		; Store sprite X-coordinate
	lda SPRITE_XMSB        	; Load sprite X-coordinate MSB
    ldx current_actor       ; Load sprite number into X
	cpx #$00
	beq +
-	lsr                   	; Shift the bits in the SPRITE_XMSB register to the right
	dex                   	; Decrement X
    bne - 					; If X is not 0, continue shifting
+	and #$01               	; Perform a bitwise AND with 1 to isolate the sprite MSB
    sta acol_sprite_msb2   	; Store sprite MSB
	beq +					; jump ahead if the msb is zero
	lda acol_sprite_x		; get the sprite x value
	cmp #$18
	bcs +					; jump ahead if it is >= 24
	lda #$00				; put 0 in the msb since the subtraction
	sta acol_sprite_msb2	; ..that will happen ahead would cause it to become 0
+	lda acol_sprite_x
	sec         
	sbc #$18				; subtract 24 from value for sprite screen offset
	lsr						; divide by 8
	lsr
	lsr
	ldy current_actor
	sta actor_cols, y		; save the column value
	lda acol_sprite_msb2	; if the X MSB is not zero, add 32 to the column result
	beq +
	lda actor_cols, y
	clc
	adc #$20
	sta actor_cols, y
+	rts

acol_sprite_x:
	.byte $00
acol_sprite_msb2:
	.byte $00

; ====================================================================
update_actor_row:
; Given a sprite number, calculates the Y axis cursor position of sprite
; ====================================================================
	lda current_actor
	ASL
	TAY
	LDA BASE_SPRITE_Y,Y
	SEC
	SBC #$32			; subtract 20 for offscreen offset
	LSR					; divide by 8
	LSR
	LSR
	ldy current_actor
	sta actor_rows, y		; put final value in actor col table
	RTS


; ====================================================================
sprite_move_right:
; ====================================================================

	; if we are already moving, keep doing so
	ldy current_actor
	lda actor_bytes_moved,y
	bne pmr_move

	; check position matrix
	jsr check_position_matrix

	; check the right movement bit
	and #%00000100
	bne pmr_ok2move

	; nope, can not move that way
;	jsr ghost_change_dir			; if ghost, change direction

	ldy current_actor				; if player, restore prev direction
	bne +
	lda player_current_direction
	sta actor_directions,y
+	jmp pmr_done					

pmr_ok2move:
	lda current_actor
	beq +
	asl                   			; Multiply current_actor by 2 to get the correct index
+	tay                   			; Transfer the result to Y
	lda actor_position_matrix, y	; add one to the actors position matrix address
	clc
	adc #$01
	sta actor_position_matrix, y
	iny
	lda actor_position_matrix, y
	adc #$00
	sta actor_position_matrix, y
	
	ldy current_actor				; reset the byte movement counter
	lda #$08
	sta actor_bytes_moved,y

	ldy current_actor				; if player, check if dot eaten
	bne pmr_move
	jsr sprite_to_screen_address
	jsr check_if_player_eat_dot

pmr_move:
	; move the sprite
	lda current_actor       ; Load sprite number
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
	jsr update_actor_col
	;jsr update_actor_row

pmr_done:
	rts

; ====================================================================
sprite_move_left:
; ====================================================================

	; if we are already moving, keep doing so
	ldy current_actor
	lda actor_bytes_moved,y
	bne pml_move

	; check position matrix
	jsr check_position_matrix

	; check the right movement bit
	lsr								; move the left bit to the carry flag
	lsr
	lsr
	lsr
	bcs pml_ok2move

	; nope, can not move that way
;	jsr ghost_change_dir			; if ghost, change direction
	ldy current_actor				; if player, restore prev direction
	bne +
	lda player_current_direction
	sta actor_directions,y
+	jmp pmr_done	

pml_ok2move:
	lda current_actor
	beq +
	asl                   			; Multiply current_actor by 2 to get the correct index
+	tay                   			; Transfer the result to Y
	lda actor_position_matrix, y	; subtract one to the actors position matrix address
	sec
	sbc #$01
	sta actor_position_matrix, y
	iny
	lda actor_position_matrix, y
	sbc #$00
	sta actor_position_matrix, y

	ldy current_actor				; reset movement counter
	lda #$08
	sta actor_bytes_moved,Y

	ldy current_actor				; if player, check if dot eaten
	bne pml_move
	jsr sprite_to_screen_address
	jsr check_if_player_eat_dot
	
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
	jsr update_actor_col
	;jsr update_actor_row

pml_done:
	rts

; ====================================================================
sprite_move_up:
; ====================================================================

	; if we are already moving, keep doing so
	ldy current_actor
	lda actor_bytes_moved,y
	bne pmu_move

	; check position matrix
	jsr check_position_matrix
	tax								; stash it

	; are we in the top tunnel?
	cmp #PM_BTM
	bne +

	jsr up_tunnel
	rts

	; check the up movement bit
+	lsr								; move the left bit to the carry flag
	bcs pmu_ok2move

	; nope, can not move that way
pmu_nope:
;	jsr ghost_change_dir			; if ghost, change direction
	ldy current_actor				; if player, restore prev direction
	bne +
	lda player_current_direction
	sta actor_directions,y
+;	jsr ghost_change_dir 			; if ghost, change direction
	jmp pmr_done	

pmu_ok2move:
	ldy current_actor				
	bne pmu_ok2move2				; if current actor is a ghost, skip ahead
	txa								; current actor is player
	asl								; check the ghost only bit
	bcc pmu_ok2move2				; if carry is clear, then its a normal position
	jmp pmu_nope					; else its a ghost position, player cant move

pmu_ok2move2:
	lda current_actor
	beq +
	asl                   			; Multiply current_actor by 2 to get the correct index
+	tay                   			; Transfer the result to Y
	lda actor_position_matrix, y	; subtract 37 to the actors position matrix address
	sec
	sbc #$25
	sta actor_position_matrix, y
	iny
	lda actor_position_matrix, y
	sbc #$00
	sta actor_position_matrix, y

	ldy current_actor				; reset movement counter
	lda #$0a
	sta actor_bytes_moved,Y

	ldy current_actor				; if player, check if dot eaten
	bne pmu_move
	jsr sprite_to_screen_address
	jsr check_if_player_eat_dot

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
	;jsr update_actor_col
	jsr update_actor_row

pmu_done:
	rts

; ====================================================================
up_tunnel:
; ====================================================================
	; in top tunnel.  move sprite to bottom and adjust pointers
	lda current_actor
	beq +
	asl                   			; Multiply current_actor by 2 to get the correct index
+	tay                   			; Transfer the result to Y
	lda #<(postion_matrix+37*17+18)
	sta actor_position_matrix, y 
	iny
	lda #>(postion_matrix+37*17+18)
	sta actor_position_matrix, y

	ldy current_actor				; reset movement counter
	lda #$0a
	sta actor_bytes_moved,Y

	lda current_actor        	; Load sprite number
	asl                   		; Multiply current_actor by 2 to get the correct index for X and Y
	tay                   		; Transfer the result to Y
	lda #$d8					; bottom tunnel
	sta BASE_SPRITE_Y, Y  		; Load sprite Y-coordinate

	ldy current_actor
	lda actor_bytes_moved,Y
	sec
	sbc #$01
	sta actor_bytes_moved,Y

	jsr update_actor_row
	rts

; ====================================================================
sprite_move_down:
; ====================================================================

	; if we are already moving, keep doing so
	ldy current_actor
	lda actor_bytes_moved,y
	bne pmd_move

	; check position matrix
	jsr check_position_matrix

	; are we in the bottom tunnel?
	cmp #PM_TOP
	bne +

	jsr down_tunnel
	rts

	; check the up movement bit
+	lsr								; move the left bit to the carry flag
	lsr
	bcs pmd_ok2move

	; nope, can not move that way
;	jsr ghost_change_dir			; if ghost, change direction
	ldy current_actor				; if player, restore prev direction
	bne +
	lda player_current_direction
	sta actor_directions,y
+	jmp pmr_done	

pmd_ok2move:
	lda current_actor
	beq +
	asl                   			; Multiply current_actor by 2 to get the correct index
+	tay                   			; Transfer the result to Y
	lda actor_position_matrix, y	; add 37 to the actors position matrix address
	clc
	adc #$25
	sta actor_position_matrix, y
	iny
	lda actor_position_matrix, y
	adc #$00
	sta actor_position_matrix, y

	ldy current_actor				; reset movement counter
	lda #$0a
	sta actor_bytes_moved,y

	ldy current_actor				; if player, check if dot eaten
	bne pmd_move
	jsr sprite_to_screen_address
	jsr check_if_player_eat_dot

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
	;jsr update_actor_col
	jsr update_actor_row

pmd_done:
	rts

; ====================================================================
down_tunnel:
; ====================================================================
	; in bottom tunnel.  move sprite to top and adjust pointers
	lda current_actor
	beq +
	asl                   			; Multiply current_actor by 2 to get the correct index
+	tay                   			; Transfer the result to Y
	lda #<(postion_matrix+37*1+18)
	sta actor_position_matrix, y 
	iny
	lda #>(postion_matrix+37*1+18)
	sta actor_position_matrix, y

	ldy current_actor				; reset movement counter
	lda #$0a
	sta actor_bytes_moved,Y

	lda current_actor        	; Load sprite number
	asl                   		; Multiply current_actor by 2 to get the correct index for X and Y
	tay                   		; Transfer the result to Y
	lda #$24					; top tunnel
	sta BASE_SPRITE_Y, Y  		; Load sprite Y-coordinate
	jsr update_actor_row

	ldy current_actor
	lda actor_bytes_moved,Y
	sec
	sbc #$01
	sta actor_bytes_moved,Y

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
delay_timer:
;=====================================================================
	pha
-	inc delay_timer_data
	lda delay_timer_data
	cmp #$ff
	bne -
	inc delay_timer_data+1
	lda delay_timer_data+1
	cmp #$20
	bne -
	lda #00
	sta delay_timer_data
	sta delay_timer_data+1
	pla
	rts

delay_timer_data:
.byte $00, $00


; ====================================================================
play_reset_sound:
; ====================================================================
	jsr SID_Clear_Registers
	
	lda #$08
	sta SID_PWHI1
	lda #$00
	sta SID_PWLO1

	lda #$0F
	sta SID_ATDCY1
	lda #$00
	sta SID_SUREL1

	lda #15			; set volume
	sta SID_VOL

	ldy #$00
prs_data_loop:
	lda sound_data_reset,Y
	beq prs_end 
	sta SID_FREHI1
	iny
	lda sound_data_reset,Y
	sta SID_FRELO1
	lda #$41
	sta SID_VCREG1
	jsr delay_timer
	lda #$40
	sta SID_VCREG1
	jsr delay_timer
	iny
	jmp prs_data_loop
prs_end:
	rts

sound_data_reset:
	.byte 32,177,36,214
	.byte 32,177,33,155
	.byte $00

; ====================================================================
play_die_sound:
; ====================================================================
	jsr SID_Clear_Registers
	
	lda #$08
	sta SID_PWHI1
	lda #$00
	sta SID_PWLO1

	lda #$0F
	sta SID_ATDCY1
	lda #$00
	sta SID_SUREL1

	lda #15			; set volume
	sta SID_VOL

	ldy #$00
pds_data_loop:
	lda sound_data_die,Y
	beq prs_end 
	sta SID_FREHI1
	iny
	lda sound_data_die,Y
	sta SID_FRELO1
	lda #$41
	sta SID_VCREG1
	jsr delay_timer
	lda #$40
	sta SID_VCREG1
	jsr delay_timer
	iny
	jmp pds_data_loop
pds_end:
	rts

sound_data_die:
	.byte 41,177,43,214
	.byte 45,177,47,155
	.byte $00

; ====================================================================
play_chomp_sound:
; ====================================================================
	lda #$01
	sta irq_sound_playing

	jsr SID_Clear_Registers
	
	lda #$08
	sta SID_PWHI1
	lda #$00
	sta SID_PWLO1

	lda #$0F
	sta SID_ATDCY1
	lda #$00
	sta SID_SUREL1

	lda #15			; set volume
	sta SID_VOL

	lda #10
	sta SID_FREHI1
	lda #255
	sta SID_FRELO1
	lda #$41
	sta SID_VCREG1
	jsr delay_timer
	lda #$40
	sta SID_VCREG1
	jsr delay_timer

	lda #$00
	sta irq_sound_playing
	rts

irq_sound_playing:
	.byte $00


SID_Clear_Registers:
	lda #$00			; fill with zero
	ldy #24
-	sta SID_FRELO1,y 	; store zero in sid chip address
	dey					; for next lower byte
	bpl -   			; fill 25 bytes
	rts

*= $3800
.include "screen_data.asm"
