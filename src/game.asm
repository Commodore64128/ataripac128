

PELLET1 = $64
PELLET2 = $64

BLACK	= $00
WHITE	= $01
RED 	= $02
CYAN 	= $03
PURPLE	= $04
GREEN	= $05
BLUE 	= $06
YELLOW 	= $07
BROWN 	= $08
L_RED	= $0a
D_GRAY	= $0b
M_GRAY	= $0C
L_GREEN	= $0D
L_BLUE	= $0E
L_GRAY	= $0F

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

SPRITE_0_X_POSITION 	= $D000
SPRITE_0_Y_POSITION 	= $D001
SPRITE_XMSB				= $D010
VIC_SCROLY				= $D011
VIC_RASTER				= $D012
SPRITE_ENBL				= $D015
VIC_SCREEN_BASE_REG		= $D018
VIC_IRQ_REG				= $D019
VIC_SPR_FG_COL_REG		= $D01F
VIC_BORDER_COLOR		= $D020
VIC_BACKGROUND_COLOR	= $D021
SPRITE_0_COLOR      	= $D027
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

; 10 SYS 7182
*=$1c01
	.byte $1c, $1c, $0A, $00, $9E, $20, $37, $31, $38, $32
    .byte $00, $00, $00

; ====================================================================
; main startup
; ====================================================================
main:

	lda #$00
	sta MMUCR				; bank 15

	jsr screen_init
	jsr chars_init
	jsr sprite_init
	jsr irq_init


main_loop:
	lda VIC_SPR_FG_COL_REG
	sta collision_flg

	; check joystick
	lda IO_PORT_DATA_REG_A
	sta joy_cache
	jsr joystick_handler

main_collision_check:
	lda collision_flg
	lsr a ;Sprite 0 crash into background
    bcs collide
	;inc VIC_BACKGROUND_COLOR

main_loop_end:
	jmp main_loop

collide:
	;inc VIC_BORDER_COLOR
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
	.byte DIR_RIGHT
joy_cache:
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

	lda #$00
	sta MMUCR				; bank 15

  	lda #56					; use default spr 0 pointer location (56x64=3584=$0e00)
  	sta SPRITE_0_POINTER
	lda #$01              	; enable...Sprite 0 => %0000 0001 (all sprites off except Sprite 0)
  	sta SPRITE_ENBL
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

movement_r0:
	.byte $23, 2, DIR_RIGHT, DIR_DOWN

pacman_dir:
	.byte $00	; 0-stopped, 1-up, 2-down, 3-right, 4-left
ghost1_dir:
	.byte $00
ghost2_dir:
	.byte $00
ghost3_dir:
	.byte $00
ghost4_dir:
	.byte $00



chars_init:

	sei					; best to disable interrupts
	
	lda #$00  			; bank 15
    sta MMUCR			; allows access to VIC chip
	
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
	lda #$01  			; bank 14
    sta MMUCR			; allow access to char rom @ $d800

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
;irq:
;	lda $d019
;	and #$01
;	beq no
;	lda $d011
;	bpl no
;	jsr irq_handler
;end: 
;	jmp $ff33
;no:
;	jmp $fa65


irq_handler:

	; decrement overall game delay
	;lda delay_game
	;cmp #$90
	;beq +
	;dec delay_game
	;jmp irq_end

;+
;	lda #$ff
;	sta delay_game
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
	sec 
	lda SPRITE_0_Y_POSITION
	sbc #$01
	sta SPRITE_0_Y_POSITION
+	lda player_direction
	cmp #DIR_DOWN
	bne +
	clc 
	lda SPRITE_0_Y_POSITION
	adc	#$01
	sta SPRITE_0_Y_POSITION
+	lda player_direction
	cmp #DIR_RIGHT
	bne +
	clc 
	lda SPRITE_0_X_POSITION
	adc #$01
	sta SPRITE_0_X_POSITION
+	lda player_direction
	cmp #DIR_LEFT
	bne +
	sec 
	lda SPRITE_0_X_POSITION
	sbc #$01
	sta SPRITE_0_X_POSITION
+
irq_end
	jmp $fa65

tempmmucr:
	.byte $00

joystick_handler:

	lda joy_cache
	and #$08
	bne +
	; move right
	lda #DIR_RIGHT
	sta player_direction

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

	jmp joystick_end
+	lda joy_cache
	and #$04
	bne +
	; move left
	lda #DIR_LEFT
	sta player_direction

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
	lda #$00
	sta joy_cache
	rts

.include "screen_data.asm"
.include "sprite_data.asm"