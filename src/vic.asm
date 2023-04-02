; ===============================================================
;          ***      PAC-MAN for the Commodore 128    *****
;        *** ***        (ATARI 2600 version)       *** * ***
;        ***                                       *********
;         *****        Scott Hutter / xlar54       * * * * *
; ===============================================================

; VIC-II Constants

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

BASE_SPRITE_X = $D000
BASE_SPRITE_Y = $D001

SPRITE_0_X_POSITION 	= $D000
SPRITE_0_Y_POSITION 	= $D001
SPRITE_1_X_POSITION 	= $D002
SPRITE_1_Y_POSITION 	= $D003
SPRITE_2_X_POSITION 	= $D004
SPRITE_2_Y_POSITION 	= $D005
SPRITE_3_X_POSITION 	= $D006
SPRITE_3_Y_POSITION 	= $D007
SPRITE_4_X_POSITION 	= $D008
SPRITE_4_Y_POSITION 	= $D009
SPRITE_5_X_POSITION 	= $D00A
SPRITE_5_Y_POSITION 	= $D00B
SPRITE_6_X_POSITION 	= $D00C
SPRITE_6_Y_POSITION 	= $D00D
SPRITE_7_X_POSITION 	= $D00E
SPRITE_7_Y_POSITION 	= $D00F
SPRITE_XMSB				= $D010
VIC_SCROLY				= $D011
VIC_RASTER				= $D012
VIC_SPRITE_ENBL			= $D015
VIC_SCREEN_BASE_REG		= $D018
VIC_IRQ_REG				= $D019
VIC_SPR_FG_COL_REG		= $D01F
VIC_BORDER_COLOR		= $D020
VIC_BACKGROUND_COLOR	= $D021
SPRITE_0_COLOR      	= $D027
SPRITE_1_COLOR      	= $D028
SPRITE_2_COLOR      	= $D029
SPRITE_3_COLOR      	= $D02A
SPRITE_4_COLOR      	= $D02B
SPRITE_5_COLOR      	= $D02C
SPRITE_6_COLOR      	= $D02D
SPRITE_7_COLOR      	= $D02E