; ===============================================================
;          ***      PAC-MAN for the Commodore 128    *****
;        *** ***        (ATARI 2600 version)       *** * ***
;        ***                                       *********
;         *****        Scott Hutter / xlar54       * * * * *
; ===============================================================

; SID Chip Constants
SID_BASE    = $d400
SID_FRELO1  = $d400     ; voice 1 frequency control (low byte)
SID_FREHI1  = $d401     ; voice 1 frequency control (high byte)
SID_PWLO1   = $d402     ; Pulse width control
SID_PWHI1   = $d403     ; Pulse width control
SID_VCREG1  = $d404     ; voice 1 control register
SID_ATDCY1  = $d405     ; voice 1 attack / decay register
SID_SUREL1  = $d406     ; voice 1 sustain/release register

SID_VOL     = $d418     ; SID volume register



SID_JIFFLO  = 162       ; low byte of jiffy clock
