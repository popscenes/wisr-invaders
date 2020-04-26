    PROCESSOR 6502

    include "vcs.h"
    INCLUDE "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

spriteHeight equ 9

    SEG.U Variables
    org $80

YPos    .byte
YPosp2  .byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg code
    ORG $f000

START:
    CLEAN_START

    lda #140
    sta YPos
    
    lda #140
    sta YPosp2

NextFrame:
    lsr SWCHB	; test Game Reset switch
    bcc START	; reset?
; 1 + 3 lines of VSYNC
    VERTICAL_SYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ldx #37
LVBlank
    sta WSYNC
    DEX
    bne LVBlank

    lda #0
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  192 scan lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ldx #192
LVScan
    TXA         ;x -> a
    SEC         ;set carry
    SBC YPos    ; a - YPos
    cmp #spriteHeight
    BCC InSpriteP1
    LDA #0

InSpriteP1    
    TAY
    LDA Frame0,y
    ;STA WSYNC
    STA GRP0
    LDA ColorFrame0,y
    STA COLUP0
    
    
    TXA         ;x -> a
    SEC         ;set carry
    SBC YPosp2    ; a - YPos
    cmp #spriteHeight
    BCC InSpriteP2
    LDA #0
    
InSpriteP2    
    TAY
    LDA PinkOWLF0,y
    STA GRP1
    LDA PinkOwlCF0,y
    STA COLUP1
    
    STA WSYNC
    DEX
    bne LVScan
    
    dec YPos
    dec YPosp2
    dec YPosp2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OVER SCAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ldx #29
LVOver	sta WSYNC
	dex
	bne LVOver
    jmp NextFrame


;---Graphics Data from PlayerPal 2600---
Frame0
        .byte 0
        .byte #%00011000;$02
        .byte #%00111100;$02
        .byte #%00111100;$02
        .byte #%01111110;$02
        .byte #%01011010;$02
        .byte #%01011010;$02
        .byte #%00111100;$B2
        .byte #%01111110;$B2

Player2F0
        .byte 0
        .byte #%00000000;$0E
        .byte #%00011000;$0E
        .byte #%00000000;$0E
        .byte #%10000001;$0E
        .byte #%00100100;$0E
        .byte #%00100100;$0E
        .byte #%00111100;$B6
        .byte #%01111110;$B6
        
PinkOWLF0
	.byte 0
        .byte #%00100100;$0E
        .byte #%10111101;$56
        .byte #%11111111;$56
        .byte #%01111110;$56
        .byte #%00111100;$0E
        .byte #%01010110;$0E
        .byte #%01111110;$0E
        .byte #%00100010;$F6
;---End Graphics Data---
;---Color Data from PlayerPal 2600---
ColorFrame0
        .byte 0
        .byte #$02;
        .byte #$02;
        .byte #$02;
        .byte #$02;
        .byte #$02;
        .byte #$02;
        .byte #$B2;
        .byte #$B2;

Player2ColorFrame0
        .byte 0
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$B6;
        .byte #$B6;
        
PinkOwlCF0
        .byte #$0E;
        .byte #$56;
        .byte #$56;
        .byte #$56;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$F6;
;---End Color Data---

    org $FFFC
    .word START
    .word START
