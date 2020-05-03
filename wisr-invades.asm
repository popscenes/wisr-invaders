        PROCESSOR 6502

    include "vcs.h"
    INCLUDE "macro.h"
    INCLUDE "xmacro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

spriteHeight equ 9

    SEG.U Variables
    org $80

YPos        .byte
XPos        .byte
YPosp2      .byte
MissileOn   .byte
MissileYpos .byte
MissileXpos .byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg code
    ORG $f000

START:
    CLEAN_START

    lda #30
    sta YPos

    lda #0
    sta XPos
    
    lda #140
    sta YPosp2

    lda #$8
    sta COLUBK

NextFrame:
    lsr SWCHB	; test Game Reset switch
    bcc START	; reset?
    VERTICAL_SYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    TIMER_SETUP 37
    lda XPos 
    ldx #0 
    jsr SetHorizPos
    lda MissileXpos 
    ldx #2 
    jsr SetHorizPos
    sta WSYNC
    sta HMOVE	; gotta apply HMOVE

    sta CXCLR ; clear collisions
    

    TIMER_WAIT

    lda #0
	sta VBLANK
.VisibleFrame    
    
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  192 scan lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ldx #95
LVScan
    lda #0
    cpx MissileYpos
    bne skipMissileDraw
    inc MissileYpos
    lda #%00000010
skipMissileDraw
    sta ENAM0
    
    TXA         ;x -> a
    SEC         ;set carry
    SBC YPos    ; a - YPos
    cmp #spriteHeight
    BCC InSpriteP1
    LDA #0

InSpriteP1    
    TAY
    LDA Frame0,y
    STA WSYNC
    STA GRP0
    LDA ColorFrame0,y
    STA COLUP0
    
    
    TXA                     ; load current scanline(x) -> a
    SEC                     ; set carry
    SBC YPosp2              ; a =  a - YPos
    cmp #spriteHeight       ; if a < spriteHeight cary flag cleared draw sprite line
    BCC InSpriteP2
    LDA #0
    
InSpriteP2    
    TAY
    LDA PinkOWLF0,y
    STA WSYNC
    STA GRP1
    LDA PinkOwlCF0,y
    STA COLUP1
    
    DEX
    bne LVScan

    
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OVER SCAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	TIMER_SETUP 29
	lda #2
    sta VBLANK
    jsr MoveJoystick
    dec YPosp2

CheckCollisionM0P1:
    lda #%10000000           ; CXP0FB bit 7 detects P0 and PF collision
    bit CXM0P                 ; check CXM0P register bit 7
    bne .CollisionM0P1       ; collision P0 with playfield happened
    jmp EndCollisionCheck
.CollisionM0P1:
    lda #32
    sta COLUBK
    lda #0
    sta MissileYpos          ; and we also reset missile position

EndCollisionCheck:
    sta CXCLR                ; clear all collision flags before next frame

    TIMER_WAIT
    jmp NextFrame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SetHorizPos - setHorizontal position of an object
; A register desired C-coordinate of teh object
; X register contains the index of the desired object:
;
; X=0: player 0
; X=1: player 1
; X=2: missile 0
; X=3: missile 1
; X=4: ball

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetHorizPos
	sta WSYNC	; start a new line
    bit 0		; waste 3 cycles
	sec		; set carry flag
DivideLoop
	sbc #15		; subtract 15
	bcs DivideLoop	; branch until negative
	eor #7		; calculate fine offset
	asl
	asl
	asl
	asl
	sta RESP0,x	; fix coarse position
	sta HMP0,x	; set fine offset
	rts		; return to caller


MoveJoystick
; Move vertically
; (up and down are actually reversed since ypos starts at bottom)
	ldx YPos
	lda #%00100000	;Up?
	bit SWCHA
	bne SkipMoveUp
    cpx #9
    bcc SkipMoveUp
    dex
SkipMoveUp
	lda #%00010000	;Down?
	bit SWCHA 
	bne SkipMoveDown
    cpx #80
    bcs SkipMoveDown
    inx
SkipMoveDown
	stx YPos
; Move horizontally
    ldx XPos
	lda #%01000000	;Left?
	bit SWCHA
	bne SkipMoveLeft
    cpx #1
    bcc SkipMoveLeft
    dex
SkipMoveLeft
	lda #%10000000	;Right?
	bit SWCHA 
	bne SkipMoveRight
    cpx #153
    bcs SkipMoveRight
    inx
SkipMoveRight
	stx XPos
	bit INPT4 
	bmi SkipButton
    lda #45
    lda YPos
    adc #8
    sta MissileYpos
    lda XPos
    adc #5
    sta MissileXpos
    
SkipButton
	rts

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
