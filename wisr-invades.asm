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

YRowPos0        .byte ;$80
YRowPos1        .byte ;$81
YRowPos3        .byte ;$82
YRowPos4        .byte ;$83

RowState0        .byte ;$84
RowState1        .byte ;$85
RowState2        .byte ;$86
RowState         .byte ;$87

xRowBase0	.byte
xRowBase1	.byte
xRowBase2	.byte
xRowBase3	.byte

XPos        .byte
YPos        .byte

MissileYpos .byte
MissileXpos .byte

Missile1Ypos .byte
Missile1Xpos .byte

OwlRowMask  .byte
currentRow  .byte


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg code
    ORG $f000

START:
    CLEAN_START    
    
    lda #120
    sta YRowPos0
    
    lda #140
    sta YRowPos0+1

    lda #160
    sta YRowPos0+2

    lda #180
    sta YRowPos0+3

    lda #$8
    sta COLUBK
    
    lda #0
    sta currentRow
    sta MissileXpos
    
    lda #10
    sta xRowBase0
    
    lda #20
    sta xRowBase1
    
    lda #30
    sta xRowBase2
    
    lda #40
    sta xRowBase3
    
    lda #30
    sta YPos

NextFrame:
    lsr SWCHB	; test Game Reset switch
    bcc START	; reset?
    VERTICAL_SYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    TIMER_SETUP 37
    
    sta CXCLR ; clear collisions
    
    lda #3
    sta currentRow
    jsr MoveMissiles
    


    TIMER_WAIT

    lda #0
    sta VBLANK
.VisibleFrame    
    
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  192 scan lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



		
    TIMER_SETUP 192
                                    ; clocks
VisibleScanlines
waitForRow
    jsr DrawMissiles
    ldx currentRow                  ; 3
    lda YRowPos0,x                  ; 4
    cmp INTIM                       ; 4
    bcc waitForRow                  ; 2
    
SetUpRow
    lda #3		; two copies, close for NUSIZ
    sta NUSIZ0
    sta NUSIZ1
    
    
    
    lda xRowBase0 ,x
    ldx #0
    jsr SetHorizPos
    lda #100
    ldx #1
    jsr SetHorizPos
    
    ;ta WSYNC
    ;ta HMOVE	; gotta apply HMOVE
    
    lda MissileXpos
    ldx #4
    jsr SetHorizPos
    sta WSYNC
    sta HMOVE	; gotta apply HMOVE

	;lda #0
        ;sta ENABL
    
    ldy 9     
InSpriteP2    

	
    LDA PinkOWLF0,y                 ; 4
    STA GRP1                        ; 3
    STA GRP0                        ; 3
    LDA PinkOwlCF0,y                ; 4
    STA COLUP0                      ; 3
    STA COLUP1                      ; 3
    STA WSYNC                       ;3


    dey                             ; 2
    bne InSpriteP2                  ; 2 ;Total - 27 clocks;
    
    lda #0                          ; 2
    STA COLUP0                      ; 3
    STA COLUP1                      ; 3
    STA GRP1                        ; 3
    STA GRP0                        ; 3
    dec currentRow                  ; 5
    bpl waitForRow                  ; 2
    
    
    lda #0		
    sta NUSIZ0
    sta NUSIZ1
    
    lda MissileXpos
    ldx #4
    jsr SetHorizPos
    sta WSYNC
    sta HMOVE	; gotta apply HMOVE
    
waitForPlayer    
    jsr DrawMissiles
    lda #30
    cmp INTIM
    bcc waitForPlayer
    
    
    
    
SetUpPlayer
    lda XPos 
    ldx #0
    jsr SetHorizPos
    sta WSYNC
    sta HMOVE	; gotta apply HMOVE

    ldy 9     
InSpriteP1    
    LDA Frame0,y                 ; 4
    STA GRP0                     ; 3
    LDA ColorFrame0,y            ; 4
    STA COLUP0                   ; 3
    STA WSYNC
    ldx #0
    
    dey                             ; 2
    bne InSpriteP1                  ; 2 ;Total - 27 clocks;
    lda #0                          ; 2
    STA COLUP0                      ; 3
    STA COLUP1                      ; 3
    STA GRP1                        ; 3
    STA GRP0
    

   TIMER_WAIT 
   

    
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OVER SCAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	TIMER_SETUP 29
	lda #2
    sta VBLANK
    jsr MoveJoystick

CheckCollisionM0P1:
    lda #%01000000           ; CXP0FB bit 7 detects P0 and PF collision
    bit CXP0FB                 ; check CXM0P register bit 7
    bne .CollisionM0P1       ; collision P0 with playfield happened
    
    lda #%01000000           ; CXP0FB bit 7 detects P0 and PF collision
    bit CXP1FB                 ; check CXM0P register bit 7
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
; A register desired x-coordinate of the object
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


DrawMissiles
	    lda INTIM	; load timer value
        pha
        sec
        sbc MissileYpos 
        cmp #8		; within 8 lines of missile?
        lda #3		; bit 1 now set
        adc #0		; if carry set, bit 1 cleared
        sta ENABL	; enable/disable ball
	pla
        sec
        sbc Missile1Ypos
        cmp #8		; within 8 lines of missile?
        lda #3		; bit 1 now set
        adc #0		; if carry set, bit 1 cleared
        sta ENAM1	; enable/disable missile
        rts


MoveMissiles
    lda MissileYpos
    beq NoMoveMiss0
    inc MissileYpos
NoMoveMiss0
    lda Missile1Ypos
    beq NoMoveMiss1
    dec Missile1Ypos
NoMoveMiss1
	rts





MoveJoystick

; Move horizontally
    ldx XPos
	lda #%01000000	;Left?
	bit SWCHA
	bne SkipMoveLeft
    cpx #30
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
