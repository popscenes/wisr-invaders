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
RowState4        .byte ;$87

xRowBase0	.byte
xRowBase1	.byte
xRowBase2	.byte
xRowBase3	.byte

xP1RowBase0	.byte
xP1RowBase1	.byte
xP1RowBase2	.byte
xP1RowBase3	.byte

xposRowCalc	.byte
xposP1RowCalc	.byte

XPos        .byte
YPos        .byte

MissileYpos .byte
MissileXpos .byte

Missile1Ypos .byte
Missile1Xpos .byte

currentRow  .byte
missileRow  .byte
missileColumn .byte

P1SpritePtr .word
P2SpritePtr .word




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg code
    ORG $f000

START:
    CLEAN_START    
    
    lda #110
    sta YRowPos0
    
    lda #140
    sta YRowPos0+1

    lda #170
    sta YRowPos0+2

    lda #200
    sta YRowPos0+3

    lda #$8
    sta COLUBK
    
    lda #0
    sta currentRow
    sta MissileXpos
    
    lda #36
    sta xRowBase0
    sta xRowBase1
    sta xRowBase2
    sta xRowBase3


    lda #84
    sta xP1RowBase0
    sta xP1RowBase1
    sta xP1RowBase2
    sta xP1RowBase3
    
    lda #30
    sta YPos

    lda %00111111
    sta RowState0
    sta RowState1
    sta RowState2
    sta RowState4 

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

    lda #<PinkOWLF0
    sta P1SpritePtr         ; lo-byte pointer for jet sprite lookup table
    lda #>PinkOWLF0
    sta P1SpritePtr+1       ; hi-byte pointer for jet sprite lookup table

    lda #<PinkOWLF0
    sta P2SpritePtr         ; lo-byte pointer for jet sprite lookup table
    lda #>PinkOWLF0
    sta P2SpritePtr+1       ; hi-byte pointer for jet sprite lookup table

    
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
    
    jsr SetUpRow
    ldy #9
    
InSpriteRow    	
    jsr DrawMissiles
    LDA (P1SpritePtr),Y             ; 4
    STA GRP0                        ; 3
    LDA (P2SpritePtr)),y            ; 4
    STA GRP1                        ; 3
    LDA PinkOwlCF0,y                ; 4
    STA COLUP0                      ; 3
    STA COLUP1                      ; 3
    STA WSYNC
    dey                             ; 2
    bne InSpriteRow                  ; 2 ;Total - 27 clocks;

    lda #0                          ; 2
    STA GRP1                        ; 3
    STA GRP0                        ; 3
    dec currentRow                  ; 5
    bpl waitForRow                  ; 2
    
    
    lda #0		
    sta NUSIZ0
    sta NUSIZ1
    
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
    ldx #0
.findRow
    lda YRowPos0,x
    sbc #16             ;hit at bottom
    SEC
    sbc MissileYpos
    Bpl .collisionOnRow
    inx
    jmp .findRow
.collisionOnRow
    stx missileRow
    lda #%00000001
    sta missileColumn
    lda MissileXpos
    Sbc xRowBase0,x
columnLoop    
    ASL missileColumn
    sec
    sbc #16		; subtract 15
	bcs columnLoop	; branch until negative
    
    LSR missileColumn
    
    lda RowState0,x
    EOR missileColumn
    sta RowState0,x

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
        cmp #5		; within 5 lines of missile?
        lda #3		; bit 1 now set
        adc #0		; if carry set, bit 1 cleared
        sta ENABL	; enable/disable ball
.missile2
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

SetUpRow
    lda #<PinkOWLF0
    sta P1SpritePtr         ; lo-byte pointer for jet sprite lookup table
    lda #>PinkOWLF0
    sta P1SpritePtr+1       ; hi-byte pointer for jet sprite lookup table

    lda #<PinkOWLF0
    sta P2SpritePtr         ; lo-byte pointer for jet sprite lookup table
    lda #>PinkOWLF0
    sta P2SpritePtr+1       ; hi-byte pointer for jet sprite lookup table
    
    lda #0
    sta ENABL	; enable/disable ball
    lda RowState0,x
    and #%00000111
    BNE .skipEmptySpriteP1
    lda #<EmptyFrame
    sta P1SpritePtr         ; lo-byte pointer for jet sprite lookup table
    lda #>EmptyFrame
    sta P1SpritePtr+1       ; hi-byte pointer for jet sprite lookup table   
.skipEmptySpriteP1   
    tay
    lda RowXNUSIZ,y
    sta NUSIZ0 

    lda RowXOffset,y
    clc
    ADC xRowBase0,x
    sta xposRowCalc

    lda RowState0,x
    LSR
    LSR
    LSR
    and #%00000111
    BNE .skipEmptySpriteP2
    lda #<EmptyFrame
    sta P2SpritePtr         ; lo-byte pointer for jet sprite lookup table
    lda #>EmptyFrame
    sta P2SpritePtr+1       ; hi-byte pointer for jet sprite lookup table
.skipEmptySpriteP2
    tay
    lda RowXNUSIZ,y
    sta NUSIZ1 

    lda RowXOffset,y
    clc
    ADC xP1RowBase0,x
    sta xposP1RowCalc
    lda xposRowCalc
    ldx #0
    jsr SetHorizPos
    lda xposP1RowCalc
    ldx #1
    jsr SetHorizPos
    
    lda MissileXpos
    ldx #4
    jsr SetHorizPos

    sta WSYNC
    sta HMOVE	; gotta apply HMOVE
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


RowXOffset
        .byte #0    ; Formation - 000
        .byte #0    ; Formation - 100
        .byte #15   ; Formation - 010
        .byte #0    ; Formation - 110
        .byte #32   ; Formation - 001
        .byte #0    ; Formation - 101
        .byte #16   ; Formation - 011
        .byte #0    ; Formation - 111

RowXNUSIZ
        .byte #0    ; Formation - 000
        .byte #0    ; Formation - 100
        .byte #0    ; Formation - 010
        .byte #1    ; Formation - 110
        .byte #0    ; Formation - 001
        .byte #2    ; Formation - 101
        .byte #1    ; Formation - 011
        .byte #3    ; Formation - 111

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

EmptyFrame
        .byte 0
        .byte 0
        .byte 0
        .byte 0
        .byte 0
        .byte 0
        .byte 0
        .byte 0
        .byte 0
        
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
