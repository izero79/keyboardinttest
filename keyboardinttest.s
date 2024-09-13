    INCDIR "include:"
    INCLUDE "include/hardware/intbits.i"
    INCLUDE "include/hardware/cia.i"

KEYBOARD_WAIT	EQU	61
TIMERB_LO	    EQU	KEYBOARD_WAIT&$ff
TIMERB_HI	    EQU	KEYBOARD_WAIT>>8

IRQ2           EQU $68

CHIPBASE        EQU $dffe00

; CIA registers
CIAA		equ	$bfe001
CIAPRA		equ	$000
CIATALO		equ	$400
CIATAHI		equ	$500
CIATBLO		equ	$600
CIATBHI		equ	$700
CIAICR		equ	$d00
CIACRA		equ	$e00
CIACRB		equ	$f00

CIAATALO        EQU $bfe401     ;Timer A low
CIAATAHI        EQU $bfe501     ;Timer A high
CIAAICR         EQU $bfed01     ;Interrupt control register
CIAACRA         EQU $bfee01     ;Timer A control
CIAASDR         EQU $BFEC01

; Custom chip registers
CUSTOM		equ	$dff000
INTREQR		equ	$01e
INTENAR		equ	$01c
DMACON		equ	$096
INTENA		equ	$09a
INTREQ		equ	$09c

DMASET2  EQU     %1000001111100000 ; set dma for blitter, sprite, audio, disk, copper and bitplanes
KEY_NONE	= $00			; No key pressed
KEY_NEW		= $01			; New key pressed
KEY_REPEAT	= $02			; Key repeated

WaitDisk	EQU	30	; 50-150 to rescue (as appropriate)

	SECTION	Start,CODE

    include "DaWorkBench.s"
	include "Startup2.s"


START:

	JMP	Init

Init:
	BSR.W	SetINT		; Setup keyboard reading

    LEA     BITPLANE,A0    ; Load the address of the reserved memory into A0
    MOVE.L  #142080,D0         ; Set the counter to the number of bytes (142080)
    CLR.B   D1                 ; Clear D1 (D1 will hold the value to be written, which is 0)

.clear_loop:
    MOVE.B  D1,(A0)+           ; Store 0 at the current address and increment A0
    SUBQ.L  #1,D0              ; Decrease the byte counter (D0)
    BNE     .clear_loop         ; If D0 is not zero, loop again

    BSR.W   InitBitlplanes  

    BRA.W   MainLoop


; Set own keyboard handler to use
SetINT:

	move.l	4.w,a6		; Execbase in a6
    lea	CIAA,a2
    move.b	#$7f,CIAICR(a2)		;disable all CIA-A ints
    move.b	#TIMERB_LO,CIATBLO(a2)
    move.b	#TIMERB_HI,CIATBHI(a2)
    move.b	#$18,CIACRB(a2)		;one-shot mode and load
	move.l	#0,a0               ;for debugging
    move.l  vbrBase,a0          ;for debugging
    move.l  IRQ2(a0),a0         ;for debugging    
    lea     KeyboardInterrupt,a0;for debugging
	move.l	#0,a0               ;for debugging
    move.l	vbrBase(a6),a0
    move.l  #KeyboardInterrupt,IRQ2(a0)

    move.w	#INTF_SETCLR|INTF_INTEN|INTF_PORTS,INTENA(a5)    move.b	#$88,CIAICR(a2)		;enable CIAA SP interrupt


SetupKeyboard:
    move.b    #142,(CIAATALO)                                        ; init timer-a (~200 µs)
    sf        (CIAATAHI)
    move.b    #$7f,(CIAAICR)                                         ; allow interrupts from the keyboard & timer-a
    move.b    #(CIAICRF_SETCLR|CIAICRF_SP|CIAICRF_TA),(CIAAICR)
    tst.b     (CIAAICR)                                              ; clear all ciaa-interrupt requests
    and.b     #~(CIACRAF_SPMODE),(CIAACRA)                           ; set input mode
    move      #INTF_PORTS,(CHIPBASE+INTREQ)                          ; clear ports interrupt	
    rts

bufferwritepointer:
    DC.B    0
bufferreadpointer:
    DC.B    0
buffer:
	DCB.B	256,0






; Keyboard INT handler
KeyboardInterrupt:
    movem.l     d0-d2/a2,-(a7)
    btst       #INTB_PORTS,(CHIPBASE+INTREQR+1)                                        ; check if keyboard has caused interrupt
    beq.b      .end    
    move.b     CIAAICR,d0                                                              ; timer-a
    btst       #CIAICRB_TA,d0
    beq        .cont	    
    sf         CIAACRA                                                                 ; set input mode (handshake end)
    bra.b      .end
.cont:		
    btst       #CIAICRB_SP,d0
    beq.b      .end
    move.b     CIAASDR,d0                                                              ; read keycode
    move.b     #CIACRAF_SPMODE|CIACRAF_LOAD|CIACRAF_RUNMODE|CIACRAF_START,(CIAACRA)    ; set output mode (handshake start)	
    not.b      d0
    ror.b      #1,d0                                                                   ; calculate rawkeycode
; check the key and save it into buffer
    bpl.b	.Down			        ; IF Key was released
    cmp.b	KeyRaw,d0		        ; IF old key == new key 
    bne.b   .New
    move.b	#KEY_REPEAT,KeyState	;   KeyState = KEY_NONE
    ;move.b	d0,KeyRaw		        ;   KeyRaw = new key with bit 7 set!
    bra.b	.end			        ; ENDIF
.Down:
    cmp.b	KeyRaw,d0		        ; IF old key == new key 
    bne.b	.New
    move.b	#KEY_REPEAT,KeyState	;   KeyState = KEY_REPEAT
    bra.b	.end			        ; ENDIF
.New:
    MOVE.B	D0,KeyRaw		        ; KeyRaw = new key
    MOVEQ   #0,D2
    move.b  bufferwritepointer,D2
    LEA     buffer,A2
    MOVE.B  D0,(A2,D2)
    ADD.B   #1,bufferwritepointer
    MOVE.B	#KEY_NEW,KeyState	    ; KeyState = KEY_NEW

.end:
    move.w     #INTF_PORTS,(INTREQ+CHIPBASE)
    tst.w      (INTREQR+CHIPBASE)  ; to avoid timing problems on very fast machines we do another custom register access
    movem.l     (a7)+,d0-d2/a2
    rte

MainLoop:
    lea CIAA,a4          ; ciaa for keyboard
    lea CUSTOM,A5          ; vhposr (006)

    BSR.W   GetKeyFromBuffer


.esc
    CMP.B   #$45,d0             ; ESC
    BEQ.S   .end
.loop:
	MOVE.L	$dff004,d0			; Wait for Vertical Blank
	AND.L	#$1ff00,d0
	CMP.L	WBLANKLINE,d0
	BNE.S	.loop				; loop until VB

    BRA.W   MainLoop
.end:
    RTS		; End the mainloop


; Read key from buffer
GetKeyFromBuffer:
    MOVE.L  A2,-(SP)
    MOVE.L  D3,-(SP)
    MOVEQ   #0,D3
    LEA     buffer,A2
    MOVE.B  bufferreadpointer,D3
    MOVE.B  (A2,D3),D0
    CMP.B   #0,D0
    BEQ.S   .end
    MOVE.B  #0,(A2,D3)
    ADD.B   #1,bufferreadpointer
.end:
    MOVE.L  (SP)+,D3
    MOVE.L  (SP)+,A2
    RTS

KeyRaw:		dc.b	0		; Raw code of last pressed key
KeyState:	dc.b	0 		; State of last pressed key

vbrBase:	rs.l	1

WBLANKLINE:
    DC.l    303<<8 ; PAL

;	Data

OldCop:			; Here goes the address of the old system COP
	dc.l	0



InitBitlplanes:


	MOVE.L	#BITPLANE,d0		; in d0 we put the address of the PIC,
						; that is, where the first bitplane begins

	LEA	BPLPOINTERS,A1	; in a1 we put the address of the
						; pointers to COPPERLIST planes
	MOVEQ	#4,D1		; number of bitplanes -1 (here it's 5-1)
						; to loop with the DBRA
POINTBP:
	move.w	d0,6(a1)	; copies the LOW word of the plane address
						; in the right word in the copperlist
	swap	d0			; swap the 2 words of d0 (e.g.: 1234 > 3412)
						; putting the word HIGH in place of that
						; LOW, allowing it to be copied with move.w!!
	move.w	d0,2(a1)	; copies the HIGH word of the plane address
						; in the right word in the copperlist
	swap	d0			; swap the 2 words of d0 (ex: 3412 > 1234)
						; resetting the address.
	ADD.L	#48,d0	    ; Length of one line of bitplane in bytes, here it is 384/8 = 48

	addq.w	#8,a1		; add 8 to address, a1 now contains the address of the next
						; bplpointers in the copperlist to write.
	dbra	d1,POINTBP	; Redo D1 times POINTBP (D1=num of bitplanes (-1))

    MOVE.W  #DMASET2,$96(a5)     ; DMACON - Enable Bitplane, Copper DMA

.StartCopper:
	move.l	#COPPERLIST,$dff080	; We set our copperlist
	move.w	d0,$dff088		    ; let's start the copper

	move.w	#0,$dff1fc		    ; FMODE - Turn off the AGA
	move.w	#$c00,$dff106		; BPLCON3 - Turn off the AGA
	MOVE.W	#$11,$10c(a5)		; Turn off the AGA
    RTS

    
	SECTION	GRAPHIC,DATA_C

COPPERLIST:

; We make the sprites point to ZERO, to eliminate them

	dc.w	$120,$0000,$122,$0000,$124,$0000,$126,$0000,$128,$0000
	dc.w	$12a,$0000,$12c,$0000,$12e,$0000,$130,$0000,$132,$0000
	dc.w	$134,$0000,$136,$0000,$138,$0000,$13a,$0000,$13c,$0000
	dc.w	$13e,$0000

	dc.w	$8e,$2c81	; DiwStrt	(logs with normal values)
	dc.w	$90,$2cc1	; DiwStop
	dc.w	$92	; DdfStart
    dc.b    $00
DDFST:
    dc.b    $18
	dc.w	$94,$00d0	; DdfStop
	dc.w	$102		; BplCon1
    dc.b    $00
MIOCON1:
    dc.b    $00
	dc.w	$104,0		; BplCon2
	dc.w	$108		; Bpl1Mod
MOD1:
    dc.w    192         ; 384/4=48, 48*(5-1) = 192
    dc.w    $10a
MOD2:
	dc.w	192		; Bpl2Mod  ; 384/4=48, 48*(5-1) = 192

; the BPLCON0 ($dff100) For a 5 bitplanes screen: (32 colors)

		        ; 5432109876543210
	DC.W	$100,%0101001000000000	; bits 14 and 12 on!! (5 = %101)

; We make bitplanes stake directly by putting them in the copperlist
; the registers $dff0e0 and following below with the addresses
; of the bitplanes which will be set by the POINTBP routine

BPLPOINTERS:
	dc.w $e0,$0000,$e2,$0000	;first   bitplane - BPL0PT
	dc.w $e4,$0000,$e6,$0000	;second  bitplane - BPL1PT
	dc.w $e8,$0000,$ea,$0000	;third   bitplane - BPL2PT
	DC.W $ec,$0000,$ee,$0000	;fourth	 bitplane - BPL3PT
	DC.W $f0,$0000,$f2,$0000	;fifth	 bitplane - BPL4PT

; Palette

    DC.W	$0180,$0058
    DC.W	$0182,$0FFF

; Enter any WAIT effects here

	dc.w	$FFFF,$FFFE	; END OF THE COPPERLIST

	EVEN


BITPLANE:
	DS.B    142080,0

	end
