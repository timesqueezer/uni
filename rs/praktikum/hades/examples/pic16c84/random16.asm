; 16 bit random LFSR generator test from Numerical Recipes
;
; (C) 1999 F.N.H
;
; 15.09.99 - first attempt


        list    P = 16C84,n=77                  ; ignored by MPASM GRRR!
	include <p16c84.inc>




_RandomHi	EQU	0x10
_RandomLo	EQU	0x11


_ResetVector		EQU		0x0000
_InterruptVector	EQU		0x0004
_PMEM_END		EQU		0x03FF	; 1 KByte ROM in Pic16C84







;
;******************************************************************************
;      Start program here, Power-On Reset occurred.
;******************************************************************************
;





;
;
; Reset: 
; On the 16C74, we should Determine whether power-up or other reset.
; The 16C84, however, does not allow this: always branch to Start.
;
	org	_ResetVector
Reset
	goto	Start
	goto	OtherReset



	
;
; This is the Periperal Interrupt routine. Need to determine the type
; of interrupt that occurred. The following interrupts are enabled:
;   1.  PORTB Change (RBIF)
;   2.  TMR1 Overflow Interrupt (T1IF)
;

	org	_InterruptVector
Interrupt
	goto	Interrupt





Start
                call    GlobalInit
RandomLoop

                call    NextRandom16
                call    OutputOnPortB
                goto    RandomLoop
                






;
; global inits
;
GlobalInit
		clrf	STATUS	
		clrf	INTCON		; disable all interrupts

		bsf	STATUS, RP0	; select bank 1
                movlw   b'00000000'
                movwf   TRISB

                movlw   b'11100000'
                movwf   TRISA

		bcf	STATUS, RP0	; select bank 0

		call	InitRandom16
                return


;
; InitRandom16:
; put a non-zero seed into the _RandomHi and _RandomLo registers, e.g. 0x7341
;
InitRandom16
                movlw   0x73
                movwf   _RandomHi
                movlw   0x41
                movwf   _RandomLo
                return


_RandomMaskHi           EQU     b'00000000'
_RandomMaskLo           EQU     b'00000001'


;
; NextRandom16:
; calculate a 16 bit random number in _RandomHi and _RandomLo
; using a LFSR-algorithm with polynome (15 1 0).
; For this polynome, the MASK is simply '1'. See Numerical Recipes.
;
NextRandom16
                btfss   _RandomHi,6     ; the '16384 bit' (for 16bit data)
		goto	ShiftLeft16

ShiftXorLeft16
                movlw   _RandomMaskLo
                xorwf   _RandomLo, F

                movlw   _RandomMaskHi
                xorwf   _RandomHi, F

                bsf     STATUS,C        ; set C flag
                rlf     _RandomLo,F
                rlf     _RandomHi,F
                return


ShiftLeft16
                bcf     STATUS,C        ; reset C flag
                rlf     _RandomLo,F
                rlf     _RandomHi,F
                return



OutputOnPortB
                movf    _RandomLo,W
                movwf   PORTB

                movf    _RandomHi,W
                movwf   PORTA
                return




;
; ErrorLoop:
; we should never arrive here. If debug flag set, we toggle the debug output
; pin to indicate that something is wrong.
;
ErrorLoop
	goto	ErrorLoop












;
; OtherReset:
; we arrive here from the ResetVector, if the reset was not a power-on-reset.
; For the 16C84, we should never arrive here...
;
OtherReset
		goto	ErrorLoop




;
; safety check: if the program arrives here it was lost...
;
	org	_PMEM_END
	goto	ErrorLoop

    end
