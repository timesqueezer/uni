
; 16 bit increment/add test from AN617
;
; (C) 1999 F.N.H
;
; 15.09.99 - first attempt


        list    P = 16C84,n=77                  ; ignored by MPASM GRRR!
	include <p16c84.inc>




_ALo            EQU     0x10
_AHi            EQU     0x11

_BLo            EQU     0x12
_BHi            EQU     0x13


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
IncrementLoop

                call    Increment16
                call    OutputOnPortB
                goto    IncrementLoop
                






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

                movlw   0x01
                movwf   _ALo
                movlw   0x00
                movwf   _AHi

                movlw   0x07
                movwf   _BLo
                movwf   _BHi

                return



; add B(H,L) = A(H,L) + B(H,L)
;
Increment16
                movf    _ALo,W
                addwf   _BLo,F

                movf    _AHi,W
                btfsc   STATUS,C
                incfsz  _AHi,W
                addwf   _BHi,F

                return


OutputOnPortB
                movf    _BLo,W
                movwf   PORTB

                movf    _BHi,W
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
