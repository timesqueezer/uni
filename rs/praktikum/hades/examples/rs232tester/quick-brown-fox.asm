    TITLE           "Quick-Brown-Fox: RS232 Tester: Half-Duplex"
    SUBTITLE        "based on Microchip AN510 and AN555"

    Processor       16C84
    Radix           DEC
    EXPAND
    include         "p16c84.inc"

;
; 
; A simple RS232 protocol tester that sends out a (fixed) sequence of 
; ASCII/Latin-1 chars to the _TXPin of the Pic at selectable baud rate.
; At 4MHz input clock, baud rates up to 38.400 are possible.
; Select the desired baud rate with pins PB2..0, and 1 or 2 stop bits with PB3.
;
; Also a demonstration for the Pic16C84/Pic16F84 microprocessor and VT52/VT100
; simulation models in the HADES simulation environment, see the HADES homepage
; at http://tech-www.informatik.uni-hamburg.de/applets/hades/
; 
; (C) 1999, F.N.Hendrich, hendrich@informatik.uni-hamburg.de
; based on AN510 and AN555 by Arizona Microchip
; 
; 03.09.99 - minor documentation cleanups
;


_ResetVector	set	0x00
_IntVector	set	0x04
_CharTable      set     0x60

_TXPort		equ 	PORTA
_TXPin          equ     4		; the RS232 output pin
_TXReg		equ	0x20		; the RS232 transmit register
_DelayConst	equ	0x21		; delay constant for selected baud rate
_DelayCntr	equ	0x22		; delay counter reg	
_CharPtr	equ	0x23		; index into the output char array
_BitCount	equ	0x24		; count tx bits 

;
; some predefined constants for the usual baud rates at 4MHz input clock.
; Use the following formula to calculate constants for other baud rates 
; (see AN510):
; _delayBaud  equ   xx   ; where 13+3*xx = _ClkOut / BaudRate
;                        ; that is,   xx = ((_ClkOut / BaudRate - 13) / 3) 
;
; The following values are for _ClkIn = 40000000;
;

_ClkIn        equ 4000000
_ClkOut       set (_ClkIn >> 2)

_delay00300   set ( ((_ClkOut /   300) -13) / 3)
_delay00600   set ( ((_ClkOut /   600) -13) / 3)
_delay01200   set ( ((_ClkOut /  1200) -13) / 3)
_delay02400   set ( ((_ClkOut /  2400) -13) / 3)
_delay04800   set ( ((_ClkOut /  4800) -13) / 3)
_delay09600   set ( ((_ClkOut /  9600) -13) / 3)
_delay19200   set ( ((_ClkOut / 19200) -13) / 3)

; too imprecise due to rounding, try to set manually:
;_delay38400   set ( ((_ClkOut / 38400) -13) / 3)

_delay38400	set 3

_DataBits       set     8               ; 8 bit data
_StopBits       set     1               ; 1 stop bit

                                        ; parity is NOT enabled by default,
                                        ; change _txmit routine if necessary




;*****************************************************************************
;*****************************************************************************
;*****************************************************************************
;
; code starts here
;

	ORG     _ResetVector
	goto    Start


;
; we should NEVER arrive here, because interrupts (and WD-timer) should 
; be disabled...
;

	ORG     _IntVector
        goto    Interrupt
                                        


Start:
	movlw	0x00
	movf	INTCON,1		; disable all interrupts

	call    InitPorts
	call    TransmitCharLoop	; this never returns...
	goto	Start



Interrupt:
	goto	Interrupt


;
; BaudDelayTable:
; return the delay constants for the baud rates 300, 600, 1200, 2400, 4800,
; 9600, 19200, 31250, 38400, ...
;
BaudDelayTable:
	addwf    PCL,F
	retlw	_delay00300
	retlw	_delay00600
	retlw	_delay01200
	retlw	_delay02400
	retlw	_delay04800
	retlw	_delay09600
	retlw	_delay19200
	retlw	_delay38400

	

;  
; InitPorts:
; initialize the _TXPin pin as output and all other ports of the Pic controller
; as inputs. Also, we set the _TXPin to the high state (RS232 inactive).
;
						
InitPorts:
	bsf	STATUS, RP0	; access port direction regs
	movlw	0xFF
	movwf	TRISA		; both ports input
	movwf	TRISB
	bcf	TRISA, _TXPin	; but _TXPin is output
	bcf	TRISA, 0	; debugging: enable Pin A0 as trigger output
	bcf	STATUS, RP0	; access data regs 
	return





;
; GetDesiredBaudRate:
; read the input pins portb<2:0> and decode the selected baud rate
; from 300 to 38400 bauds. The delay constant corresponding to the
; baud rate is put into the _DelayConst register.
; Set Portb<3> to 0 for 1 stop bits, to 1 for 2 stop bits.
;
GetDesiredBaudRate:
	movf	PORTB,0		; move PORTB to W
	andlw	0x07		; mask lower three bits
	call	BaudDelayTable
	movwf	_DelayConst
	return



;
; TransmitCharLoop:
; assuming that all baud rate constants and the serial port is initialized,
; this method enters an infinite loop that samples the baud rate selector
; inputs, then transmits the (fixed) output chars sequences at the selected
; baud rate.
;
;
TransmitCharLoop:

	call	GetDesiredBaudRate	; read and decode baud rate

	movlw	0
	movwf	_CharPtr		; initialize String index pointer

SendNextChar:
	call 	GetDesiredBaudRate

	incf	_CharPtr,F		; increment String index pointer
	movf	_CharPtr,W		; and move pointer to W
	call	QuickBrownFoxTable	; puts the next String char into W

	addlw	0			; add 0 to W (to set the zero flag)
	btfsc	STATUS,Z		; check ZERO flag
	goto	TransmitCharLoop	; if ZERO, next outer iteration 

	call	SendOneChar		; not ZERO, send the one char in W
        goto    SendNextChar



;
; SendOneChar:
; the actual RS232 transmission routine, half-duplex, no-flow-control.
; See AN510 for an explanation
;
SendOneChar:
	movwf	_TXReg			; move W (char to send) to _TXReg

	bsf	PORTA,0			; debug sync impulse on port A0
	bcf	PORTA,0

	movlw	0x08
	movwf	_BitCount		; send 8 bits

	bcf	_TXPort,_TXPin		; set _TXPin for start bit

	nop
	nop
	nop
	nop
	call	BitDelay

SendNextBit:
	bcf     STATUS,C
	rrf     _TXReg,1		; rotate TXReg

	btfsc   STATUS,C
	goto	_setTX

_clearTX:
	nop				; to get equal set/clear times
	bcf	_TXPort,_TXPin
	goto	_readyTX

_setTX:
	bsf	_TXPort,_TXPin
	goto	_readyTX
	
_readyTX:
	call    BitDelay		
	decfsz  _BitCount,1		; decrement bit counter (8..0)
	goto    SendNextBit

	nop
	nop
	nop
	nop
	nop

	bsf     _TXPort,_TXPin		; send stop bit
	call    BitDelay		; always 1 stop bit

	btfsc	PORTB,3			; send second stop bit?
	call 	BitDelay		; yes

	return



BitDelay:
	movf	_DelayConst,0		; move baud delay constant to W
	movwf	_DelayCntr 		; initialize delay counter

DelayLoop:
	decfsz	_DelayCntr,1		; decrement delay counter
	goto	DelayLoop
	return


;
; the array with the fixed transmission String.
; Currently, we use the standard English and German demo strings,
; "the quick brown fox jumps over the lazy dog"
; "Victor jagt zw÷lf Boxk„mpfer ’ber den grožen Sylter Deich"
;
; If you know of other demo strings, please drop me an e-mail (see file header)
;



         org _CharTable

QuickBrownFoxTable:
        addwf PCL,F
        retlw ' ' 
        retlw 'T'
        retlw 'h'
        retlw 'e'
        retlw ' '
        retlw 'q'
        retlw 'u'
        retlw 'i'
        retlw 'c'
        retlw 'k'
        retlw ' '
        retlw 'b'
        retlw 'r'
        retlw 'o'
        retlw 'w'
        retlw 'n'
        retlw ' '
        retlw 'f'
        retlw 'o'
        retlw 'x'
        retlw ' '
        retlw 'j'
        retlw 'u'
        retlw 'm'
        retlw 'p'
        retlw 's'
        retlw ' '
        retlw 'o'
        retlw 'v'
        retlw 'e'
        retlw 'r'
        retlw ' '
        retlw 't'
        retlw 'h'
        retlw 'e'
        retlw ' '
        retlw 'l'
        retlw 'a'
        retlw 'z'
        retlw 'y'
        retlw ' '
        retlw 'd'
        retlw 'o'
        retlw 'g'
        retlw '.'
        retlw ' '


SyltTable:
;        addwf PCL,F
        retlw 'V'
        retlw 'i'
        retlw 'c'
        retlw 't'
        retlw 'o'
        retlw 'r'
        retlw ' '
        retlw 'j'
        retlw 'a'
        retlw 'g'
        retlw 't'
        retlw ' '
        retlw 'z'
        retlw 'w'
        retlw 0xf6  ; ö
        retlw 'l'
        retlw 'f'
        retlw ' '
        retlw 'B'
        retlw 'o' 
        retlw 'x' 
        retlw 'k' 
        retlw 0xe4  ; ä 
        retlw 'm' 
        retlw 'p' 
        retlw 'f' 
        retlw 'e' 
        retlw 'r' 
        retlw ' ' 
        retlw 0xfc   ; ü
        retlw 'b'
        retlw 'e'
        retlw 'r'
        retlw ' ' 
        retlw 'd'
        retlw 'e'
        retlw 'n'
        retlw ' ' 
        retlw 'g'
        retlw 'r'
        retlw 'o'
        retlw 0xdf   ; ß
        retlw 'e'
        retlw 'n' 
        retlw ' '
        retlw 'S'
        retlw 'y' 
        retlw 'l' 
        retlw 't' 
        retlw 'e'
        retlw 'r' 
        retlw ' '
        retlw 'D'
        retlw 'e'
        retlw 'i'
        retlw 'c'
        retlw 'h' 
        retlw '.'
        retlw ' ' 
        retlw 0


	END
