    LIST	C=150, N=999, P=16C84
    TITLE 	"MFOOT REVXXXXs44"
    ERRORLEVEL  0, -305, -302, -224
    __CONFIG    _CP_OFF & _WDT_OFF & _XT_OSC

;************************************************************************
;
;   Midi Footswitcher
;       Using PIC16C54
;           Copyright 1994, 1998
;               David Sorlien
;		    For non-commercial use only
;
;************************************************************************
;
;   Program flow:
;                   Initialize
;                       |
;                   Get Channel #
;                       |
;       -------->   Scan Switches   <--------------------------------
;       |               |                        |                  |
;       |           Key Press?  - no ---->  Refresh Display         |
;       |               |                                           |
;       |               yes                                         |
;       |               |                                           |
;       |           Bank Up/Dn? - yes --->  Process Up/Dn -----------
;       |               |
;       |               no
;       |               |
;       |           Process 0-9 (send MIDI)
;       |               |
;       -----------------
;
;***********************************************************************
;    INCLUDE         "P16C5X.INC"
  INCLUDE "P16C84.INC"

;************************************************************************
; Ram Used

;   cblock	0x08  not a good choice for the 16C84... FNH
    cblock      0x16
    rNEWKEY
    rCOMMAND
    rFLAGS
    rLEDones
    rLEDtens
    rLEDhunds
    rLEDMux
    rTEMP1
    rTEMP2
    rTEMP3
    rTEMP4
    rTEMP5
    endc
;************************************************************************
; rFlag bit assignments

bNewK           equ     0
bOnesEn         equ     1
bRange          equ     3

;************************************************************************
; IO pins

bOUT            equ     0

;************************************************************************
; Constants
kOptionDefault  equ     B'11111111'

kPATrisDefault  equ     B'11110000'
kPAPinsDefault  equ     B'11111111'
kPALEDsOff      equ     B'11111111'

kPBPinsDefault  equ     B'01111111'
kPBTrisDefault  equ     B'10000000'
kPBSwitchTris   equ     B'11110000'

kLEDMuxStart    equ     B'11111101'

;************************************************************************
;  Construction note:
;   Wire the 12 momentary foot switches as a 3 by 4 matrix.
;   The constants below define the function of each switch.
;   Once circuitry is working, shuffle constant names around
;   until all switches function correctly.

k9	equ	B'11101110'	; PB4 - PB0 switch closed
k8	equ	B'11101101'	; PB4 - PB1 switch closed
k7	equ	B'11101011'	; PB4 - PB2 switch closed
k6	equ	B'11100111'	; PB4 - PB3 switch closed
k3	equ	B'11011110'	; PB5 - PB0 switch closed
k2	equ	B'11011101'	; PB5 - PB1 switch closed
k1	equ	B'11011011'	; PB5 - PB2 switch closed
k0	equ	B'11010111'	; PB5 - PB3 switch closed
kDN	equ	B'10111110'	; PB6 - PB0 switch closed
kUP	equ	B'10111101'	; PB6 - PB1 switch closed
k5	equ	B'10111011'	; PB6 - PB2 switch closed
k4	equ	B'10110111'	; PB6 - PB3 switch closed

;************************************************************************
;************************************************************************
    ORG         0X01FF
    goto        initialize
    ORG         0
    goto        initialize

;************************************************************************

ledMuxDelay
    nop
    retlw       0

ledMuxDelay_slow
    movlw       .6
    movwf       rTEMP3
    clrf        rTEMP4
_lmLoop
    decfsz      rTEMP4
    goto        _lmLoop
    decfsz      rTEMP3
    goto        _lmLoop
    retlw       0

;************************************************************************
scanKP
    movlw       kPALEDsOff      ;
    movwf       PORTA           ; make sure LEDs are off
    movlw       kPBSwitchTris   ;
;    tris        PORTB           ; set portB directions  
    movwf	TRISB           ; FNH


    clrf        PORTB           ; discharge matrix
    movlw       B'11110111'     ; setup first column read
    movwf       rTEMP1          ;
_scan01
    movfw       rTEMP1          ;
    movwf       PORTB           ; Sets PB bits 3/2/1/0 in succession
    nop                         ;  pause to let pins settle
    nop
    nop
    nop
    nop
    nop
    movfw       PORTB           ; read the port
    movwf       rTEMP2          ; save it for possible use later
    andlw       B'01110000'     ; strip garbage - W holds 0x70 if no press
    xorlw       B'01110000'     ; result will be 0 if no switch pressed
    btfss       STATUS,Z        ;
    goto        switchPress     ; a switch was pressed
    bsf         STATUS,C        ;
    rrf         rTEMP1          ; prepare for next column read
    btfsc       STATUS,C        ; check if done
    goto        _scan01         ; not done yet
    clrf        rNEWKEY         ; no switch was detected, so
    decf        rNEWKEY         ;  rNEWKEY = 0xFF
    goto        scanCleanup     ;

switchPress
    incf        rNEWKEY,W       ; this code makes sure that one switch
    btfss       STATUS,Z        ;  press does not result in extra prog change messages:
    goto        scanCleanup     ;  - rNEWKEY must be 0xFF before a new press is decoded
                                ;    and this will not happen until the scanKP routine
                                ;    detects no switches down.

    bsf         rTEMP2,7        ; ignore pin 7 of PortB

    movlw       k0              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x00            ; "0" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       k1              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x01            ; "1" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       k2              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x02            ; "2" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       k3              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x03            ; "3" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       k4              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x04            ; "4" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       k5              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x05            ; "5" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       k6              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x06            ; "6" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       k7              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x07            ; "7" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       k8              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x08            ; "8" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       k9              ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x09            ; "9" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       kUP             ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x80            ; "UP" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    movlw       kDN             ; raw key data mask
    subwf       rTEMP2,W        ; test for match
    movlw       0x81            ; "DOWN" SWITCH
    skpnz                       ;
    movwf       rNEWKEY         ;

    bsf         rFLAGS,bNewK    ; signal a switch just pressed

scanCleanup
    movlw       kPBPinsDefault  ;
    movwf       PORTB           ;
    movlw       kPBTrisDefault  ;
;    tris        PORTB           ; set PORTB to defaults
    movwf       TRISB            ; FNH


    goto        scanKPDone      ; done

;************************************************************************
getLEDPattern
    addwf       PCL             ; do computed jump
    retlw       B'11000000'     ; pattern for 0
    retlw       B'11111001'     ; pattern for 1
    retlw       B'10100100'     ; pattern for 2
    retlw       B'10110000'     ; pattern for 3
    retlw       B'10011001'     ; pattern for 4
    retlw       B'10010010'     ; pattern for 5
    retlw       B'10000010'     ; pattern for 6
    retlw       B'11111000'     ; pattern for 7
    retlw       B'10000000'     ; pattern for 8
    retlw       B'10011000'     ; pattern for 9

;************************************************************************
doLEDMux
    movlw       0               ; clear W for safe lookup table access
    btfss       rLEDMux,3       ;
    movfw       rLEDones        ; if(rLEDMUX,3 == 0){W = rLEDONES}
    btfss       rLEDMux,2       ;
    movfw       rLEDtens        ; if(rLEDMUX,2 == 0){W = rLEDTENS}
    btfss       rLEDMux,1       ;
    movfw       rLEDhunds       ; if(rLEDMUX,1 == 0){W = rLEDHUNDS}
    call        getLEDPattern   ; now lookup the segment pattern

    btfsc       rLEDMux,1       ; if rLEDMUX,1 == 0
    goto        _doMux00        ;
    btfss       rLEDhunds,0     ;  and rLEDHUNDS == 0
    movlw       0xFF            ;   blank the leading zero

_doMux00
    btfsc       rLEDMux,3       ; if rLEDMUX,3 == 0
    goto        _doLMux01       ;
    btfss       rFLAGS,bOnesEn  ;  and bank up/dn was last keypress
    movlw       B'10111111'     ;   put a "-" on ones digit

_doLMux01
    movwf       PORTB           ; put segment pattern on port_B
    movfw       rLEDMux         ;
    movwf       PORTA           ; turn on a transistor
    bsf         STATUS,C        ;
    rlf         rLEDMux         ; shift mux pattern to prepare for next call
    movlw       kLEDMuxStart    ;
    btfss       rLEDMux,4       ;
    movwf       rLEDMux         ; if(rLEDMUX,4 == 0){rLEDMUX = kLEDMuxStart}

    call        ledMuxDelay     ; pause for a a while

    movlw       kPAPinsDefault  ;
    movwf       PORTA           ; turn off transistor
    movlw       kPBPinsDefault  ;
    movwf       PORTB           ; reset portB to defaults
    retlw       0

;************************************************************************
initialize
    movlw       kOptionDefault  ;
;    option
    bsf         STATUS, RP0 
    movwf       OPTION_REG         ; FNH
    bcf         STATUS, RP0 

    movlw       kPBPinsDefault  ;
    movwf       PORTB           ;

    movlw       kPBTrisDefault  ;
;    tris        PORTB           ; set PORTB to defaults
    bsf         STATUS, RP0
    movwf       TRISB            ; FNH
    bcf         STATUS, RP0


    movlw       kPAPinsDefault  ;
    movwf       PORTA           ;

    movlw       kPATrisDefault  ;
;    tris        PORTA           ; set PORTA to defaults
    bsf         STATUS, RP0
    movwf       TRISA            ; FNH
    bcf         STATUS, RP0

    clrf        rNEWKEY         ; set some initial conditions
    decf        rNEWKEY         ;
    clrf        rFLAGS          ;
    movlw       kLEDMuxStart    ;
    movwf       rLEDMux         ;
    clrf        rLEDones        ;
    clrf        rLEDtens        ;
    clrf        rLEDhunds       ;

;************************************************************************
; Read the 4-position DIP switch
getChannel
    movlw       0xC0            ; rCOMMAND holds default MIDI command byte
    movwf       rCOMMAND        ; defaults to program change, MIDI channel 1
    movlw       kPALEDsOff      ;
    movwf       PORTA           ; make sure portA is off

    movlw       kPBSwitchTris   ;
;    tris        PORTB           ; set new portB directions
    bsf         STATUS, RP0
    movwf       TRISB            ; FNH
    bcf         STATUS, RP0


    movlw       B'11111110'     ;
    movwf       PORTB           ; look at first dip switch
    movlw       B'11111101'     ; pause 1uS and setup for next switch read
    btfss       PORTB,7         ; was dip switch 1 closed?
    bsf         rCOMMAND,0      ; set bit if so

    movwf       PORTB           ; look at second dip switch
    movlw       B'11111011'     ; pause 1uS and setup for next switch read
    btfss       PORTB,7         ; was dip switch 2 closed?
    bsf         rCOMMAND,1      ; set bit if so

    movwf       PORTB           ; look at third dip switch
    movlw       B'11110111'     ; pause 1uS and setup for next switch read
    btfss       PORTB,7         ; was dip switch 3 closed?
    bsf         rCOMMAND,2      ; set bit if so

    movwf       PORTB           ; look at fourth dip switch
    nop                         ; pause 1uS to let pins settle
    btfss       PORTB,7         ; was dip switch 4 closed?
    bsf         rCOMMAND,3      ; set bit if so

    movlw       kPBPinsDefault  ;
    movwf       PORTB           ;
    movlw       kPBTrisDefault  ;
;    tris        PORTB           ; reset PORTB to defaults
    bsf         STATUS, RP0
    movwf       TRISB            ; FNH
    bcf         STATUS, RP0



    movlw       kPAPinsDefault  ;
    movwf       PORTA           ;
    movlw       kPATrisDefault  ;
;    tris        PORTA           ; reset PORTA to defaults
    bsf         STATUS, RP0
    movwf       TRISA            ; FNH
    bcf         STATUS, RP0


;************************************************************************
; read range switch
                                ; now, check if range switch is closed
    bcf         rFLAGS,bRange   ;
    clrf        TMR0            ;
    nop				;
    bcf         PORTA,1         ; the hi to low transition will increment
    nop                         ;  the TMR0 counter
    bsf         PORTA,1         ; reset the pin to default

    movfw       TMR0            ;
    btfss       STATUS,Z        ; if TMR0 == 0, don't set bRange bit
    bsf         rFLAGS,bRange   ;

;************************************************************************
; end of initialization
;************************************************************************

;************************************************************************
; MAIN PROGRAM LOOP
;************************************************************************
mainLoop
    movlw       kOptionDefault  ; insure option register is correct
    option
    goto        scanKP          ; scan the switch matrix
scanKPDone
    btfsc       rFLAGS,bNewK    ; bNewK == 1 if switch press detected
    goto        handlePress     ;
    call        doLEDMux        ;
    call        doLEDMux        ;
    call        doLEDMux        ;
    goto        mainLoop

;************************************************************************
; A switch press was detected
handlePress
    bcf         rFLAGS,bNewK    ; signal that press was processed
    btfss       rNEWKEY,7       ; was UP or Down pressed?
    goto        calcMsg         ;  no, must be 0 to 9 switch

;************************************************************************
; Either the up or down switch was pressed
doUpDown
    bcf         rFLAGS,bOnesEn  ; will cause "-" to appear on ones digit
    btfss       rNEWKEY,0       ; was it Up or Down
    goto        doDown          ;  it was Down

doUp
    incf        rLEDtens        ; increment tens digit
    movlw       0x06            ; check for decimal carry
    addwf       rLEDtens,W      ;
    btfss       STATUS,DC       ;
    goto        _upNoCarry      ; jump forward if no decimal carry
    clrf        rLEDtens        ;
    incf        rLEDhunds       ; increment hunds digit
    goto        _upDnDone       ; done

_upNoCarry
    btfss       rLEDhunds,0     ; check for wrap around - is hunds==1?
    goto        _upDnDone       ;  no - bank is in range

    movlw       0x03            ; hunds was 1, now is tens equal to 3?
    subwf       rLEDtens,W      ;  do the test
    btfss       STATUS,Z        ; Z bit set if wrap aound
    goto        _upDnDone       ;  tens and hunds are fine - skip ahead
    clrf        rLEDtens        ; Wrap around to "00x"
    clrf        rLEDhunds       ;
    goto        _upDnDone       ; done

doDown
    decf        rLEDtens        ;
    btfss       rLEDtens,7      ; did underflow occur?
    goto        _upDnDone       ;  no

    movlw       .9              ;
    movwf       rLEDtens        ;

    decf        rLEDhunds       ;
    btfss       rLEDhunds,7     ; did underflow occur?
    goto        _upDnDone       ;  no

    movlw       0x01            ; underflow occurred
    movwf       rLEDhunds       ;  so set hunds to 1
    movlw       0x02            ;   and
    movwf       rLEDtens        ;    set tens to 2

_upDnDone
    goto        mainLoop        ;

;************************************************************************
; A 0-9 switch press was detected
;   Here is the stuff that assembles the program change number
;
calcMsg
    movfw       rNEWKEY         ;
    movwf       rLEDones        ; update the LED stuff
    bsf         rFLAGS,bOnesEn  ; now don't display the "-"

    movfw       rLEDtens        ; get the tens digit
    movwf       rTEMP2          ;  put it in accumulator
    bcf         STATUS,C        ;
    rlf         rTEMP2          ;
    rlf         rTEMP2          ;
    rlf         rTEMP2          ;
    rlf         rLEDtens,W      ;
    addwf       rTEMP2          ; now rTEMP2 holds 10 x rLEDtens
    movfw       rLEDones        ;
    addwf       rTEMP2          ; add the ones digit
    movlw       .100            ;
    btfsc       rLEDhunds,0     ; should add 100 to result?
    addwf       rTEMP2          ; yes

				; rTEMP2 could now hold 0 to 129
    btfsc	rFLAGS,bRange	;
    goto	_adjustRng	;

				; range sw open, adjust prog# and ones digit
    movlw	.127		;
    btfss       rTEMP2,7        ; is prog # out of range?
    goto	sendMIDI        ;  no
    movwf	rTEMP2		; out of range, limit to 127
    movlw	7		;
    movwf	rLEDones	; the ones digit was out of range - correct it
    goto	sendMIDI	;

_adjustRng                      ; range sw closed, adjust prog# and ones digit
    movfw	rTEMP2		; is prog # selected == 0?
    skpz			;
    goto	_adjRngNZ	;
    incf	rLEDones	; keep display from showing "00" in this mode
    goto	sendMIDI	;

_adjRngNZ			;
    decf	rTEMP2		; adjust real program change #
    movlw	.127		;
    btfss       rTEMP2,7        ; is prog # out of range?
    goto	sendMIDI        ;  no
    movwf	rTEMP2		; out of range, limit real prog# to 127
    movlw	8		;
    movwf	rLEDones	; and set display to read 128

;************************************************************************
;   Last but not least, send the MIDI byte
;
sendMIDI
    movfw       rCOMMAND        ; make copy of command byte
    movwf       rTEMP1          ;
    movlw       0xFF            ; get ready to assemble data stream
    movwf       rTEMP3          ;

    				; to make midi output routine easier,
				;  add start and stop bits
    				;  by shifting these bits in one at a time

       				;  rTEMP3    rTemp2    rTemp1
    bcf         STATUS,C        ;  11111111  PPPPPPPP  CCCCCCCC
    rlf         rTEMP2          ;
    rlf         rTEMP3          ;  1111111P  PPPPPPP0  CCCCCCCC
    bsf         STATUS,C        ;
    rlf         rTEMP2          ;
    rlf         rTEMP3          ;  111111PP  PPPPPP01  CCCCCCCC
    bcf         STATUS,C        ;
    rlf         rTEMP1          ;
    rlf         rTEMP2          ;
    rlf         rTEMP3          ;  11111PPP  PPPPP01C  CCCCCCC0

    movlw       .20             ; 20 bits to send (in rTEMP3:2:1)
    movwf       rTEMP4          ;

_cbloop                         ; (32 uS per bit)
    movlw       6               ; setup delay loop for start bit
    movwf       rTEMP5          ;

    rrf         rTEMP3          ; rotate next bit to send into carry
    rrf         rTEMP2          ;
    rrf         rTEMP1          ;

    btfsc       STATUS,C        ;
    goto        _cbONE          ;
_cbZERO
    nop                         ;
    bcf         PORTA,bOUT      ;
    goto        _cbdly          ;
_cbONE
    bsf         PORTA,bOUT      ;
    goto        _cbdly          ;

_cbdly
    decfsz      rTEMP5          ;
    goto        _cbdly          ; delay for 17 inst
s    nop
    decfsz      rTEMP4          ; more bits to send?
    goto        _cbloop         ; yes
    goto        mainLoop        ; last bit is always 1

    END


