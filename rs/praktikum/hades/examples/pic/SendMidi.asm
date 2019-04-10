    LIST	C=150, N=999, P=16C84
;   LIST       C=150, N=999, P=15C54
    TITLE 	"SendMidiTest"
    ERRORLEVEL  0, -305, -302, -224
    __CONFIG    _CP_OFF & _WDT_OFF & _XT_OSC

;************************************************************************
;
;   Test Midi Send Routine for HADES/PIC16C84
;   FNH, 01.12.98
;
;   Midi Footswitcher
;       Using PIC16C54
;           Copyright 1994, 1998
;               David Sorlien
;		    For non-commercial use only
;
;************************************************************************
;
;   Initialize, Send NoteOn, Wait, Send NoteOff, Wait, Send NoteOn, ....
;
;   increment note numbers (40 ... 80 ... 40) after each send
;   output MIDI commands on Pin A0
;
;***********************************************************************

  INCLUDE "P16C84.INC"

;************************************************************************
; Ram Used


    cblock      0x10
    rChannel 
    rWaitCounter
    rNote

    rCOMMAND
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

kWaitDelay      equ     0x40
kChannel1       equ     0x0C
kNoteOn         equ     0x90
kNoteOff        equ     0x80

kOptionDefault  equ     B'11111111'

kPATrisDefault  equ     B'11110000'
kPAPinsDefault  equ     B'11111111'

kPBPinsDefault  equ     B'01111111'
kPBTrisDefault  equ     B'10000000'
kPBSwitchTris   equ     B'11110000'



;************************************************************************


    ORG         0X01FF
    goto        initialize
    ORG         0
    goto        initialize

;************************************************************************
initialize
    movlw       kOptionDefault  ;
    bsf         STATUS, RP0 
    movwf       OPTION_REG        
    bcf         STATUS, RP0 

    movlw       kPBPinsDefault  ;
    movwf       PORTB           ;

    movlw       kPBTrisDefault  ;
    bsf         STATUS, RP0
    movwf       TRISB           
    bcf         STATUS, RP0


    movlw       kPAPinsDefault  ;
    movwf       PORTA           ;
    movlw       kPATrisDefault  ;
    bsf         STATUS, RP0
    movwf       TRISA            
    bcf         STATUS, RP0

    movlw       0x42
    movwf       rNote

    movlw       kChannel1
    movwf       rChannel

 


;************************************************************************
; MAIN PROGRAM LOOP
;************************************************************************

mainLoop

    movlw       kOptionDefault  ; insure option register is correct
    bsf         STATUS, RP0 
    movwf       OPTION_REG        
    bcf         STATUS, RP0 

    incf        rNote, 1

    call        sendNoteOn
    call        waitLoop
    call        sendNoteOff
    call        waitLoop

    goto        mainLoop


;************************************************************************

waitLoop
    movlw       kWaitDelay
    movwf       rWaitCounter
 
_waitLoop
    decfsz      rWaitCounter
    goto        _waitLoop
    retlw       0


waitLoop_slow
    movlw       .80
    movwf       rTEMP3
    clrf        rTEMP4
_lmLoop
    decfsz      rTEMP4
    goto        _lmLoop
    decfsz      rTEMP3
    goto        _lmLoop
    retlw       0


;************************************************************************

sendNoteOn
    movlw     kNoteOn
    addwf     rChannel, 0
    movwf     rCOMMAND

    movfw     rNote
    movwf     rTEMP2
    call      sendMIDI
    retlw     0


sendNoteOff
    movlw     kNoteOff
    addwf     rChannel, 0
    movwf     rCOMMAND

    movfw     rNote
    movwf     rTEMP2
    call      sendMIDI
    retlw     0


;************************************************************************

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

    retlw       1
  
    END


