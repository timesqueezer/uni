	list p=16c84
;
;  This program demonstrates the PIC16C84.
;  To demo the EEPROM capability of the PIC16C84, then
;  a value is incrementally written and verified in all 
;  64 locations of the EEPROM. If an error occurs at any location,
;  then all leds on portb will blink at a 1 sec rate.
;
	#include <P16C84.INC>
;
TIME      equ     0x10
GPFLAG    equ     0x20            ;Define flag register
EEPERROR  equ     0               ;GPFLAG,0
;
	org     0
	goto    Start
;
	org     4
	goto    ServiceRtcc
;
	org     10
Start
	clrf    GPFLAG          ;clr all flags
	movlw   B'00000000'     ;make port b outputs
	movwf   PORTB
        bsf     STATUS, RP0
	clrf    TRISB           ;       /
        bcf     STATUS, RP0
Next
	call    WriteAll        ;write to all locations
	call    CheckAll        ;verify all locations
	btfsc   GPFLAG,EEPERROR ;no error then skip
	goto    BlinkLeds       ;else blink leds.
	incf    PORTB, F        ;inc value in port b
	goto    Next
;
WriteAll
	clrf    EEADR           ;start at addr = 0
	incf    PORTB,W         ;read current value+1
	movwf   EEDATA          ;ld. data reg.
	bsf     STATUS,RP0      ;select pg 1
	bsf     EECON1,WREN     ;enable write operation
	bcf     STATUS,RP0      ;select pg 0
WA1
	call    WriteOne        ;write to a location
	incf    EEADR, F        ;inc address
	btfss   EEADR,6         ;all 64 done?
	goto    WA1             ;no then do next
	bsf     STATUS,RP0      ;pg 1
	bcf     EECON1,WREN     ;disable write
	bcf     STATUS,RP0      ;pg 0
	return
;
WriteOne
	bsf     STATUS,RP0      ;page 1
	movlw   0x55            ;do write seq.
	movwf   EECON2          ;       /
	movlw   0xaa            ;      / 
	movwf   EECON2          ;     /  
	bsf     EECON1,WR       ;initiate write
WO1     
	btfsc   EECON1,WR       ;write complete?
	goto    WO1             ;no then keep checking
	bcf     STATUS,RP0      ;pg 0
	return
;
CheckAll
	clrf    EEADR           ;start at addr = 0
	incf    PORTB,W         ;ld. data to inspect
CA1
	call    CheckOne        ;check location
	btfsc   GPFLAG,EEPERROR ;any error?
	return                  ;yes then quit
	incf    EEADR, F        ;inc address
	btfss   EEADR,6         ;all 64 checked?
	goto    CA1             ;no then do next
	return
;
CheckOne
	bsf     STATUS,RP0      ;pg 1
	bsf     EECON1,RD       ;do a read
CO1
	btfsc   EECON1,RD       ;rd done?
	goto    CO1             ;no then loop
	bcf     STATUS,RP0      ;pg 0
	xorwf   EEDATA, F       ;compare?
	btfss   STATUS,Z        ;same then skip
	bsf     GPFLAG,EEPERROR ;set error flag
	return
;
BlinkLeds
	call    InitRtcc
	clrf    PORTB           ;turn off leds
BL1
	call    SecondOver      ;wait for 1/2 second
	comf    PORTB, F        ;toggle leds
	goto    BL1
;
InitRtcc
        bsf     STATUS, RP0
	movlw   B'10000111'     ;rtcc inc = tcylc/256
	movwf   OPTION_REG      ;       /
        bcf     STATUS, RP0
	clrf    TMR0            ;start time
	movlw   B'10100000'     ;enable interrupts
	movwf   INTCON          ;       /
	movlw   .8              ;initialize time
	movwf   TIME
	return
;
ServiceRtcc
	btfsc   INTCON,T0IF     ;rtcc interrupt?
	decf    TIME, F         ;yes then dec time
	clrf    INTCON          ;clr all interrupts
	bsf     INTCON,T0IE     ;enable RTIE
	retfie                  ;not zero then return
;
SecondOver
	movf    TIME,W          ;check if time = 0
	btfss   STATUS,Z        ;       /
	goto    SecondOver      ;no then loop
	movlw   .8              ;load for 1/2 second
	movwf   TIME
	return                  

	end

	
