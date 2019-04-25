	list p=16c84
;
;  This program runs on the PICDBD-1 demo board.
;  In the Demo board, Port B is connected to 8 LEDs. 
;  RA1 is connected to a switch (S3). This program increments
;  the file register count each time S3 is pressed.
;  The value of count is displayed on the LEDs connected
;  to Port B.
;  Net result is that Leds should increment in a binary
;  manner every time S3 is pressed.
;
	#include <P16C84.INC>
;
COUNT   equ     0x10
;
;
	org     00h             ; reset vector.
	goto    Start

	org     05h
Start
	movlw   0
	movwf   PORTB           ; config port b as output
        bsf     STATUS, RP0
	clrf    TRISB 
        bcf     STATUS, RP0
	clrf    COUNT           ; clr count
Loop
	btfss   PORTA,1         ; see if RA1 pressed
	goto    IncCount        ; yes then inc count
Endloop
	goto    Loop            ; else check again
IncCount
	incf    COUNT, F        ; inc count
	movf    COUNT,W         ;
	movwf   PORTB           ; display on port b
Debounce
	btfss   PORTA,1         ; wait for key release
	goto    Debounce        ; not release then wait
Enddebounce
	goto    Loop            ; else check key press again
;
	end




