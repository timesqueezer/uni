
;*******************************************************************
;                           walk.asm
;
; This code walks LEDs backwards and forwards on port b. 
; Couldn't be simpler.
;*******************************************************************

	LIST    P=16C84;f=inhx8m
	
_CP_OFF		equ	H'3FFF'			;code protect off
_PWRTE_ON	equ	H'3FFF' 		;Power on timer on
_WDT_OFF	equ	H'3FFB'			;watch dog timer off
_XT_OSC		equ	H'3FFD'			;crystal oscillator
__CONFIG       _CP_OFF & _PWRTE_ON & _WDT_OFF & _XT_OSC
						;configure programmer directive

w       equ     0	; register destination numbers.
f       equ     1	
same	equ	1

z       equ     2	; status flags
zero	equ	2
c	equ	0
carry	equ	0
	
count1  equ     0C      ; wait counter ls digit file register C
count2  equ	0D	; wait counter ms digit file register D
portb   equ     06      ; port b I/O register f6
porta	equ	05	; port a I/O register f5
status  equ     03      ; status register f3
;
;
;
	org     0	; origin
;
init   
	movlw   0
	tris    portb                   ; set portb as outputs
	movwf   portb                   ; set portb levels all low

start
        bsf	portb,0         	; set portb bit 0 high
	

rot_L	call 	wait			; wait for a bit
	bcf	status,c		; clear carry bit
	rlf	portb,same		; rotate left portb, store result portb
	btfss	portb,7			; skip next line if top bit set
	goto	rot_L

rot_R	call 	wait			; wait for a bit
	bcf	status,c		; clear carry bit
	rrf	portb,same		; rotate right portb, store in portb
	btfss	portb,0			; skip next line if bottom bit set
	goto	rot_R
	goto 	rot_L			; do it all again	

; ----------------------------
; wait subroutine 
; ----------------------------
wait    
	movlw   .200	        ; load count1 with decimal 200
	movwf   count1
d1	movlw   .200		; load count2 with decimal 200
	movwf   count2	
				; shorten these for the simulator
	
d2	decfsz  count2,same	; decrement and skip next line if zero
	goto 	d2		; if not zero		
	decfsz  count1		; decrement count1 if count2 is zero
	goto 	d1		; do inside loop again if count2 nz
	retlw 	00
; ----------------------------        

END


