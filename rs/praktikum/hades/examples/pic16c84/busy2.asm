; 
; control a LCD dot-matrix display using a PIC16C84,
; for test in HADES.
; (FNH)
;
; 18.06.99 - first version
;

;
; This program interfaces a PIC16C84 to a (Hitache LM032L) 2x20 character 
; display module. For this first version, we use 8-bit communication on PORTB, 
; with control signals on PORTA.
;


;      PROCESSOR   16C84
;      RADIX       DEC
      EXPAND
      LIST P=16C84
      ERRORLEVEL  -302



      include <p16c84.inc>


_ResetVector   set    0x00
_IntVector     set    0x04


FALSE	       EQU	0
TRUE           EQU	1

Dev_Freq       EQU   D'4000000'     ; 4 MHz clock frequency

DB_HI_BYTE     EQU   (HIGH ((( Dev_Freq / 4 ) * 1 / D'1000' ) / 3 ) ) + 1
LCD_INIT_DELAY EQU   (HIGH ((( Dev_Freq / 4 ) * D'46' / D'10000' ) / 3 ) ) + 1
MSD    	       EQU   0x023   
LSD    	       EQU   0x024

TABLE_INDEX    EQU   0x020
CHAR           EQU   0x021
TEMP           EQU   0x022



;
; LCD port assignments
;

LCD_DATA       EQU	PORTB
LCD_DATA_TRIS  EQU	TRISB
LCD_CNTL       EQU	PORTA

;
; LCD Display Commands and Control Signal names.
;

E              EQU	3	; LCD Enable control line
RW	       EQU	2	; LCD Read/Write control line
RS	       EQU	1	; LCD Register Select control line

;
;
; LCD Module commands
;
DISP_ON                 EQU     0x00C   ; Display on
DISP_ON_C               EQU     0x00E   ; Display on, Cursor on
DISP_ON_CB              EQU     0x00F   ; Display on, Cursor on, Blink cursor
DISP_OFF                EQU     0x008   ; Display off
CLR_DISP                EQU     0x001   ; Clear the Display
ENTRY_INC               EQU     0x006   ;
ENTRY_INC_S             EQU     0x007   ;
ENTRY_DEC               EQU     0x004   ;
ENTRY_DEC_S             EQU     0x005   ;
DD_RAM_ADDR             EQU     0x080   ; Least Significant 7-bit for address
DD_RAM_UL               EQU     0x080   ; Upper Left corner of the Display




;
; seset vector: goto START
;

       org     _ResetVector

RESET
	GOTO    START            



;
; interrupt routine. Should NOT get here
;
        org     _IntVector              ; Interrupt vector location
ERROR1     
	GOTO 	ERROR1


;
;
;
START                         

	CLRF    STATUS          ; Do initialization (Bank 0)

            BSF     STATUS, RP0     ; Select Bank 1
            CLRF    TRISA           ; RA5 -  0 outputs
            MOVLW   0xFF            ;
            clrf    TRISB           ; RB7 - 0 inputs, RB3 - 0 outputs

;            BSF     OPTION_REG,NOT_RBPU  ; Disable PORTB pull-ups
            BCF     STATUS, RP0     ; Select Bank 0
;


;
; FNH FNH
;


      call     LCD_INIT
      call     LCD_DEFAULTS 

	    call     HARD_MSG
	    call     TABLE_MSG
	    






;
; command sequence for 2 lines of 5x7 characters
;
CMD_SEQ
            MOVLW   0x038
            MOVWF   LCD_DATA        ; This code for both 4-bit and 8-bit modes
            BSF     LCD_CNTL, E     ; 
            BCF     LCD_CNTL, E     ;

;
; Busy Flag should be valid after this point
;
            MOVLW   DISP_ON_CB      ;
            CALL    SEND_CMD        ;
            MOVLW   CLR_DISP        ;
            CALL    SEND_CMD        ;
            MOVLW   ENTRY_INC       ;
            CALL    SEND_CMD        ;
            MOVLW   DD_RAM_ADDR     ;
            CALL    SEND_CMD        ;
            return



;
; Send a message the hard way
;

HARD_MSG
            movlw   'W'
            call    SEND_CHAR
            movlw   'e'
            call    SEND_CHAR
            movlw   'l'
            call    SEND_CHAR
            movlw   'c'
            call    SEND_CHAR
            movlw   'o'
            call    SEND_CHAR
            movlw   'm'
            call    SEND_CHAR
            movlw   'e'
            call    SEND_CHAR
            movlw   ' '
            call    SEND_CHAR
            movlw   't'
            call    SEND_CHAR
            movlw   'o'
            call    SEND_CHAR
            movlw   ' '
            call    SEND_CHAR
            movlw   'H'
            call    SEND_CHAR
            movlw   'a'
            call    SEND_CHAR
            movlw   'd'
            call    SEND_CHAR
            movlw   'e'
            call    SEND_CHAR
            movlw   's'
            call    SEND_CHAR
            movlw   ' '
            call    SEND_CHAR
            movlw   '0'
            call    SEND_CHAR
            movlw   '.'
            call    SEND_CHAR
            movlw   '6'
            call    SEND_CHAR
            movlw   '7'
            call    SEND_CHAR
            movlw   'i'
            call    SEND_CHAR

            movlw   B'11000000'     ;Address DDRam first character, second line
            call    SEND_CMD


            return

	
;
; Demonstration of the use of a table to output a message
;

TABLE_MSG
            movlw   0               ;index into table at start of message
dispmsg 
            movwf   TABLE_INDEX     ;TABLE_INDEX holds start of message address
            call    TABLE
            andlw   0FFh            ;check if at end of message (zero
            btfsc   STATUS,Z        ;returned at end)
            goto    out             
            call    SEND_CHAR       ;send data for one character
            movf    TABLE_INDEX,w   ;point to next character
            addlw   1
            goto    dispmsg
out
loop
            goto    loop            ;Stay here forever


;
; LCD_INIT:
; initialize the display: send the 'set function' command and select
; the 8-bit (vs. 4-bit) bus interface, 2-line display, internal font.
; This routine falls-through into the LCD_DELAY routine.
;

LCD_INIT
            CLRF    LCD_CNTL        ; control port output should output Low.
            MOVLW   0x038           ; 8-bit interface, select 2 lines, internal font
            MOVWF   LCD_DATA        ;
            BSF     LCD_CNTL, E     ; 
            BCF     LCD_CNTL, E     ;

            ; fall-through


;
; this routine takes the calculated times that the delay loop needs to
; be executed, based on the LCD_INIT_DELAY EQUate that includes the
; frequency of operation. These uses registers before they are needed to 
; store the time.
;

LCD_DELAY 
            movlw   LCD_INIT_DELAY  ;
            movwf   MSD             ; use MSD and LSD regs for delay counters
            clrf    LSD             ;
LOOP2    
            decfsz  LSD, F          ; delay time = MSD * ((3 * 256) + 3) * Tcy
            goto    LOOP2           ;
            decfsz  MSD, F          ;
END_LCD_DELAY
            goto    LOOP2           ;
	    return;





;
; LCD_DEFAULTS:  send 'display on', 'clear display', 'entry mode, no shift'
; commands to the display.
;
LCD_DEFAULTS
            movlw   DISP_ON_CB      ; Display On, Cursor On, Cursor Blink
            call    SEND_CMD        ; 
            movlw   CLR_DISP        ; Clear the Display
            call    SEND_CMD        ; 
            movlw   ENTRY_INC       ; Set Entry Mode Inc., No shift
            call    SEND_CMD        ; 
            return



;
; SEND_CHAR: send one character (but not a command) from register W
; to the LCD, data is transmitted on port<7:0> pins.
;
SEND_CHAR
            movwf   CHAR            ; character to be sent is in W
            call    BUSY_CHECK      ; wait for LCD to be ready
            movf    CHAR, w          
            movwf   LCD_DATA        ; send data to LCD
            bcf     LCD_CNTL, RW    ; set LCD in read mode
            bsf     LCD_CNTL, RS    ; set LCD in data mode
            bsf     LCD_CNTL, E     ; toggle E for LCD
            bcf     LCD_CNTL, E
            return


;
; SEND_CMD: send one command (but not character data) from register W 
; to the LCD, data is transmitted on port<7:0> pins.

SEND_CMD
            movwf   CHAR            ; command to be sent is in W
            call    BUSY_CHECK      ; wait for LCD to be ready
            movf    CHAR, w          
            movwf   LCD_DATA        ; send data to LCD
            bcf     LCD_CNTL, RW    ; set LCD in read mode
            bcf     LCD_CNTL, RS    ; set LCD in command mode
            bsf     LCD_CNTL, E     ; toggle E for LCD
            bcf     LCD_CNTL, E
            return


;
; check the busy flag of the LCD display module (8-bit version).
; The method loops until the busy flag is cleared.
;

BUSY_CHECK
;            return

BUSY_CHECK_ORIG
            bsf     STATUS,RP0      ; select Register page 1
            movlw   0xFF            ; set data port for input
            movwf   LCD_DATA_TRIS

            bcf     STATUS, RP0     ; select Register page 0
            bcf     LCD_CNTL, RS    ; set LCD for command mode
            bsf     LCD_CNTL, RW    ; setup to read busy flag
            bsf     LCD_CNTL, E     ; set E high
;            bcf     LCD_CNTL, E     ; set E low -- to early: hold time viol.
            movf    LCD_DATA, w     ; read busy flag, DDram address
            bcf     LCD_CNTL, E     ; set E low
         
            movwf   TEMP    
            btfsc   TEMP, 7         ; check busy flag, high=busy
            goto    BUSY_CHECK

            bcf     LCD_CNTL, RW        
            bsf     STATUS, RP0     ; select Register page 1
            movlw   0x00
            movwf   LCD_DATA_TRIS   ; set port for output again
            bcf     STATUS, RP0     ; select Register page 0
            return




;
; some dummy data for table-based character output
;
TABLE
            addwf   PCL, F          ;Jump to char pointed to in W reg
            retlw   '('
            retlw   'C'
            retlw   ')'
            retlw   ' '
            retlw   '1'
            retlw   '9'
            retlw   '9'
            retlw   '9'
            retlw   ' '
            retlw   'U'
            retlw   'n'
            retlw   'i'
            retlw   'v'
            retlw   '.'
            retlw   ' '
            retlw   'H'
            retlw   'a'
            retlw   'm'
            retlw   'b'
            retlw   'u'
            retlw   'r'
            retlw   'g'
            retlw   ' '
            retlw   'I'
            retlw   'n'
            retlw   'f'
            retlw   'o'
            retlw   'r'
            retlw   'm'
            retlw   'a'
            retlw   't'
            retlw   'i'
            retlw   'k'
            retlw   '/'
            retlw   'T'
            retlw   'E'
            retlw   'C'
            retlw   'H'
TABLE_END
            retlw   0
;
	if ( (TABLE & 0x0FF) >= (TABLE_END & 0x0FF) )
		MESSG "Warning - Table crosses page boundry in computed jump"
	endif
;



    end




