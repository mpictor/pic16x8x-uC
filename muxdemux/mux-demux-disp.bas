'This program WORKS 10PM 15 Feb
'takes input from 4051, expresses it on display and 74HC595
'rapid loop, don't know just how fast


'pins used:
'
'DISPLAY
'PortB pin    disp. connection
'RB2 (8)		   Clock (2)
'RB3 (9)		   Data  (4)
'RB4 (10)		   Reset (5)
'
'74HC595:	'595 sym.	pin
'RB6(12)		_G_		(13)
'RB7(13)		RCK		(12)
'RA2(1)			SRCK	(11)		***SHARED with 4051***
'RA3(2)			SER		(14)		***SHARED with 4051***
'
'CD4051:	4051 sym.	pin
'RA0(17)		I/O		(6)
'RA1(18)		A		(11)	
'RA2(1)			B		(10)		***SHARED with '595***
'RA3(2)			C		(9)			***SHARED with '595***
'
'
'declarations 
'
'74HC595
'NO pins symbolized because it'll have to be an assembly routine due to use of PortA
symbol shiftdat = b10	'used by serout subroutine (74HC595)
'
'CD4051
'NO pins symbolized because it'll have to be an assembly routine due to use of PortA
symbol andat = b3		'used by ain routine (4051)
'						'******also used by temp code (as ctemp and _ctemp)******
'
'DISPLAY
symbol  vfdres = 4      ' VFD reset signal
symbol  VFDDIR = Dir3   ' VFD data pin direction bit
symbol  VFDPIN = Pin3   ' VFD data pin
symbol  VFCPIN = 2      ' VFD clock pin is 2
symbol dispdata= B0		' data to display or command to send

'general
'B0 and B1 are used also but not renamed
symbol       X = B2     ' For/next counter: error, shiftout, ain, dispout
symbol  PORTA  = 5
symbol  TRISA  = $85	'PortA TRIState register

'***********************************************************************************************************************

Dirs=%11111111          ' all outputs
Pins=%10100110			' all outputs high except 0,3,4,6
poke TRISA,%00000001	' ra0 is input (HARDWARE IS INVERTED FROM PBC "STANDARD")
poke PORTA,%00000000	' all low

'================================================================================

startup:				'initialize anything that needs it

'74HC595
shiftdat=255
gosub shiftout	'all registers on 595 high
				'(doesn't affect outputs because _G_ isn't low yet)
low 6			'enable '595 outputs
        
'Display
ASM
;ASM
;Part of display reset routine
	bsf 	portb,_vfdres	;reset high
	movlw	9				;set up 31us delay
	call	DSWAIT
	bcf		portb,_vfdres	;reset low
;ENDASM
ENDASM
	pause 2			'ms
	dispdata=$4:	gosub dispout	'set brightness
	dispdata=128:	gosub dispout	'to 128 (about half)
	

'================================================================================

main:
'gosub init1820s

'check buttons etc. here
gosub checkbuttons

'-----------LINE 1-----------
dispdata = $2:			gosub dispout		'set cursor position
dispdata = $1:			gosub dispout		'cursor at upper left corner
shiftdat=b1
gosub dispbits

gosub shiftout			'control the relays

goto main
'================================================================================

checkbuttons:
			'connection:
			'I/O-ra0
			'A-ra1 - LSb
			'B-ra2
			'C-ra3 - MSb	
	for x = 0 to 7
		b0=x*2				'we use x for the address on the 4051
							'shift left by 1
		poke porta,b0	
		pause 1				'ms
		peek porta,b0
		b1=b1/2
		bit15=bit0			'MSb of b1 now equals i/o pin of 4051
	next x
return

'================================================================================

' Subroutine to synchronously shift out one byte to VFD
dispout: VFDDIR = 1      ' Set data pin direction to output

	For X = 1 to 8  ' 8 bits to a byte
    	VFDPIN = Bit7     ' Data out is MSB
ASM
;ASM
	nop
	nop							;>3us delay
	nop
	nop
	bcf		portb,_vfcpin		;clock low
	movlw	4					;set up 16us delay
	call 	DSWAIT
	bsf 	portb,_vfcpin		;clock high
	nop
				;need to wait 2 us more before clocking again - the rest of the code does that easily
;ENDASM
ENDASM
		dispdata = dispdata * 2     ' Shift byte 1 bit to the left
	Next X          ' Loop
ASM
	movlw 	5					;19us delay
	call 	DSWAIT
ENDASM

Return                ' Go back to main

'================================================================================

shiftout:		'shiftout to 74hc595
					'use variable shiftdat
					'connection:
					'rb6-_g_ - gate enable when low (shouldn't need to do anything with it here)
					'rb7-RCK - register clock: pulse high to copy data from shift register to pin latches
					'ra2-srck - serial clock
					'ra3-SER - serial data
					'no need for delays because 74HC595 is typ. 50MHz, guaranteed to 30MHz
	b0=0
	b1=shiftdat
	for x = 1 to 8
		bit3=bit15		'ra3=MSb of shiftdat
		poke porta,b0
		bit2=1			'SRCK
		poke porta,b0
		b1=b1*2			'shift left 1
		bit2=0			'SRCK
		poke porta,b0
	next x
	high 7				'RCK (pulse high to latch data)
	pause 1
	low 7
return

'================================================================================
dispbits:
		'use b1
	for b20 = 1 to 8					'loop 8x
		if bit8=0 then az				'it's a zero
		dispdata = "1" : gosub dispout	'it's not - it's a one
		goto sk							'skip
	az:									'a zero
		dispdata = "0" : gosub dispout
	sk:
		b1=b1/2							'shift right 1
	next b20

return

ASM
;================================================================================
DSWAIT		;wait - number of reps in W
			;use: W and T5
			;timing: 7Tcy + 3*(W-1)Tcy counting the call & return
			;"MOVLW 19" for 61us@4MHz
	MOVWF	T5	;move count from W to T5
DSWT		;start of loop
	DECFSZ	T5,1	;T5=T5-1 - if that's 0 then skip the next opcode
	goto	DSWT	;loop
	return
ENDASM