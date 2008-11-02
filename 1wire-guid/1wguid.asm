include p16f84.inc

	__CONFIG _HS_OSC & _WDT_OFF & _PWRTE_ON & _CP_OFF
	radix dec
	org 0

;SFR assignments
indf		equ	0x00
status		equ	0x03
fsr		equ	0x04
porta		equ	0x05
portb		equ	0x06

;GPR assignments
temp		equ	0x0C
count8		equ	0x0D
hexcnt		equ	0x0E
count		equ	0x0F
;cbyte		equ	0x10
id1		equ	0x11
id2		equ	0x12
id3		equ	0x13
id4		equ	0x14
id5		equ	0x15
id6		equ	0x16
id7		equ	0x17
id8		equ	0x18
;skip some in case of overflow
ctemp		equ	0x30
T4		equ	0x31
;T3		equ	0x32
;T2		equ	0x33
T5		equ	0x34
tmp		equ	0x35

;for ds1820 port/pin assignment
dsport		equ	portb
dspin		equ	0x01

;constants
RP0		equ	5	;for use with status reg


;set TRIS registers
start	bsf	STATUS,RP0
	movlw	0x01
	movwf	portb
	movlw	0x1D		;11101 - was 0x11
	movwf	porta
	bcf	STATUS,RP0

	btfss 	portb,0	;don't do the test, if rb0 is high (switch)
	goto 	test

	;input data from ds1820
	movlw	0x8		;byte count
	movwf	count
	call	initmp
	movlw	0x33		;read rom command
	movwf	ctemp								;was cbyte
	call	wbyte		;tell ds to send rom contents
	movlw	0x11		;first ram address for ds1820 data
	movwf	tmp		;store address for fsr, since DS routines overwrite fsr
loop1	call	rbyte
	movf	tmp,w		;get fsr address
	movwf	fsr
	movf	ctemp,w								;was cbyte
	movwf	indf
	incf	tmp,f
	decfsz	count,f
	goto	loop1

	;output data via serial
	movlw	0x08
	movwf	count		;count number of bytes
	movlw	0x11		;address of first byte
loop2	movwf	fsr		
	call	serhex
	incf	fsr,f
	decfsz	count,f
	goto	loop2
	sleep			;until reset
	goto start

	;send two bytes to test serhex, then CR/LF
test	movlw	0x15
	movwf	fsr
	movlw	0x0A
	movwf	indf
	call	serhex
	movlw	0x9F
	movwf	indf
	call	serhex
	movlw	0x0A
	call	serbyte
	movlw	0x0D
	call	serbyte
	sleep
	goto start

;prog only does two chars over and over
;0-K fixed to 0-A
;why 33333333?


;Use RA2,RA3,RA4 for inputs from switches.  Use them to control which DS routines are used - those which use INDF, or those that don't.


;================================================================================
DSLOW1			;1-wire LOW output - rb1
				;what to do: hold low min. 60 us, then make input (high)
				
	movlw 86h 				;put 86h in w (the address of TRISB)
	movwf FSR				;now put that 86h in the FSR
	BCF		dsport,dspin	;rb1 low
	BCF		INDF,dspin		;make rb1 output - accessing INDF actually accesses
							;the address stored in the FSR - which is 86h, or TRISB
	
	MOVLW	19				;how many times to loop - 61us
	call DSWAIT
	BSF		indf,dspin		;rb1 input (and high -- due to pullup)
							;total of 63us low
	goto done
;================================================================================
DSHIGH1		;1-wire HIGH output - rb1
			;what to do: low 1<t<15us, then high remainder of 60us
	;
	movlw 86h 				;put 86h in w (the address of TRISB)
	movwf FSR				;now put that 86h in the FSR
	BCF		dsport,dspin	;rb1 low
	BCF 	indf,dspin		;rb1 output
	MOVLW	2				;delay 10us - 11 counting this line
	call DSWAIT
	BSF		indf,dspin		;rb1 high
	MOVLW	16				;delay value - 52us
	call DSWAIT
							;leave with output high via pullup
							;total 66us
	goto done
;================================================================================
DSRD1			;create read time slot & read result - rb1
				;what to do: low for >1us then read before 15us is up, wait 45us+ more
				;uses: T4, ctemp (BYTE var that returns BIT value)
				;
	movlw 86h ;put 86h in w (the address of TRISB)
	movwf FSR	;now put that 86h in the FSR
	BCF		dsport,dspin	;low
	BCF 	indf,dspin		;output
	clrf	ctemp			;clear the ctemp byte var in prep for below - putting it here
							;makes for faster code (replaces a nop)
	nop						;wait
	bsf		indf,dspin		;input - pullup
	movlw	2				;10us worth - 14 total to end of dswait from output going low
	call	DSWAIT
	movf	portb,0			;15us total
							;this copies portb to W
	movwf	T4				;now "move" it to var T4
	btfsc	T4,dspin		;was input low? skip next instruction if is was
	incf	ctemp,1		;bit was high, increment ctemp (0+1=1)
	movlw	15				;18us from drop of portb, need 42 more -- 49 will work
	call 	DSWAIT			;wait the remainder of the 60us
	goto 	done

;================================================================================
initmp1		;initialize pulse over rb1
				;todo:send low >480us, ds waits 15-60us, then it holds low 60-240us
				;presence if low after released for 60us; presence signal
				;valid for add'l 60us min.
				;uses: ctemp - bit 1 - high for presence, low for none
				;rest of ctemp is cleared
				;166,19-values for dswait - 502,61us
				;had to add setting of fsr and use of indf wherever there was a tris access
				;due to TRIS being on the second memory page
				;at first, used status-rp0, but this didn't work for unknown reasons.

	movlw	86h 			;put 86h in w (the address of TRISB)
	movwf	FSR				;now put that 86h in the FSR
	bcf		dsport,dspin	;low rb1
	bcf		indf,dspin		;output rb1
	movlw	166				;set up 502us delay
	call 	DSWAIT
	bsf		indf,dspin		;input, high by pullup
	movlw	19				;61us
	call 	DSWAIT
	clrf	ctemp			;clear the variable
	btfss	dsport,dspin	;check if temp1 responded (low if present)
	bsf		ctemp,dspin	;if it did, set corresponding bit in ctemp
							;if it isn't responding, 
							;the bit stays zero (cleared).
							
	movlw	166				;finish up required wait period
	call 	DSWAIT			;502 us is good enough
	goto	done
;================================================================================
wbyte1		;write a byte to the DS1820 on PortB.1 (rb1)
			;uses: ctemp (value to write), count8
	bcf	STATUS,RP0			;select page 0
	movlw 	8			;loop value, 8 reps
	movwf 	count8			;put that value in the counter variable
w1loop	
	btfss	ctemp,0	;skip next if bit 0 is high
	call 	DSLOW1		;write a 0 bit
	btfsc	ctemp,0	;skip next if bit 0 is low
	call 	DSHIGH1		;write a 1 bit
	RRF 	ctemp,1	;rotate (shift) right
	decfsz 	count8,1		;subtract 1, then check: exit loop if 0
	goto 	w1loop
	goto 	done
;================================================================================
rbyte1		;read byte from ds1820 on rb1
			;uses: ctemp(temporary & returning value), count8(count),temp(temporary value)
			;put bit returned from dsrd in MSb, shift down (right)
	bcf	STATUS,RP0
	movlw 	8			;loop value, 8 reps
	movwf 	count8			;put that value in the counter variable
r1loop
	RRF 	temp,1		;rotate (shift) right
	call 	DSRD1		;read bit from rb1, returned as lsb of ctemp
	bsf 	temp,7		;set MSb of temp
	btfss 	ctemp,0	;skip next if set
	bcf 	temp,7		;clear the MSb of temp - if returned was low
	decfsz	count8,1		;exit loop counter check
	goto 	r1loop
	movf 	temp,0		;copy temp to W
	movwf	ctemp		;copy W to ctemp
	goto 	done
	

;================================================================================
;	btfss	PORTA,2
;	goto	initmp1
;ra2-initmp - low to use indf
;ra3-wbyte - low to use indf
;ra4-rbyte - low to use indf


;============================================================================================
; toggle2		btfsc	porta,2		;toggle pin
; 		goto	clearpin
; 		bsf	porta,2
; 		return
; clearpin	bcf	porta,2
; 		return
;================================================================================
;wait for state change on button input...
; wait	
; 		btfss 	portb,0
; 		goto	wclr		;bit is cleared
; 		movlw	0xff		;bit is set
; 		movwf	count8
; 		goto	wtloop
; wclr		clrf	count8
; wtloop					;here we just wait for a status change on rb0
; 		call	DSWAIT		;0xff in w, so ~769us
; 		btfss	PORTB,0
; 		goto	wclr2
; 		btfss	count8,0	;rb0 set, check count8
; 		return			;only if c8 not set
; 		goto wtloop
; wclr2		btfsc	count8,0	;rb0 clear, check c8
; 		return			;only if c8 set
; 		goto wtloop

;================================================================================
;take byte in indf, convert to two ascii hex chars (0-F)
;then transmit it via serial
serhex		clrf	hexcnt		;counter for hex decode
nibb		swapf	indf,f		;swap nibbles; start with high byte
		movlw	0x09		;use to check whether numeric (0-9) or alpha (A-F) below
		movwf	temp
		movlw	0x0F		;00001111
		andwf	indf,w		;and with the byte
		subwf	temp,f		;put result in temp - w is not disturbed
		btfss	status,0	;if carry is clear, this nibble >9, so use additional offset
		addlw	0x07		;extra offset for ascii A-F (0x41 - 0x0A = 0x37)
		addlw	0x30		;offset for ascii 0-9
		call	serbyte		;serial out for byte in w
		btfsc	hexcnt,0	;don't return unless we have done both nibbles
		return
		incf	hexcnt,f	;low nibble flag
;		call 	toggle2
		goto	nibb		;do low nibble
;================================================================================
;serial transmit one byte (W) @ 4800 8N1, pin ra1
serbyte		clrf	count8		; put count8 to zero
		bcf	porta,	1	;start bit
		nop
		nop
		nop
		nop
		nop			;several nops to meet timing
		movwf	temp
newbit		call	delay		;go to delay
		btfsc	temp,	0	;is bit 0 = 0 on variable memory address?
		goto	one		;no, is 1
zero		bcf	porta,	1	;yes, send a 0 on RA1
		rrf	temp,	1	;move all bits down one place
		incf	count8,	1	;count8=count8+1
		btfss	count8,	3	;is it 8?
		goto	newbit		;no, go and find a new bit
		goto	stopbit		;yes, go and send stop bit
one		bsf	porta,	1	;send a 1 on RA1
		rrf	temp,	1	;move all bits down one place
		incf	count8,	1	;count8=count8+1
		btfss	count8,	3	;is it 8?
		goto	newbit		;no, go and find a new bit
stopbit		call	delay		;delay
		bsf	porta,	1	;send a 1 on RA1
		call	delay		;delays between bytes
		call	delay		;
		return
;============================================================================================
delay		movlw	0x40
		movwf	T5
loop		decfsz	T5,1
		goto	loop
		return

;============================================================================================
;====================================DS1820 stuff============================================
;============================================================================================
DSLOW			;1-wire LOW output - rb1
				;what to do: hold low min. 60 us, then make input (high)	
	;movlw	0x86 				;put 86h in w (the address of TRISB)
	;movwf	FSR				;now put that 86h in the FSR
	BCF		dsport,dspin	;rb1 low
	bsf		STATUS,RP0	;
	BCF		dsport,dspin		;make rb1 output - accessing INDF actually accesses
	
							;the address stored in the FSR - which is 86h, or TRISB
	
	MOVLW		19				;how many times to loop - 61us
	call		DSWAIT
	BSF		dsport,dspin		;rb1 input (and high -- due to pullup)
							;total of 63us low
	goto		done
;================================================================================
DSHIGH		;1-wire HIGH output - rb1
			;what to do: low 1<t<15us, then high remainder of 60us
	;movlw 0x86 				;put 86h in w (the address of TRISB)
	;movwf FSR				;now put that 86h in the FSR
	BCF		dsport,dspin		;rb1 low
	bsf		STATUS,RP0		;
	BCF 		dsport,dspin		;rb1 output
	MOVLW		2			;delay 10us - 11 counting this line
	call 		DSWAIT
	BSF		dsport,dspin		;rb1 high
	MOVLW		16			;delay value - 52us
	call 		DSWAIT
						;leave with output high via pullup
						;total 66us
	goto 		done
;================================================================================
DSRD			;create read time slot & read result - rb1
				;what to do: low for >1us then read before 15us is up, wait 45us+ more
				;uses: T4, ctemp (BYTE var that returns BIT value)

	;movlw 0x86 ;put 86h in w (the address of TRISB)
	;movwf FSR	;now put that 86h in the FSR
	BCF		dsport,dspin	;low
	bsf		STATUS,RP0	;
	BCF 		dsport,dspin		;output
	clrf		ctemp			;clear the ctemp byte var in prep for below - putting it here
							;makes for faster code (replaces a nop)
	;nop				;wait
	bsf		dsport,dspin	;input - pullup
	movlw	2			;10us worth - 14 total to end of dswait from output going low
	bcf		STATUS,RP0	;back to bank 0 in prep for reading it
	call	DSWAIT
	movf	dsport,0		;15us total
					;this copies portb to W
	movwf	T4			;now "move" it to var T4
	btfsc	T4,dspin		;was input low? skip next instruction if is was
	incf	ctemp,1			;bit was high, increment ctemp (0+1=1)
	movlw	15			;18us from drop of portb, need 42 more -- 49 will work
	call 	DSWAIT			;wait the remainder of the 60us
	goto 	done
;================================================================================
initmp
	btfss	PORTA,2
	goto	initmp1
				;initialize pulse over rb1
				;todo:send low >480us, ds waits 15-60us, then it holds low 60-240us
				;presence if low after released for 60us; presence signal
				;valid for add'l 60us min.
				;uses: ctemp - bit 1 - high for presence, low for none
				;rest of ctemp is cleared
				;166,19-values for dswait - 502,61us
				;had to add setting of fsr and use of indf wherever there was a tris access
				;due to TRIS being on the second memory page
				;at first, used status-rp0, but this didn't work for unknown reasons.

	;movlw	0x86 			;put 86h in w (the address of TRISB)
	;movwf	FSR				;now put that 86h in the FSR
	bcf		dsport,dspin	;low rb1
	bsf		STATUS,RP0	;select bank1 for tris
	bcf		dsport,dspin	;output rb1
	movlw		166		;set up 502us delay
	call 		DSWAIT
	bsf		dsport,dspin	;input, high by pullup
	bcf		STATUS,RP0	;select bank0 in prep for read
	movlw		19		;61us
	call 		DSWAIT
	clrf		ctemp		;clear the variable
	btfss		dsport,dspin	;check if temp1 responded (low if present)
	bsf		ctemp,dspin	;if it did, set corresponding bit in ctemp
					;if it isn't responding, 
					;the bit stays zero (cleared).
							
	movlw		166		;finish up required wait period
	call 		DSWAIT		;502 us is good enough
	goto		done
;================================================================================
wbyte
	btfss	PORTA,3
	goto	wbyte1

		;write a byte to the DS1820 on PortB.1 (rb1)
			;uses: ctemp (value to write), count8
	;bcf		STATUS,RP0	;select page 0
	movlw 		8		;loop value, 8 reps
	movwf 		count8		;put that value in the counter variable
wloop	btfss		ctemp,0		;skip next if bit 0 is high
	call 		DSLOW		;write a 0 bit
	btfsc		ctemp,0		;skip next if bit 0 is low
	call 		DSHIGH		;write a 1 bit
	RRF 		ctemp,1		;rotate (shift) right
	decfsz 		count8,1	;subtract 1, then check: exit loop if 0
	goto 		wloop
	goto 		done
;================================================================================
rbyte
	btfss	PORTA,4
	goto	rbyte1

		;read byte from ds1820 on rb1
			;uses: ctemp(temporary & returning value), count8(count),temp(temporary value)
			;put bit returned from dsrd in MSb, shift down (right)
	;bcf	STATUS,RP0
	movlw 	8			;loop value, 8 reps
	movwf 	count8			;put that value in the counter variable
rloop	RRF 	temp,1		;rotate (shift) right
	call 	DSRD		;read bit from rb1, returned as lsb of ctemp
	bsf 	temp,7		;set MSb of temp
	btfss 	ctemp,0	;skip next if set
	bcf 	temp,7		;clear the MSb of temp - if returned was low
	decfsz	count8,1		;exit loop counter check
	goto 	rloop
	movf 	temp,0		;copy temp to W
	movwf	ctemp		;copy W to ctemp
	goto 	done
;================================================================================
DSWAIT		;wait - number of reps in W
			;use: W and T5
			;timing: 7Tcy + 3*(W-1)Tcy counting the call & return
			;"MOVLW 19" for 61us@4MHz
	MOVWF	T5	;move count from W to T5
DSWT	DECFSZ	T5,1	;T5=T5-1 - if that's 0 then skip the next opcode
	goto	DSWT	;loop
	return

;================================================================================
;=============================cleanup routine====================================
;========================for compatibility with PBC==============================
;================================================================================
done	bcf	STATUS,RP0	;reset to bank 0
	clrwdt			;hit wdt even if not enabled
	return

end

