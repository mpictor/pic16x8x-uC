'THIS WORKS to read ROM code from DS1820 and xmit via serial. 11/17/08
'
'
'***********************************************************************************************************************
'
'Last modified January 19, 2001
'Written by Mark Pictor
'
'last mod: change to allow both C and F conv w/o re-programming, change formula for c>f conv.
'                       (it was faulty - used 32 instead of 3200)
'mod: switch from con1 (byte) to con (word) for c2ascii_disp subroutine
'mod: disable c>f because wierd output (?)
'mod: convert c to f
'mod: setup to use more precise temp. reading from ds1820
'mod: use fsr and indf to access trisb, reset timing from use of status (original problems?)
'         also inserted bcf rp0 in byte read/write routines; still need to change RB5 routines...
'mod: add change of status.rp0 for use with TRIS on all ASSEMBLY routines
'         specific to RB1 (the first DS1820) - need to change RB5 routines also, just not now...
'
'***********************************************************************************************************************
'
'this program is accurate to 72.816666666 deg C (163.07 deg F) due to overflow
'in conversion to Fahrenheit (it's good for Celsius above that though)
'the overflow is when multiplying by 9
'swapping the order of *9 and /5 (/5 first) will increase Fahrenheit overflow temp. 
'to 346.3055556 deg C (655.35 deg. F) -  (NOTE that the device only works up to 125C and
'any temperature greater than 127 deg C will result in overflow inside the DS1820 - who
'knows what would happen then...)
'
'swapping the *9 and /5 will, however, decrease accuracy by some small amount
'
'this code will also not work below 0 deg C - see the DS1820 data sheet for how to do this...
'
'files to reference:
'fancontA.bat
'..\new.txt
'..\hex\adc0831.src
'..\inc\p84.inc
'..\inc\pbl.inc
'pic16f84 datasheet
'1820 datasheet

'1820s on pins rb1 and rb5
'pic at 4mhz - 1 us per instruction

'DSLOW1,DSLOW2 - low output
'DSHIGH1,DSHIGH2 - high output
'DSRD1,DSRD2 - read slot; _ctemp LSb is read value
'_initmp1,_initmp2 - initialize pulse - _ctemp bits 1&5 are high for response
'_wbyte1,_wbyte2 - write a byte (_ctemp is source)
'_rbyte1,_rbyte2 - read a byte, put it in _ctemp


'tris low for output ****asm only!!!**** (high in pbc)

'================================================================================


'BASIC program here:

'pins used:

'DISPLAY
'PortB pin    disp. connection
'RB2 (8)                   Clock (2)
'RB3 (9)                   Data  (4)
'RB4 (10)                  Reset (5)

'DS1820s:
'RB1(7)
'RB5(11)


'declarations 

'DISPLAY
Symbol  vfdres = 4      ' VFD reset signal
Symbol  VFDDIR = Dir3   ' VFD data pin direction bit
Symbol  VFDPIN = Pin3   ' VFD data pin
Symbol  VFCPIN = 2      ' VFD clock pin is 2
symbol dispdata= B0             ' data to display or command to send

'ds1820
symbol temp1 = 1                'pin to use with 1st DS1820
symbol temp2 = 5                'pin to use with 2nd DS1820
symbol temperature1 = W4        'word var for returned temp from the 1820 on rb1
symbol temperature2 = W5        'word var for returned temp from the 1820 on rb5
symbol ctemp = B7               'working byte for assembly subs for bit i/o with 1820s

symbol tval1 = B8               'value for temp1
symbol sign1 = B9               'sign BYTE for temp1
symbol count_per_c1 = B18       'temp1 var for more precise temperature
symbol count_remain1 = B19      '"              "       "       "               "               "

symbol TE1 = W7                 'for calc. precise temp.
symbol TE2 = W8                 '"              "       "               "

symbol tval2 = B10              'temp2 value
symbol sign2 = B11              'temp2 sign
symbol count_per_c2 = B20       'temp2 var for more precise temperature
symbol count_remain2 = B21      '"              "       "       "               "               "


'general
'B0 and B1 are used also but not renamed
Symbol       I = B2     ' Loop counter
Symbol       X = B3     ' For/next in main routine

'conversion
symbol    con = W6      'conversion variable 

'***********************************************************************************************************************
'PIC 16C84 resource useage
'Variables
'B0 - dispdata - display
'B1 - 
'B2 - I - general
'B3     - X - general
'B4
'b5
'b6
'b7 - ctemp - assy routines for DS1820
'b8 - tval1 - W4 - temp from 1st ds1820
'b9 - sign1 - W4
'b10 - tval2 - W5 - temp from 2nd ds1820
'b11 - sign2 - W5
'b12 - W6 - con - conversion to ASCII
'b13 - W6
'b14 - W7 - TE1 - temp. conv.
'b15 - W7
'b16 - W8 - TE2 - temp. conv.
'b17 - W8
'b18 - count_per_c1
'b19 - count_remain1
'b20 - count_per_c2
'b21 - count_remain2


'***********************************************************************************************************************

Dirs=%01011101          ' all outputs except rb1,rb5,rb7
Pins=%00100110                  ' all outputs low except 1,2,5

'================================================================================

startup:                                'initialize anything that needs it
main:

call initmp1    'init - rb1
if ctemp>0 then present
        gosub NotPresent        'put message on display, end program
present:
ctemp=$cc               'skip rom command
call wbyte1             'write byte - rb1
ctemp=$44               'start conversion
call wbyte1             'write command to DS1820
pause 500               '.5s wait - for temp conversion and wait to update disp
call initmp1
ctemp=$cc               'skip rom command
call wbyte1             'write byte - rb1
ctemp=$be               'read scratchpad
call wbyte1             'write read scratchpad command to DS1820
ASM
;ASM
        call    _rbyte1         ;read a byte from the scratchpad (temp value here)
        movf    _ctemp,0        ;copy it to W
        movwf   _tval1          ;now to tval1
        call    _rbyte1         ;read sign byte
        movf    _ctemp,0        ;copy to W
        movwf   _sign1          ;now to sign1
        call    _rbyte1         ;th alarm>nowhere
        call    _rbyte1         ;tl alarm>null
        call    _rbyte1         ;reserved byte>null
        call    _rbyte1         ;reserved byte>null
        call    _rbyte1         ;count_remain
        movf    _ctemp,0
        movwf   _count_remain1  
        call    _rbyte1         ;count_per_c
        movf    _ctemp,0
        movwf   _count_per_c1
;ENDASM
ENDASM
gosub convDisp          'convert and display

'now read serial number, convert to hex, display
call initmp1
ctemp = $33             'read rom command
call wbyte1
ASM
;ASM            ;store serial number in B8-B15
        call    _rbyte1         ;read byte
        movf    _ctemp,0        ;copy to W
        movwf   _B8             ;now to var
        call    _rbyte1         ;read byte
        movf    _ctemp,0        ;copy to W
        movwf   _B9             ;now to var
        call    _rbyte1         ;read byte
        movf    _ctemp,0        ;copy to W
        movwf   _B10            ;now to var
        call    _rbyte1         ;read byte
        movf    _ctemp,0        ;copy to W
        movwf   _B11            ;now to var
        call    _rbyte1         ;read byte
        movf    _ctemp,0        ;copy to W
        movwf   _B12            ;now to var
        call    _rbyte1         ;read byte
        movf    _ctemp,0        ;copy to W
        movwf   _B13            ;now to var
        call    _rbyte1         ;read byte
        movf    _ctemp,0        ;copy to W
        movwf   _B14            ;now to var
        call    _rbyte1         ;read byte
        movf    _ctemp,0        ;copy to W
        movwf   _B15            ;now to var
;ENDASM
ENDASM
                'now convert to hex and transmit
gosub hexOut    'uses B8, which is the first byte anyway
B8=B9
gosub hexOut
B8=B10
gosub hexOut
B8=B11
gosub hexOut
B8=B12
gosub hexOut
B8=B13
gosub hexOut
B8=B14
gosub hexOut
B8=B15
gosub hexOut

goto main
'================================================================================
hexOut:
'convert to hex, then call dispout for each nibble
'data is in B8
I=0
ctemp = B8 & $F0
ctemp = ctemp / 16
hex2:
if ctemp <= 9 then hexNumeric
ctemp = ctemp + 7              'additional offset for alpha
hexNumeric:
dispdata = ctemp + $30          'offset
gosub dispout
if I=1 then hexEnd
I=1
ctemp = B8 & $0F
goto hex2
hexEnd:
return
'================================================================================
convdisp:
'convert to more accurate
'truncate LSb (.5 deg) first
'temp=read_temp-.25+((count_per_c1-count_remain1)/count_per_c1)
'picbasic computes strictly left-to-right...
'...but it doesn't seem to handle multiple operations on a line correctly
tval1=tval1/2   'remove LSb (.5C)
TE1=tval1*100   '*100 for decimal point
TE1=TE1-25
TE2=count_per_c1-count_remain1
TE2=TE2*100
TE2=TE2/count_per_c1
TE1=TE1+TE2


'convert to F (if rb7 is low)
if pin7=1 then skipCtoF
'f=9/5c+32
TE2=TE1*9
TE2=TE2/5
TE2=TE2+3200    '3200 because this is *100...
X="F"           'type of conversion
goto ready2Disp

skipCtoF:       'skip convert
TE2=TE1
X="C"           'type of conversion

ready2Disp:
'display
dispdata = $0D:                 gosub dispout           'CR/LF
dispdata = $0A:                 gosub dispout
gosub c2ascii_disp                                      'convert and display
dispdata = X:                   gosub dispout           'Celsius (C) or Fahrenheit (F)

return

'================================================================================

NotPresent:
        For X = 0 to 14 '15 characters                   
         Lookup x,("DS: No Response"),dispdata
         Gosub dispout  ' display it
        Next X
'        dispdata = $4          'SB - set brightness
'        gosub dispout
'        dispdata = 128         'brightness value
'        gosub dispout
                end             'The only way out of the sub is by NEVER ENTERING IT
                'return                   '****** for testing, allow program to continue *******

'================================================================================

' Subroutine to synchronously shift out one byte to VFD
dispout:
Serout VFCPIN,T9600,(dispdata)  'vfd clock pin is rb2/pin8
'T for "true", as opposed to inverted.

Return

'================================================================================



c2ascii_disp: 'sub to convert temperature (celsius) to ASCII characters and display it

                'incoming value is in TE2

        b0 = TE2/10000                          '5th digit (10^4)
        b0=b0+48                                        'add 48 for offset (ASCII)
        Gosub dispout                           'display digit

        con = TE2//10000
        B0 = con/1000                           '4th digit (10^3)
                b0=b0+48
        gosub dispout

        con = TE2//1000  
        b0 = con/100                            '3rd digit   .
                b0=b0+48
        gosub dispout
        
        b0 = "." : gosub dispout        'decimal point

        con = TE2//100
        b0 = con/10                             '2nd digit   .
                b0=b0+48
        gosub dispout

        b0 = TE2//10                            '1st digit (10^0)
                b0=b0+48
        gosub dispout        
        
'        let con1 = b1//1000  
'        let con2 = con1/100   '3rd digit   .
'        let hundreds = con2 + 48       'ascii offset to 0
'        'gosub lcddata
'        let con1 = b1//100
'        let con2 = con1/10    '2nd digit   .
'        let tens = con2 + 48
'        'gosub lcddata
'        let con1 = b1//10   '1st digit (10^0)
'        let ones = con1 + 48
'        'gosub lcddata
Return





ASM 
;PIC Macro Assembler requires hex numbers to be suffixed with h (i.e. 85h)

;================================================================================
DSLOW1                  ;1-wire LOW output - rb1
                                ;what to do: hold low min. 60 us, then make input (high)
                                
        movlw 86h                               ;put 86h in w (the address of TRISB)
        movwf FSR                               ;now put that 86h in the FSR
        BCF             PORTB,_temp1    ;rb1 low
        BCF             INDF,_temp1             ;make rb1 output - accessing INDF actually accesses
                                                        ;the address stored in the FSR - which is 86h, or TRISB
        
        MOVLW   19                              ;how many times to loop - 61us
        call DSWAIT
        BSF             indf,_temp1             ;rb1 input (and high -- due to pullup)
                                                        ;total of 63us low
        goto done

;================================================================================
DSLOW2                  ;1-wire LOW output - rb5
                                ;what to do: hold low min. 60 us, then make input (high)

        BCF             PORTB,_temp2    ;rb5 low
        BCF             TRISB,_temp2    ;rb5 output
        
        MOVLW   19                              ;how many times to loop - 61us
        call DSWAIT
        BSF             TRISB,_temp2    ;rb5 input (and high -- due to pullup)
                                                        ;total of 63us low
        goto done

;================================================================================
DSHIGH1         ;1-wire HIGH output - rb1
                        ;what to do: low 1<t<15us, then high remainder of 60us
        ;
        movlw 86h                               ;put 86h in w (the address of TRISB)
        movwf FSR                               ;now put that 86h in the FSR
        BCF             PORTB,_temp1    ;rb1 low
        BCF     indf,_temp1             ;rb1 output
        MOVLW   2                               ;delay 10us - 11 counting this line
        call DSWAIT
        BSF             indf,_temp1             ;rb1 high
        MOVLW   16                              ;delay value - 52us
        call DSWAIT
                                                        ;leave with output high via pullup
                                                        ;total 66us
        goto done
        
;================================================================================
DSHIGH2         ;1-wire HIGH output - rb5
                        ;what to do: low 1<t<15us, then high remainder of 60us
        ;
        BCF             PORTB,_temp2    ;rb5 low
        BCF     TRISB,_temp2    ;rb5 output
        MOVLW   2       ;delay 10us - 11 counting this line
        call DSWAIT
        BSF             TRISB,_temp2    ;rb5 high
        MOVLW   16      ;delay value - 52us
        call DSWAIT
                ;leave with output high via pullup
                ;total 65us
        goto done
        
        
        
;================================================================================
DSRD1                   ;create read time slot & read result - rb1
                                ;what to do: low for >1us then read before 15us is up, wait 45us+ more
                                ;uses: T4, _ctemp (BYTE var that returns BIT value)
                                ;
        movlw 86h ;put 86h in w (the address of TRISB)
        movwf FSR       ;now put that 86h in the FSR
        BCF             PORTB,_temp1    ;low
        BCF     indf,_temp1             ;output
        clrf    _ctemp                  ;clear the _ctemp byte var in prep for below - putting it here
                                                        ;makes for faster code (replaces a nop)
        nop                                             ;wait
        bsf             indf,_temp1             ;input - pullup
        movlw   2                               ;10us worth - 14 total to end of dswait from output going low
        call    DSWAIT
        movf    portb,0                 ;15us total
                                                        ;this copies portb to W
        movwf   T4                              ;now "move" it to var T4
        btfsc   T4,_temp1               ;was input low? skip next instruction if is was
        incf    _ctemp,1                ;bit was high, increment _ctemp (0+1=1)
        movlw   15                              ;18us from drop of portb, need 42 more -- 49 will work
        call    DSWAIT                  ;wait the remainder of the 60us
        goto    done
        
        
;================================================================================
DSRD2                   ;create read time slot & read result - rb5
                                ;what to do: low for >1us then read before 15us is up, wait 45us+ more
                                ;uses: T4, _ctemp (BYTE var that returns BIT value)
        BCF             PORTB,_temp2    ;low
        BCF     TRISB,_temp2    ;output
        clrf    _ctemp                  ;clear the _ctemp byte var in prep for below - putting it here
                                                        ;makes for faster code (replaces a nop)
        bsf             trisb,_temp2    ;input - pullup
        movlw   2                               ;10us worth - 13 total to end of dswait from output going low
        call    DSWAIT
        movf    portb,0                 ;this copies portb to W
                                                        ;14us total at this point
        movwf   T4                              ;move it to var T4
        btfsc   T4,_temp2               ;was input low? skip next opcode if true
        incf    _ctemp,1                ;bit was high, increment _ctemp (0+1=1)
        movlw   15                              ;18us from drop of portb, need 42 more -- 49 will work
        call    DSWAIT                  ;wait the remainder of the 60us
        goto    done
        
;================================================================================
_initmp1                ;initialize pulse over rb1
                                ;todo:send low >480us, ds waits 15-60us, then it holds low 60-240us
                                ;presence if low after released for 60us; presence signal
                                ;valid for add'l 60us min.
                                ;uses: _ctemp - bit 1 - high for presence, low for none
                                ;rest of _ctemp is cleared
                                ;166,19-values for dswait - 502,61us
                                ;had to add setting of fsr and use of indf wherever there was a tris access
                                ;due to TRIS being on the second memory page
                                ;at first, used status-rp0, but this didn't work for unknown reasons.

        movlw   86h                     ;put 86h in w (the address of TRISB)
        movwf   FSR                             ;now put that 86h in the FSR
        bcf             portb,_temp1    ;low rb1
        bcf             indf,_temp1             ;output rb1
        movlw   166                             ;set up 502us delay
        call    DSWAIT
        bsf             indf,_temp1             ;input, high by pullup
        movlw   19                              ;61us
        call    DSWAIT
        clrf    _ctemp                  ;clear the variable
        btfss   portb,_temp1    ;check if temp1 responded (low if present)
        bsf             _ctemp,_temp1   ;if it did, set corresponding bit in _ctemp
                                                        ;if it isn't responding, 
                                                        ;the bit stays zero (cleared).
                                                        
        movlw   166                             ;finish up required wait period
        call    DSWAIT                  ;502 us is good enough
        goto    done


;================================================================================
_initmp2                ;initialize pulse over rb5
                                ;todo:low >480us, release >60us, check presence
                                ;present if low after released for 60us --
                                ;presence signal valid for add'l 60us minimum
                                ;uses: _ctemp - bit 5 - high for presence, low for none
                                ;rest of _ctemp is cleared
                                ;166,19-values for dswait - 502,61us
        bcf     portb,_temp2    ;rb5 low
        bcf             trisb,_temp2    ;rb5 output
        movlw   166                             ;set up 502us delay
        call    DSWAIT
        bsf             trisb,_temp2    ;input
        movlw   19                              ;61us
        call    DSWAIT
        clrf    _ctemp                  ;clear the variable
        btfss   portb,_temp2    ;check temp2 - it'll be low if the DS1820 is present
        bsf             _ctemp,_temp2   ;set bit if it's present/responding
                                                        ;if one of them isn't responding, 
                                                        ;the bit stays zero (cleared).
        goto    done
        
;================================================================================
_wbyte1         ;write a byte to the DS1820 on PortB.1 (rb1)
                        ;uses: _ctemp (value to write), T3
        bcf             rp0                     ;select page 0
        movlw   8                       ;loop value, 8 reps
        movwf   T3                      ;put that value in the counter variable
w1loop  
        btfss   _ctemp,0        ;skip next if bit 0 is high
        call    DSLOW1          ;write a 0 bit
        btfsc   _ctemp,0        ;skip next if bit 0 is low
        call    DSHIGH1         ;write a 1 bit
        RRF     _ctemp,1        ;rotate (shift) right
        decfsz  T3,1            ;subtract 1, then check: exit loop if 0
        goto    w1loop
        goto    done



;================================================================================
_wbyte2         ;write a byte to the DS1820 on PortB.5 (rb5)
                        ;uses: _ctemp (value to write), T3
        movlw 8                         ;loop value, 8 reps
        movwf T3                        ;put that value in the counter variable
w2loop  
        btfss   _ctemp,0        ;skip next if bit 0 is high
        call DSLOW2                     ;write a 0 bit
        btfsc   _ctemp,0        ;skip next if bit 0 is low
        call DSHIGH2            ;write a 1 bit
        RRF _ctemp,1            ;rotate (shift) right
        decfsz T3,1                     ;subtract 1, then check: exit loop if 0
        goto w2loop
        goto done


;================================================================================
_rbyte1         ;read byte from ds1820 on rb1
                        ;uses: _ctemp(temporary & returning value), T3(count),T2(temporary value)
                        ;put bit returned from dsrd in MSb, shift down (right)
        bcf             rp0
        movlw   8                       ;loop value, 8 reps
        movwf   T3                      ;put that value in the counter variable
r1loop
        RRF     T2,1            ;rotate (shift) right
        call    DSRD1           ;read bit from rb1, returned as lsb of _ctemp
        bsf     T2,7            ;set MSb of T2
        btfss   _ctemp,0        ;skip next if set
        bcf     T2,7            ;clear the MSb of T2 - if returned was low
        decfsz  T3,1            ;exit loop counter check
        goto    r1loop
        movf    T2,0            ;copy T2 to W
        movwf   _ctemp          ;copy W to _ctemp
        goto    done
        

;================================================================================
_rbyte2         ;read byte from ds1820 on rb5
                        ;uses: _ctemp(temporary & returning value), T3(count),T2(temporary value)
                        ;put bit returned from dsrd in MSb, shift down (right)
        movlw 8                         ;loop value, 8 reps
        movwf T3                        ;put that value in the counter variable
r2loop
        RRF T2,1                        ;rotate (shift) right
        call DSRD2                      ;read bit from rb5, returned as lsb of _ctemp
        bsf T2,7                        ;set MSb of T2
        btfss _ctemp,0          ;skip next if set
        bcf T2,7                        ;clear the MSb of T2 - if returned was low
        decfsz T3,1             ;exit loop counter check
        goto r2loop
        movf T2,0                       ;copy T2 to W
        movwf _ctemp            ;copy W to _ctemp
        goto done
        


;================================================================================
;================================================================================
;================================================================================
;================================================================================
;================================================================================
;================================================================================
;================================================================================
;================================================================================
;================================================================================

;================================================================================
DSWAIT          ;wait - number of reps in W
                        ;use: W and T5
                        ;timing: 7Tcy + 3*(W-1)Tcy counting the call & return
                        ;"MOVLW 19" for 61us@4MHz
        MOVWF   T5      ;move count from W to T5
DSWT            ;start of loop
        DECFSZ  T5,1    ;T5=T5-1 - if that's 0 then skip the next opcode
        goto    DSWT    ;loop
        return


ENDASM