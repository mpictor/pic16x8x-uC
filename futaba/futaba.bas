'FUTABA.BAS
'Use with Futaba Vacuum Fluorescent Display US162SD03CB
'Pinout: (as marked on PCB of VFD)
'1   -   Vcc (+5V)
'2   -   Clock
'3   -   Ground
'4   -   Data
'5   -   Reset
'
'By Mark Pictor
'10-9-98

Symbol  vfdres = 2      ' VFD reset signal
Symbol  VFDDIR = Dir0   ' VFD data pin direction is Dir0
Symbol  VFDPIN = Pin0   ' VFD data pin is 0
Symbol  VFCPIN = 1      ' VFD clock pin is 1
Symbol       I = B2     ' Loop counter
Symbol       X = B3     ' For/next in main routine
Dirs=%11111111          ' all outputs

        high vfcpin
        pause 1         'ms
        High vfdres
        pause 50        'ms
        low vfdres

Loop:
        For X = 0 to 11 '12 characters                   
         Lookup x,("Hello There!"),B0
         Gosub dispout  ' display it
        Next X
        sleep 1
        for x = 0 to 255
         b0 = $4       'SB - set brightness
         pause 5
         gosub dispout
         b0 = x        'brightness value
         gosub dispout
        next x
        b0 = $1       'CD - clear disp.
        gosub dispout  'clear display
        sleep 1
Goto loop


' Subroutine to synchronously shift out one byte to VFD
dispout: VFDDIR = 1      ' Set data pin direction to output

        For I = 1 to 8  ' 8 bits to a byte
         VFDPIN = Bit7     ' Data out is MSB
         pause 1
         LOW VFCPIN     ' Toggle shift clock
         B0 = B0 * 2     ' Shift byte 1 bit to the left
         pause 1
         HIGH VFCPIN     ' Toggle shift clock once more
        Next I          ' Loop

Return                ' Go back to main

