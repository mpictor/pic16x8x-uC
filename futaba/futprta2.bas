'FUTprtA.BAS
'Use with Futaba Vacuum Fluorescent Display US162SD03CB
'Pinout: (as marked on PCB of VFD)
'1   -   Vcc (+5V)
'2   -   Clock          PortA.1 (pin 18)
'3   -   Ground
'4   -   Data           PortA.0 (17)
'5   -   Reset          PortA.2 (1)
'
'By Mark Pictor
'10-9-98

Symbol   PortA = 5      ' PortA is PIC register 5
Symbol   TrisA = $85    ' PortA direction register is hex 85
Symbol       N = B2     ' For bitwise calculations necessary
                        '  for use of VFD on PortA
Symbol       I = B3     ' Loop counter
Symbol       X = B4     ' For/next in main routine
Pins = %10111111        ' all pins but 1 high on PortB
Dirs=%11111111          ' all outputs on PortB
Poke TrisA, 1           ' all out on PortA

        Poke PortA,2    'clock high
        pause 1         'ms
        Poke PortA,6    'clock and reset high
        pause 50        'ms
        Poke PortA,2    'clock high, reset low

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
dispout:
        For I = 1 to 8          ' 8 bits to a byte           
         N=Bit7					' Data out is MSB
         Poke PortA,N           
         pause 1
         Let N = Bit7|%10       ' Add clock signal to data
         Poke PortA,N           ' Clock high, data still present
         B0 = B0 * 2            ' Shift byte 1 bit to the left
         pause 1
         N=Bit7
         Poke PortA,N        ' Clock low, data still present
        Next I                  ' Loop

Return                ' Go back to main

