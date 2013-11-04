Assembly and PicBasic programs written for the PIC16C84 (36 bytes ram) and PIC16F84 (68 bytes RAM). Written without the aid of an oscilliscope or an emulator, ICE, or other debugging tool.

All programs assume a 4MHz clock. Code should be portable to other 14-bit PICs with minimal changes. However, this code hasn't been tested for *years* and may have suffered bitrot. I didn't do version control back then.

### Code most likely to be useful to others
* PIC 16X8X assembly for 1-Wire protocol
* PicBasic and assembly to drive Futaba US162SD03C Vacuum Fluorescent Display


### directories

* futaba
 - PicBasic programs to demo use of Futaba US162SD03C VFD.
* ds1820-no-guid
 - PicBasic and inline assembly; reads 2 DS1820s on different pins; makes no use of GUIDs
* muxdemux
 - test code for 4051, 74HC595
* readrom
 - read DS1820 GUIDs; PicBasic and assembly
* ds-rom
 - use GUIDs to read temps from 4 DS1820s on a single I/O pin
* finger-temps
 - perl script to decode hexadecimal output from ds-rom
