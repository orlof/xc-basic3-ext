REM **************************************
REM Spritemultiplexer adaptation
REM 
REM Based on:
REM Spritemultiplexing example V2.1
REM by Lasse Öörni (loorni@gmail.com)
REM Available at http://cadaver.github.io
REM **************************************

REM **************************************
REM * Color constants
REM **************************************
SHARED CONST COLOR_BLACK        = 0
SHARED CONST COLOR_WHITE        = 1
SHARED CONST COLOR_RED          = 2
SHARED CONST COLOR_CYAN         = 3
SHARED CONST COLOR_PURPLE       = 4
SHARED CONST COLOR_GREEN        = 5
SHARED CONST COLOR_BLUE         = 6
SHARED CONST COLOR_YELLOW       = 7
SHARED CONST COLOR_ORANGE       = 8 
SHARED CONST COLOR_BROWN        = 9
SHARED CONST COLOR_LIGHTRED     = 10
SHARED CONST COLOR_DARKGRAY     = 11
SHARED CONST COLOR_MIDDLEGRAY   = 12
SHARED CONST COLOR_LIGHTGREEN   = 13
SHARED CONST COLOR_LIGHTBLUE    = 14
SHARED CONST COLOR_LIGHTGRAY    = 15

REM **************************************
REM   0 to  3
CONST BANK                      = 0
REM **************************************
REM   0 to 15
CONST SCRMEM                    = 1
REM **************************************
REM 16 / 24 / 32
REM You must also update MAXSPR in ASM code
CONST MAX_NUM_SPRITES           = 16

REM **************************************
REM CODE START
REM **************************************
DIM BankAddr AS WORD
    BankAddr = 16384 * BANK
DIM ScrMemAddr AS WORD
    ScrMemAddr = BankAddr + 1024 * SCRMEM
DIM SprPtrAddr AS WORD
    SprPtrAddr = ScrMemAddr + 1016

REM FAST variables for sprite multiplexing
DIM numsprites AS BYTE FAST
DIM sprupdateflag AS BYTE FAST
DIM sortedsprites AS BYTE FAST
DIM tempvariable AS BYTE FAST
DIM sprirqcounter AS BYTE FAST

DIM sortorder(MAX_NUM_SPRITES) AS BYTE FAST

REM Unsorted sprite table
DIM sprx(MAX_NUM_SPRITES) AS BYTE
DIM spry(MAX_NUM_SPRITES) AS BYTE
DIM sprc(MAX_NUM_SPRITES) AS BYTE
DIM sprf(MAX_NUM_SPRITES) AS BYTE

SUB SpriteColor(spr_nr AS BYTE, color AS BYTE) SHARED STATIC
    sprc(spr_nr) = color
END SUB

SUB SpriteShape(spr_nr AS BYTE, shape AS BYTE) SHARED STATIC
    sprf(spr_nr) = shape
END SUB

SUB SpriteAt(spr_nr AS BYTE, x AS BYTE, y AS BYTE) SHARED STATIC
    sprx(spr_nr) = x
    spry(spr_nr) = y
END SUB

SUB SpriteMove(spr_nr AS BYTE, dx AS BYTE, dy AS BYTE) SHARED STATIC
    sprx(spr_nr) = sprx(spr_nr) + dx
    spry(spr_nr) = spry(spr_nr) + dy
END SUB

SUB SpriteUpdate() SHARED STATIC
    ASM
                inc {sprupdateflag}             ;Signal to IRQ: sort the
                                                ;sprites
waitloop:       lda {sprupdateflag}             ;Wait until the flag turns back
                bne waitloop                    ;to zero
    END ASM
END SUB

SUB SpriteInit() SHARED STATIC
    numsprites = MAX_NUM_SPRITES
    sortedsprites = 0
    sprupdateflag = 0
    FOR t AS BYTE = 0 TO MAX_NUM_SPRITES
        sortorder(t) = t
    NEXT t

    ASM
        ;Routine to init the raster interrupt system
initraster:
                lda {SprPtrAddr}
                sta irq2_sprf+1
                lda {SprPtrAddr}+1
                sta irq2_sprf+2

                sei
                lda #<irq1
                sta $0314
                lda #>irq1
                sta $0315
                lda #$7f                        ;CIA interrupt off
                sta $dc0d
                lda #$01                        ;Raster interrupt on
                sta $d01a
                lda $d011
                and #%01111111                  ;High bit of interrupt position = 0
                sta $d011
                lda #IRQ1LINE                   ;Line where next IRQ happens
                sta $d012
                lda $dc0d                       ;Acknowledge IRQ (to be sure)
                cli
                rts

IRQ1LINE        = $fc                           ;This is the place on screen where the sorting IRQ happens
MAXSPR          = 16                            ;Maximum number of sprites


        ;Raster interrupt 1. This is where sorting happens.

irq1:           dec $d019                       ;Acknowledge raster interrupt
                lda #$ff                        ;Move all sprites
                sta $d001                       ;to the bottom to prevent
                sta $d003                       ;weird effects when sprite
                sta $d005                       ;moves lower than what it
                sta $d007                       ;previously was
                sta $d009
                sta $d00b
                sta $d00d
                sta $d00f

                lda {sprupdateflag}             ;New sprites to be sorted?
                beq irq1_nonewsprites
                lda #$00
                sta {sprupdateflag}
                lda {numsprites}                ;Take number of sprites given by the main program
                sta {sortedsprites}             ;If it's zero, don't need to
                bne irq1_beginsort              ;sort

irq1_nonewsprites:
                ldx {sortedsprites}
                cpx #$09
                bcc irq1_notmorethan8
                ldx #$08
irq1_notmorethan8:
                lda d015tbl,x                   ;Now put the right value to
                sta $d015                       ;$d015, based on number of
                beq irq1_nospritesatall         ;sprites
                                                ;Now init the sprite-counter
                lda #$00                        ;for the actual sprite display
                sta {sprirqcounter}             ;routine
                lda #<irq2                      ;Set up the sprite display IRQ
                sta $0314
                lda #>irq2
                sta $0315
                jmp irq2_direct                 ;Go directly; we might be late
irq1_nospritesatall:
                jmp $ea81

irq1_beginsort: ;inc $d020
                ldx #MAXSPR
                dex
                cpx {sortedsprites}
                bcc irq1_cleardone
                lda #$ff                        ;Mark unused sprites with the
irq1_clearloop: sta {spry},x                    ;lowest Y-coordinate ($ff);
                dex                             ;these will "fall" to the
                cpx {sortedsprites}             ;bottom of the sorted table
                bcs irq1_clearloop
irq1_cleardone: ldx #$00
irq1_sortloop:  ldy {sortorder}+1,x             ;Sorting code. Algorithm
                lda {spry},y                    ;ripped from Dragon Breed :-)
                ldy {sortorder},x
                cmp {spry},y
                bcs irq1_sortskip
                stx irq1_sortreload+1
irq1_sortswap:  lda {sortorder}+1,x
                sta {sortorder},x
                sty {sortorder}+1,x
                cpx #$00
                beq irq1_sortreload
                dex
                ldy {sortorder}+1,x
                lda {spry},y
                ldy {sortorder},x
                cmp {spry},y
                bcc irq1_sortswap
irq1_sortreload:ldx #$00
irq1_sortskip:  inx
                cpx #MAXSPR-1
                bcc irq1_sortloop
                ldx {sortedsprites}
                lda #$ff                        ;$ff is the endmark for the
                sta sortspry,x                  ;sprite interrupt routine
                ldx #$00
irq1_sortloop3: ldy {sortorder},x               ;Final loop:
                lda {spry},y                    ;Now copy sprite variables to
                sta sortspry,x                  ;the sorted table
                lda {sprx},y
                sta sortsprx,x
                lda {sprf},y
                sta sortsprf,x
                lda {sprc},y
                sta sortsprc,x

                inx
                cpx {sortedsprites}
                bcc irq1_sortloop3
                ;dec $d020
                jmp irq1_nonewsprites

        ;Raster interrupt 2. This is where sprite displaying happens

irq2:           dec $d019                       ;Acknowledge raster interrupt
irq2_direct:    ldy {sprirqcounter}             ;Take next sorted sprite number
                lda sortspry,y                  ;Take Y-coord of first new sprite
                clc
                adc #$10                        ;16 lines down from there is
                bcc irq2_notover                ;the endpoint for this IRQ
                lda #$ff                        ;Endpoint can�t be more than $ff
irq2_notover:   sta {tempvariable}
irq2_spriteloop:lda sortspry,y
                cmp {tempvariable}              ;End of this IRQ?
                bcs irq2_endspr
                ldx physicalsprtbl2,y           ;Physical sprite number x 2
                sta $d001,x                     ;for X & Y coordinate
                lda sortsprx,y
                asl
                sta $d000,x
                bcc irq2_lowmsb
                lda $d010
                ora ortbl,x
                sta $d010
                jmp irq2_msbok
irq2_lowmsb:    lda $d010
                and andtbl,x
                sta $d010
irq2_msbok:     ldx physicalsprtbl1,y           ;Physical sprite number x 1
                lda sortsprf,y
irq2_sprf:
                sta {SprPtrAddr},x              ;for color & frame
                lda sortsprc,y
                sta $d027,x
                iny
                bne irq2_spriteloop
irq2_endspr:    cmp #$ff                        ;Was it the endmark?
                beq irq2_lastspr
                sty {sprirqcounter}
                sec                             ;That coordinate - $10 is the
                sbc #$10                        ;position for next interrupt
                cmp $d012                       ;Already late from that?
                bcc irq2_direct                 ;Then go directly to next IRQ
                sta $d012
                jmp $ea81
irq2_lastspr:   lda #<irq1                      ;Was the last sprite,
                sta $0314                       ;go back to irq1
                lda #>irq1                      ;(sorting interrupt)
                sta $0315
                lda #IRQ1LINE
                sta $d012
                jmp $ea81

sortsprx:       ds.b MAXSPR,0                   ;Sorted sprite table
sortspry:       ds.b MAXSPR+1,0                 ;Must be one byte extra for the $ff endmark
sortsprc:       ds.b MAXSPR,0
sortsprf:       ds.b MAXSPR,0


d015tbl:        dc.b %00000000                  ;Table of sprites that are "on"
                dc.b %00000001                  ;for $d015
                dc.b %00000011
                dc.b %00000111
                dc.b %00001111
                dc.b %00011111
                dc.b %00111111
                dc.b %01111111
                dc.b %11111111

physicalsprtbl1:dc.b 0,1,2,3,4,5,6,7            ;Indexes to frame & color
                dc.b 0,1,2,3,4,5,6,7            ;registers
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7
                dc.b 0,1,2,3,4,5,6,7

physicalsprtbl2:dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14
                dc.b 0,2,4,6,8,10,12,14

andtbl:         dc.b 255-1
ortbl:          dc.b 1
                dc.b 255-2
                dc.b 2
                dc.b 255-4
                dc.b 4
                dc.b 255-8
                dc.b 8
                dc.b 255-16
                dc.b 16
                dc.b 255-32
                dc.b 32
                dc.b 255-64
                dc.b 64
                dc.b 255-128
                dc.b 128
    END ASM
END SUB
