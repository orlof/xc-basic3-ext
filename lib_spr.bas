'INCLUDE "lib_memory.bas"
'INCLUDE "lib_types.bas"
'INCLUDE "lib_irq.bas"

REM **************************************
REM 16 / 24 / 32 (only 16 tested)
REM You must also update MAXSPR in ASM code
REM **************************************
SHARED CONST MAX_NUM_SPRITES = 16

REM ****************************************************************************
REM Initialise Sprite System
REM 
REM Must be called AFTER VIC-bank and Screen Memory address are set, 
REM but BEFORE any other methods are used.
REM 
REM Mode can be SPR_MODE_8 or SPR_MODE_16.
REM  - Invidividual sprite's MultiColorMode, Priority, DoubleWidth or
REM    DoubleHeight can only be set in 8 sprite mode. e.g.
REM     - SprDoubleX(0) = TRUE
REM  - 16 sprite mode can use only:
REM     - SprDoubleXAll(TRUE)
REM     - SprDoubleYAll(TRUE)
REM     - SprMultiColorAll(TRUE)
REM     - SprPriorityAll(TRUE)
REM 
REM NOTE Both modes intialise a raster interrupt that updates sprite data
REM      from cache arrays to IO registers during off-screen. Update is
REM      triggered with SprUpdate.
REM ****************************************************************************
DECLARE SUB SprInit(VicBankPtr AS BYTE, ScreenMemPtr AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Commit all changes from cache arrays to IO registers.
REM Blocking=TRUE waits until the next raster interrupt is completed.
REM ****************************************************************************
DECLARE SUB SprUpdate(Blocking AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Clears sprite frame.
REM ****************************************************************************
DECLARE SUB SprClearFrame(FramePtr AS BYTE) SHARED STATIC

REM ****************************************************************************
REM CALL SprXY(0, 0, 0)
REM ****************************************************************************
REM Set sprite's top left corner to screen pixel (x, y).
REM Bits [8:0] are used for x coordinate.
REM 
REM                 left border   fully visible   right border
REM x normal          x <= -24    0 <= x < 296      x >= 320
REM x expanded        x <= -48    0 <= x < 272      x >= 320
REM 
REM                  top border   fully visible   bottom border
REM y normal          y <= -21    0 <= y < 235      y >= 200
REM y expanded        y <= -42    0 <= y < 214      y >= 200
REM 
REM ****************************************************************************
REM NOTE If you need collision detection, you MUST ALWAYS use this method to 
REM set sprite position!
REM ****************************************************************************
DECLARE SUB SprXY(SprNr AS BYTE, x AS WORD, y AS BYTE) SHARED STATIC
'DECLARE SUB SprBoundingBox(SprNr AS BYTE, Left AS BYTE, Top AS BYTE, Right AS BYTE, Bottom AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Set TRUE/FALSE property for all prites. Works in both modes.
REM ****************************************************************************
DECLARE SUB SprDisable() SHARED STATIC
DECLARE SUB SprPriority(Value AS BYTE) SHARED STATIC
DECLARE SUB SprMultiColor(Value AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Update SprCollision array with TRUE/FALSE values to identify which
REM sprites collide with SprNr
REM 
REM CALL SprRecordCollisions(0)
REM FOR t AS BYTE = 1 TO 7
REM     PRINT t, SprCollision(t)
REM NEXT t
REM ****************************************************************************
DECLARE FUNCTION SprRecordCollisions AS BYTE(SprNr AS BYTE) SHARED STATIC

REM **************************************
REM PUBLIC FIELDS
REM **************************************

REM ****************************************************************************
REM Collision data (TRUE/FALSE) for each sprite
REM Update by "CALL SprRecordCollisions(SprNr)" 
REM ****************************************************************************
DIM SHARED SprCollision(MAX_NUM_SPRITES) AS BYTE

REM ****************************************************************************
REM Direct R/W to Multicolor registers e.g.
REM SprMultiColor1 = (MultiColor1 + 1) AND %00001111
REM ****************************************************************************
DIM SHARED SprMultiColor1 AS BYTE @$d025
DIM SHARED SprMultiColor2 AS BYTE @$d026

REM ****************************************************************************
REM R/W individual sprite properties
REM ****************************************************************************
DIM SHARED SprColor(MAX_NUM_SPRITES) AS BYTE
DIM SHARED SprFrame(MAX_NUM_SPRITES) AS BYTE

REM Collision Detection
DIM SHARED Spr_EdgeWest(MAX_NUM_SPRITES) AS BYTE
DIM SHARED Spr_EdgeEast(MAX_NUM_SPRITES) AS BYTE
DIM SHARED Spr_EdgeNorth(MAX_NUM_SPRITES) AS BYTE
DIM SHARED Spr_EdgeSouth(MAX_NUM_SPRITES) AS BYTE

DIM spr_ptrs AS WORD
    spr_ptrs = 2040

DIM SHARED spr_num_sprites AS BYTE
    spr_num_sprites = MAX_NUM_SPRITES

DIM SHARED spr_vic_bank_ptr AS BYTE
DIM SHARED spr_vic_bank_addr AS WORD

DIM spr_reg_mc AS BYTE @$d01c
DIM spr_reg_dx AS BYTE @$d01d
DIM spr_reg_dy AS BYTE @$d017
DIM spr_reg_bg AS BYTE @$d01b

DIM spr_x(MAX_NUM_SPRITES) AS BYTE
DIM spr_y(MAX_NUM_SPRITES) AS BYTE SHARED

REM FAST variables for sprite multiplexing
DIM sprupdateflag AS BYTE FAST
DIM sortedsprites AS BYTE FAST
DIM tempvariable AS BYTE FAST
DIM sprirqcounter AS BYTE FAST

DIM sortorder(MAX_NUM_SPRITES) AS BYTE FAST

SUB SprDisable() SHARED STATIC
    ASM
        lda #$ff
        ldx {spr_num_sprites}

spr_enable_all_loop
        dex
        bmi spr_enable_all_end
        sta {spr_y},x
        jmp spr_enable_all_loop

spr_enable_all_end
    END ASM
END SUB

SUB SprMultiColor(Value AS BYTE) SHARED STATIC
    spr_reg_mc = Value
END SUB

SUB SprPriority(Value AS BYTE) SHARED STATIC
    spr_reg_bg = Value
END SUB

SUB SprInit(VicBankPtr AS BYTE, ScreenMemPtr AS BYTE) SHARED STATIC
    ASM
        ;spr_vic_bank_ptr = VicBankPtr
        lda {VicBankPtr}
        sta {spr_vic_bank_ptr}

        ;spr_vic_bank_addr = 16384 * CWORD(VicBankPtr)
        lda #0
        sta {spr_vic_bank_addr}

        lda {VicBankPtr}        ;16384 * CWORD(VicBankPtr)
        lsr
        ror
        ror
        sta {spr_vic_bank_addr}+1

        ; spr_ptrs = spr_vic_bank_addr + SHL(CWORD(ScreenMemPtr), 10) + 1016
        lda {ScreenMemPtr}      ;vic_bank_addr + 1024 * ScreenMemPtr
        asl
        asl

        clc
        adc {spr_vic_bank_addr}+1
        adc #3
        sta {spr_ptrs}+1
        lda #$f8
        sta {spr_ptrs}

        ;-----------------------------
        ;init sprite properties
        ldx {spr_num_sprites}

spr_init_loop
        dex
        bmi spr_init_end

        lda #0
        sta {spr_x},x
        sta {Spr_EdgeWest},x
        sta {Spr_EdgeNorth},x
        sta {SprCollision},x

        lda #1
        sta {SprColor},x

        lda #12
        sta {Spr_EdgeEast},x

        lda #21
        sta {Spr_EdgeSouth},x

        lda #255
        sta {spr_y},x

        jmp spr_init_loop
spr_init_end
    END ASM

    ASM
MAXSPR          = 16                            ;Maximum number of sprites
IRQ1LINE        = $fc                           ;This is the place on screen where the sorting IRQ happens
        lda #0
        sta {sortedsprites}
        sta {sprupdateflag}

        ldx {spr_num_sprites}
spr_mode16_init_loop
        dex
        bmi spr_mode16_init_loop_break

        txa
        sta {sortorder},x
        jmp spr_mode16_init_loop

spr_mode16_init_loop_break
        lda {spr_ptrs}
        sta irq2_sprf+1
        lda {spr_ptrs}+1
        sta irq2_sprf+2

        lda #<mode16_irq
        sta {ZP_W0}
        lda #>mode16_irq
        sta {ZP_W0}+1

        jmp mode16_end

;Raster interrupt 1. This is where sorting happens.
;-----------------------------------
mode16_irq:
;-----------------------------------
;                dec $d019                       ;Acknowledge raster interrupt
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
                lda {spr_num_sprites}                ;Take number of sprites given by the main program
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

irq1_beginsort:
                ;inc $d020                      ; debug
                ldx #$00
irq1_sortloop:
                ldy {sortorder}+1,x             ;Sorting code. Algorithm
                lda {spr_y},y                    ;ripped from Dragon Breed :-)
                ldy {sortorder},x
                cmp {spr_y},y
                bcs irq1_sortskip
                stx irq1_sortreload+1
irq1_sortswap:  lda {sortorder}+1,x
                sta {sortorder},x
                sty {sortorder}+1,x
                cpx #$00
                beq irq1_sortreload
                dex
                ldy {sortorder}+1,x
                lda {spr_y},y
                ldy {sortorder},x
                cmp {spr_y},y
                bcc irq1_sortswap
irq1_sortreload:
                ldx #$00
irq1_sortskip:
                inx
                cpx #MAXSPR-1
                bcc irq1_sortloop
                ldx {sortedsprites}
                lda #$ff                        ;$ff is the endmark for the
                sta sortspry,x                  ;sprite interrupt routine
                ldx #$00
irq1_sortloop3: ldy {sortorder},x               ;Final loop:
                lda {spr_y},y                    ;Now copy sprite variables to
                sta sortspry,x                  ;the sorted table
                lda {spr_x},y
                sta sortsprx,x
                lda {SprFrame},y
                sta sortsprf,x
                lda {SprColor},y
                sta sortsprc,x

                inx
                cpx {sortedsprites}
                bcc irq1_sortloop3
                ;dec $d020                       ; debug
                jmp irq1_nonewsprites

;Raster interrupt 2. This is where sprite displaying happens
;-----------------------------------
irq2:
;-----------------------------------
                dec $d019                       ;Acknowledge raster interrupt
irq2_direct:    ldy {sprirqcounter}             ;Take next sorted sprite number
                lda sortspry,y                  ;Take Y-coord of first new sprite
                clc
                adc #$10                        ;16 lines down from there is
                bcc irq2_notover                ;the endpoint for this IRQ
                lda #$ff                        ;Endpoint cant be more than $ff
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
irq2_msbok:     
                ldx physicalsprtbl1,y           ;Physical sprite number x 1
                lda sortsprf,y
irq2_sprf:
                sta $dead,x                     ;for color & frame
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
irq2_lastspr:   lda {IrqHandler}               ;Was the last sprite,
                sta $0314                       ;go back to irq1
                lda {IrqHandler}+1               ;(sorting interrupt)
                sta $0315
                lda #IRQ1LINE
                sta $d012
                jmp $ea81


sortsprx:       ds.b MAXSPR,0                   ;Sorted sprite table
sortspry:       ds.b MAXSPR+1,0                 ;Must be one byte extra for the $ff endmark
sortsprc:       ds.b MAXSPR,0
sortsprf:       ds.b MAXSPR,0
sortsprx2:      ds.b MAXSPR,0


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
mode16_end:
    END ASM
    CALL IrqSpr(ZP_W0)
END SUB

SUB SprStop() SHARED STATIC
    CALL IrqSpr(0)
END SUB

SUB SprClearFrame(FramePtr AS BYTE) SHARED STATIC
    MEMSET spr_vic_bank_addr + SHL(CWORD(FramePtr), 6), 63, 0
END SUB

SUB SprXY(SprNr AS BYTE, x AS WORD, y AS BYTE) SHARED STATIC
    ASM
        ldx {SprNr}

        clc                     ; spr_reg_xy(SprNr).y = y + 50
        lda {y}
        adc #40
        sta {spr_y},x

sprxy_x:
        lsr {x}+1
        lda {x}
        ror

        clc
        adc #6
        
        cmp #252
        bcc spr_xy_no_bad_zone

        sec                     ; THEN x -= 8
        sbc #4

spr_xy_no_bad_zone:
        sta {spr_x},x
    END ASM
END SUB

FUNCTION SprRecordCollisions AS BYTE(SprNr AS BYTE) SHARED STATIC
    ASM
        lda #0
        sta {SprRecordCollisions}

        ldy {SprNr}
        ldx {spr_num_sprites}
        dex
        lda {spr_y},y
        cmp #$ff
        bne spr_collision_loop

        lda #0
spr_collision_disabled_loop:
        sta {SprCollision},x
        dex
        bpl spr_collision_disabled_loop

        jmp spr_collision_end

spr_collision_loop:
        lda {spr_y},x
        cmp #$ff
        beq spr_collision_false

spr_collision_check_y:
        sec
        ;lda {spr_y},x                       ; Load Enemy Y position
        sbc {spr_y},y                       ; Subtract Player Y position
        bcs spr_collision_enemy_is_lower    ; enemy_y >= player_y
        eor #$ff                            ; Negate result
        adc #1                              ; carry must be clear
                                            ; - absolute distance from top-left to top-left is in a
                                            ; - player is right from enemy
spr_collision_enemy_is_higher
        sec
        sbc {Spr_EdgeSouth},x
        bcc spr_collision_check_x
        sbc {Spr_EdgeNorth},y
        bcs spr_collision_false
        bcc spr_collision_check_x

spr_collision_enemy_is_lower
        sec
        sbc {Spr_EdgeNorth},x
        bcc spr_collision_check_x
        sbc {Spr_EdgeSouth},y
        bcs spr_collision_false

spr_collision_check_x
        sec
        lda {spr_x},x                       ; Enemy X coordinate
        sbc {spr_x},y                       ; Player X coordinate
        bcs spr_collision_enemy_is_right
        eor #$ff                            ; Negate result
        adc #1

spr_collision_enemy_is_left
        sec
        sbc {Spr_EdgeEast},x
        bcc spr_collision_true
        sbc {Spr_EdgeWest},y
        bcs spr_collision_false
        bcc spr_collision_true

spr_collision_enemy_is_right
        sec
        sbc {Spr_EdgeWest},x
        bcc spr_collision_true
        sbc {Spr_EdgeEast},y
        bcs spr_collision_false

spr_collision_true:
        lda #$ff
        dc.b $2c                            ; BIT instruction that skips next LDA

spr_collision_false:
        lda #$00
        sta {SprCollision},x

        ora {SprRecordCollisions}
        sta {SprRecordCollisions}

        dex                                 ; Goes to next sprite
        bpl spr_collision_loop

spr_collision_end:
        lda #0
        sta {SprCollision},y
    END ASM
END FUNCTION

REM **************************************
REM Spritemultiplexer adaptation
REM 
REM Based on:
REM Spritemultiplexing example V2.1
REM by Lasse Öörni (loorni@gmail.com)
REM Available at http://cadaver.github.io
REM **************************************

SUB SprUpdate(Blocking AS BYTE) SHARED STATIC
    ASM
waitloop1:      lda {sprupdateflag}         ;Wait until the flag turns back
                bne waitloop1               ;to zero
                inc {sprupdateflag}         ;Signal to IRQ: sort the
                lda {blocking}              ;sprites
                beq non_blocking
waitloop2:      lda {sprupdateflag}         ;Wait until the flag turns back
                bne waitloop2               ;to zero
non_blocking:
    END ASM
END SUB
