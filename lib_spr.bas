'INCLUDE "lib_memory.bas"
'INCLUDE "lib_types.bas"
'INCLUDE "lib_irq.bas"

REM **************************************
REM 16 / 24 / 32 (only 16 tested)
REM You must also update MAXSPR in ASM code
REM **************************************
SHARED CONST MAX_NUM_SPRITES = 16

REM **************************************
REM Modes for SprInit(Mode)
REM You must also update MAXSPR in ASM code
REM **************************************
SHARED CONST SPR_MODE_8 = 8
SHARED CONST SPR_MODE_16 = 16

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
DECLARE SUB SprInit(Mode AS BYTE, VicBankPtr AS BYTE, ScreenMemPtr AS BYTE) SHARED STATIC

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
REM Enable/disable individual sprites
REM ****************************************************************************
DECLARE SUB SprEnable(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION SprEnable AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD

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
DECLARE SUB SprBoundingBox(SprNr AS BYTE, Left AS BYTE, Top AS BYTE, Right AS BYTE, Bottom AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Set TRUE/FALSE property for all prites. Works in both modes.
REM ****************************************************************************
DECLARE SUB SprEnableAll(Value AS BYTE) SHARED STATIC
DECLARE SUB SprDoubleXAll(Value AS BYTE) SHARED STATIC
DECLARE SUB SprDoubleYAll(Value AS BYTE) SHARED STATIC
DECLARE SUB SprPriorityAll(Value AS BYTE) SHARED STATIC
DECLARE SUB SprMultiColorAll(Value AS BYTE) SHARED STATIC

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
DIM SHARED SprBoundingBoxLeft(MAX_NUM_SPRITES) AS BYTE
DIM SHARED SprBoundingBoxRight(MAX_NUM_SPRITES) AS BYTE
DIM SHARED SprBoundingBoxTop(MAX_NUM_SPRITES) AS BYTE
DIM SHARED SprBoundingBoxBottom(MAX_NUM_SPRITES) AS BYTE

REM ****************************************************************************
REM SPR_MODE_8 ONLY - R/W individual TRUE/FALSE sprite properties
REM ****************************************************************************
DIM SHARED SprDoubleX(MAX_NUM_SPRITES) AS BYTE
DIM SHARED SprDoubleY(MAX_NUM_SPRITES) AS BYTE
DIM SHARED SprMultiColor(MAX_NUM_SPRITES) AS BYTE
DIM SHARED SprPriority(MAX_NUM_SPRITES) AS BYTE


REM ****************************************************************************
REM  INTERNAL - INTERNAL - INTERNAL - INTERNAL - INTERNAL - INTERNAL - INTERNAL 
REM ****************************************************************************
DECLARE SUB spr_mode8_init() STATIC
DECLARE SUB spr_mode16_init() STATIC

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
DIM spr_y(MAX_NUM_SPRITES) AS BYTE
DIM spr_yy(MAX_NUM_SPRITES) AS BYTE 
DIM spr_e(MAX_NUM_SPRITES) AS BYTE

REM FAST variables for sprite multiplexing
DIM sprupdateflag AS BYTE FAST
DIM sortedsprites AS BYTE FAST
DIM tempvariable AS BYTE FAST
DIM sprirqcounter AS BYTE FAST

DIM sortorder(MAX_NUM_SPRITES) AS BYTE FAST

SUB SprEnableAll(Value AS BYTE) SHARED STATIC
    FOR ZP_B0 = 0 to spr_num_sprites - 1
        CALL SprEnable(ZP_B0, Value)
    NEXT
END SUB

SUB SprDoubleXAll(Value AS BYTE) SHARED STATIC
    spr_reg_dx = Value
    MEMSET @SprDoubleX, MAX_NUM_SPRITES, Value
END SUB

SUB SprDoubleYAll(Value AS BYTE) SHARED STATIC
    spr_reg_dy = Value
    MEMSET @SprDoubleY, MAX_NUM_SPRITES, Value
END SUB

SUB SprMultiColorAll(Value AS BYTE) SHARED STATIC
    spr_reg_mc = Value
    MEMSET @SprMultiColor, MAX_NUM_SPRITES, Value
END SUB

SUB SprPriorityAll(Value AS BYTE) SHARED STATIC
    spr_reg_bg = Value
    MEMSET @SprPriority, MAX_NUM_SPRITES, Value
END SUB

SUB SprInit(Mode AS BYTE, VicBankPtr AS BYTE, ScreenMemPtr AS BYTE) SHARED STATIC
    spr_vic_bank_ptr = VicBankPtr
    spr_vic_bank_addr = 16384 * CWORD(VicBankPtr)

    spr_num_sprites = Mode
    spr_ptrs = spr_vic_bank_addr + SHL(CWORD(ScreenMemPtr), 10) + 1016

    FOR ZP_B0 = 0 TO spr_num_sprites-1
        spr_x(ZP_B0) = 0
        spr_y(ZP_B0) = 255
        spr_yy(ZP_B0) = 255
        spr_e(ZP_B0) = 0

        SprColor(ZP_B0) = 1
        SprBoundingBoxLeft(ZP_B0) = 0
        SprBoundingBoxRight(ZP_B0) = 12
        SprBoundingBoxTop(ZP_B0) = 0
        SprBoundingBoxBottom(ZP_B0) = 21
        SprCollision(ZP_B0) = 0
        SprDoubleX(ZP_B0) = 0
        SprDoubleY(ZP_B0) = 0
        SprMultiColor(ZP_B0) = 0
        SprPriority(ZP_B0) = 0
    NEXT ZP_B0

    IF spr_num_sprites < 9 THEN
        CALL spr_mode8_init()
    ELSE
        CALL spr_mode16_init()
    END IF
END SUB

SUB SprBoundingBox(SprNr AS BYTE, Left AS BYTE, Top AS BYTE, Right AS BYTE, Bottom AS BYTE) SHARED STATIC
    SprBoundingBoxLeft(SprNr) = SHR(Left, 1)
    SprBoundingBoxRight(SprNr) = SHR(Right, 1)
    SprBoundingBoxTop(SprNr) = Top
    SprBoundingBoxBottom(SprNr) = Bottom
END SUB

SUB SprStop() SHARED STATIC
    CALL IrqSpr(0)
END SUB

SUB SprClearFrame(FramePtr AS BYTE) SHARED STATIC
    MEMSET spr_vic_bank_addr + SHL(CWORD(FramePtr), 6), 63, 0
END SUB

SUB SprEnable(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
    IF Value THEN 
        IF spr_e(SprNr) = 0 THEN
            spr_y(SprNr) = spr_yy(SprNr)
            spr_e(SprNr) = $ff
        END IF
    ELSE
        IF spr_e(SprNr) = $ff THEN
            spr_y(SprNr) = $ff
            spr_e(SprNr) = $00
        END IF
    END IF
END SUB

FUNCTION SprEnable AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_e(SprNr)
END FUNCTION

SUB SprDxDy(SprNr AS BYTE, dx AS BYTE, dy AS BYTE) SHARED STATIC
    ASM
        ldx {SprNr}

        clc                     ; spr_reg_xy(SprNr).y = y + 50
        lda {spr_yy},x
        adc {dy}
        sta {spr_yy},x          ; spr_y(SprNr) = y + 50
        
        ldy {spr_e},x
        beq spr_dxdy_x

        sta {spr_y},x

spr_dxdy_x:
        CLC                     ; preserve sign bit
        LDA {dx}
        BPL spr_dxdy_positive
        SEC
spr_dxdy_positive:
        ROR

        clc
        adc {spr_x},x

        cmp #252
        bcc spr_dxdy_no_bad_zone

        sec                     ; THEN x -= 8
        sbc #4

spr_dxdy_no_bad_zone:
        sta {spr_x},x
    END ASM
END SUB

SUB SprXY(SprNr AS BYTE, x AS WORD, y AS BYTE) SHARED STATIC
    ASM
        ldx {SprNr}

        clc                     ; spr_reg_xy(SprNr).y = y + 50
        lda {y}
        adc #50
        sta {spr_yy},x           ; spr_y(SprNr) = y + 50
        
        ldy {spr_e},x
        beq sprxy_x

        sta {spr_y},x

sprxy_x:
        lda {x}+1
        lsr
        lda {x}
        ror

        clc
        adc #12
        
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
        lda {spr_e},y
        bne spr_collision_loop

spr_collision_disabled_loop:
        sta {SprCollision},x
        dex
        bpl spr_collision_disabled_loop

        jmp spr_collision_end

spr_collision_loop:
        lda {spr_e},x
        beq spr_collision_false

spr_collision_check_y:
        sec
        lda {spr_y},x                       ; Load Enemy Y position
        sbc {spr_y},y                       ; Subtract Player Y position
        bcs spr_collision_dy_positive       ; enemy_x >= player_x
        eor #$ff                            ; Negate result
        adc #1                              ; carry must be clear
                                            ; - absolute distance from top-left to top-left is in a
                                            ; - player is right from enemy
spr_collision_dy_negative:
        clc
        adc {SprBoundingBoxTop},y
        bcs spr_collision_false

        sec
        sbc {SprBoundingBoxBottom},x
        bcs spr_collision_false

        jmp spr_collision_check_x

spr_collision_dy_positive:
        clc
        adc {SprBoundingBoxTop},x
        bcs spr_collision_false

        sec
        sbc {SprBoundingBoxBottom},y
        bcs spr_collision_false

spr_collision_check_x:
        sec
        lda {spr_x},x                       ; Compare x coordinates
        sbc {spr_x},y                       ; Subtract Player X position
        bcs spr_collision_dx_positive
        eor #$ff                            ; Negate result
        adc #1

spr_collision_dx_negative:
        clc
        adc {SprBoundingBoxLeft},y
        bcs spr_collision_false

        sec
        sbc {SprBoundingBoxRight},x
        bcs spr_collision_false

        jmp spr_collision_true

spr_collision_dx_positive:
        clc
        adc {SprBoundingBoxLeft},x
        bcs spr_collision_false

        sec
        sbc {SprBoundingBoxRight},y
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
END SUB

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
                inc {sprupdateflag}              ;Signal to IRQ: sort the
                lda {blocking}                   ;sprites
                beq non_blocking
waitloop:       lda {sprupdateflag}             ;Wait until the flag turns back
                bne waitloop                    ;to zero
non_blocking:
    END ASM
END SUB

SUB spr_mode8_init() STATIC
    ASM
        lda {spr_ptrs}
        sta mode8_irq_sprf+1
        lda {spr_ptrs}+1
        sta mode8_irq_sprf+2

        lda #<mode8_irq
        sta {ZP_W0}
        lda #>mode8_irq
        sta {ZP_W0}+1

        jmp mode8_end
;-----------------------------------
mode8_irq:
;-----------------------------------
        ;inc $d020
        lda {sprupdateflag}                 ;Update sprite properties
        beq mode8_irq_exit
        lda #$00
        sta {sprupdateflag}

        ;inc $d020                          ; debug
        ldy #7
        ldx #14
mode8_irq_loop:
        lda {spr_x},y
        asl
        sta $d000,x
        rol $d010

        lda {spr_y},y
        sta $d001,x

        lda {SprColor},y
        sta $d027,y

        lda {SprFrame},y
mode8_irq_sprf:
        sta {spr_ptrs},y

        lda {spr_e},y
        rol
        rol $d015

        lda {SprDoubleX},y
        rol
        rol $d01d

        lda {SprDoubleY},y
        rol
        rol $d017

        lda {SprMultiColor},y
        rol
        rol $d01c

        lda {SprPriority},y
        rol
        rol $d01b

        dex
        dex
        dey
        bpl mode8_irq_loop
        ;dec $d020                       ; debug

mode8_irq_exit:
        jmp ({irq_spr_return_addr})
;-----------------------------------
mode8_end:
;-----------------------------------
    END ASM
    CALL IrqSpr(ZP_W0)
END SUB

SUB spr_mode16_init() STATIC
    sortedsprites = 0
    sprupdateflag = 0
    FOR t AS BYTE = 0 TO spr_num_sprites-1
        sortorder(t) = t
    NEXT t

    ASM
MAXSPR          = 16                            ;Maximum number of sprites
IRQ1LINE        = $fc                           ;This is the place on screen where the sorting IRQ happens

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
