REM **************************************
REM LIBRARY CONVENTIONS
REM   TRUE  = $ff
REM   FALSE = $00
REM **************************************

REM **************************************
REM 16 / 24 / 32
REM You must also update MAXSPR in ASM code
CONST MAX_NUM_SPRITES = 16

REM ****************************************************************************
REM Must be called before other methods if VIC bank or videoram is moved
REM ****************************************************************************
DECLARE SUB SprInit() SHARED STATIC

REM ****************************************************************************
REM Define colors for multicolor sprites
REM ****************************************************************************
DECLARE SUB SprMultiColors(Color1 AS BYTE, Color2 AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Copies sprite pattern from DATA AS BYTE statements to address specified by 
REM given FramePtr (16384 * VIC_BANK + 64 * FramePtr = dest_address)
REM ****************************************************************************
DECLARE SUB SprImportShape(SrcAddr AS WORD, FramePtr AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Creates new pattern from existing pattern by mirroring it horizontally.
REM ****************************************************************************
DECLARE SUB SprFlipXShape(SrcFramePtr AS BYTE, DstFramePtr AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Creates new pattern from existing pattern by mirroring it vertically.
REM ****************************************************************************
DECLARE SUB SprFlipYShape(SrcFramePtr AS BYTE, DstFramePtr AS BYTE) SHARED STATIC

DECLARE SUB SprClearFrame(FramePtr AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Following methods set and get sprite properties
REM ****************************************************************************
DECLARE SUB SprConfig(SprNr AS BYTE, IsMultiColor AS BYTE, DoubleX AS BYTE, DoubleY AS BYTE, Background AS BYTE, Color AS BYTE) SHARED STATIC
DECLARE SUB SprEnable(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION SprEnable AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB SprDoubleX(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION SprDoubleX AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB SprDoubleY(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION SprDoubleY AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB SprFrame(SprNr AS BYTE, FramePtr AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION SprFrame AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD

REM ****************************************************************************
REM Set x, y position of sprite.
REM ****************************************************************************
DECLARE SUB SprXY(SprNr AS BYTE, x AS WORD, y AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Records sprite-sprite collisions with defined sprite to SprCollision array
REM ****************************************************************************
DECLARE SUB SprRecordCollisions(SprNr AS BYTE) SHARED STATIC

REM **************************************
REM PUBLIC FIELDS
REM **************************************

REM ****************************************************************************
REM "CALL SprRecordCollisions(SprNr)" updates collison data (TRUE/FALSE) for each sprite
REM ****************************************************************************
DIM SHARED SprCollision(MAX_NUM_SPRITES) AS BYTE

REM ****************************************************************************
REM INTERNAL helper to access io registers
REM ****************************************************************************
TYPE TYPE_SPR_REG
    x AS BYTE
    y AS BYTE
END TYPE

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM vic_bank_addr AS WORD
    vic_bank_addr = 0
DIM spr_ptrs AS WORD
    spr_ptrs = 2040

'DIM spr_reg_xy(MAX_NUM_SPRITES) AS TYPE_SPR_REG @$d000
'DIM spr_reg_msb AS BYTE @$d010
'DIM spr_reg_mc AS BYTE @$d01c
DIM spr_reg_mc1 AS BYTE @$d025
DIM spr_reg_mc2 AS BYTE @$d026
'DIM spr_reg_dx AS BYTE @$d01d
'DIM spr_reg_dy AS BYTE @$d017
'DIM spr_reg_back AS BYTE @$d01b
'DIM spr_reg_enable AS BYTE @$d015

REM **************************************
REM INTERNAL SPRITE DATA
REM **************************************
DIM SHARED spr_x(MAX_NUM_SPRITES) AS BYTE
DIM SHARED spr_y(MAX_NUM_SPRITES) AS BYTE 
DIM spr_c(MAX_NUM_SPRITES) AS BYTE
DIM spr_f(MAX_NUM_SPRITES) AS BYTE

DIM spr_yy(MAX_NUM_SPRITES) AS BYTE 
DIM SHARED spr_e(MAX_NUM_SPRITES) AS BYTE
DIM SHARED spr_w(MAX_NUM_SPRITES) AS BYTE
DIM SHARED spr_h(MAX_NUM_SPRITES) AS BYTE

REM **************************************
REM INTERNAL HELPER
REM **************************************
DIM ZP_W0 AS WORD FAST
DIM ZP_W1 AS WORD FAST

REM ****************************************************************************
REM CALL SprInit()
REM ****************************************************************************
REM Call before using the library if you change VIC bank or screen memory 
REM address
REM ****************************************************************************
SUB SprInit() SHARED STATIC
    vic_bank_addr = 16384 * ((PEEK($dd00) AND %00000011) XOR %00000011)
    spr_ptrs = vic_bank_addr + SHL(CWORD(PEEK($d018) AND %11110000), 6) + 1016

    FOR t AS BYTE = 0 TO MAX_NUM_SPRITES-1
        spr_w(t) = 12
        spr_h(t) = 21
        spr_x(t) = 0
        spr_y(t) = 255
        spr_yy(t) = 255
        spr_e(t) = 0
        SprCollision(t) = 0
    NEXT t
END SUB

REM ****************************************************************************
REM INCLUDE "color.bas"
REM CALL SprMultiColors(COLOR_BLACK, COLOR_WHITE)
REM ****************************************************************************
REM Set color values for multicolor sprites
REM ****************************************************************************
SUB SprMultiColors(Color1 AS BYTE, Color2 AS BYTE) SHARED STATIC
    spr_reg_mc1 = Color1
    spr_reg_mc2 = Color2
END SUB

REM ****************************************************************************
REM CALL SprImportShape(@PLAYER_SHIP, 255)
REM CALL SprFrame(0, 255)
REM ...
REM PLAYER_SHIP:
REM DATA AS BYTE 1,2,3,4,5...
REM ****************************************************************************
REM Copies sprite pattern from DATA AS BYTE statements to address specified by 
REM given FramePtr (16384 * VIC_BANK + 64 * FramePtr = dest_address)
REM [Developer is responsible that the area is free]
REM ****************************************************************************
SUB SprImportShape(SrcAddr AS WORD, FramePtr AS BYTE) SHARED STATIC
    ASM
        sei                     ; turn off interrupts  
        dec 1                   ; can use also io memory for sprites
        dec 1                   ; by disabling kernel and io
    END ASM
    MEMCPY SrcAddr, vic_bank_addr + SHL(CWORD(FramePtr), 6), 63
    ASM
        inc 1                   ; restore io, kernel and interrupts
        inc 1
        cli
    END ASM
END SUB

SUB SprClearFrame(FramePtr AS BYTE) SHARED STATIC
    ASM
        sei                     ; turn off interrupts  
        dec 1                   ; can use also io memory for sprites
        dec 1                   ; by disabling kernel and io
    END ASM
    MEMSET vic_bank_addr + SHL(CWORD(FramePtr), 6), 63, 0
    ASM
        inc 1                   ; restore io, kernel and interrupts
        inc 1
        cli
    END ASM
END SUB

REM ****************************************************************************
REM Creates new pattern from existing pattern by mirroring it horizontally.
REM ****************************************************************************
SUB SprFlipXShape(SrcFramePtr AS BYTE, DstFramePtr AS BYTE) SHARED STATIC
    ZP_W0 = vic_bank_addr + SHL(CWORD(SrcFramePtr), 6)
    ZP_W1 = vic_bank_addr + SHL(CWORD(DstFramePtr), 6)

    DIM tmp AS BYTE
    ASM
        ldy #60
flip_x_row:
        lda ({ZP_W0}),y
        sta {tmp}
        lda #128
flip_x_byte0:
        asl {tmp}
        ror
        bcc flip_x_byte0
        tax

        iny
        iny

        lda ({ZP_W0}),y
        sta {tmp}
        txa
        sta ({ZP_W1}),y
        lda #128
flip_x_byte2:
        asl {tmp}
        ror
        bcc flip_x_byte2
        tax

        dey

        lda ({ZP_W0}),y
        sta {tmp}
        lda #128
flip_x_byte1:
        asl {tmp}
        ror
        bcc flip_x_byte1
        sta ({ZP_W1}),y

        dey

        txa
        sta ({ZP_W1}),y

        dey
        dey
        dey
        bpl flip_x_row
    END ASM
END SUB

REM ****************************************************************************
REM Creates new pattern from existing pattern by mirroring it vertically.
REM ****************************************************************************
SUB SprFlipYShape(SrcFramePtr AS BYTE, DstFramePtr AS BYTE) SHARED STATIC
    ZP_W0 = vic_bank_addr + SHL(CWORD(SrcFramePtr), 6)
    ZP_W1 = vic_bank_addr + SHL(CWORD(DstFramePtr), 6) + 60
    FOR t AS BYTE = 0 TO 63
        POKE ZP_W1+2, PEEK(ZP_W0 + 2)
        POKE ZP_W1+1, PEEK(ZP_W0 + 1)
        POKE ZP_W1, PEEK(ZP_W0)
        ZP_W0 = ZP_W0 + 3
        ZP_W1 = ZP_W1 - 3
    NEXT t
END SUB

REM ****************************************************************************
REM INCLUDE "lib_color.bas"
REM INCLUDE "lib_types.bas"
REM CALL SprConfig(0, FALSE, FALSE, FALSE, FALSE, COLOR_WHITE)
REM ****************************************************************************
REM Configure all multiple sprite attributes with one call
REM ****************************************************************************
SUB SprConfig(SprNr AS BYTE, IsMultiColor AS BYTE, DoubleX AS BYTE, DoubleY AS BYTE, Background AS BYTE, Color AS BYTE) SHARED STATIC
    'DIM bit AS BYTE
    '    bit = bit_pos(SprNr)
    'IF IsMultiColor THEN
    '    spr_reg_mc = spr_reg_mc OR bit
    'ELSE
    '    spr_reg_mc = spr_reg_mc AND NOT bit
    'END IF
    'IF DoubleX THEN
    '    spr_reg_dx = spr_reg_dx OR bit
    '    spr_w(SprNr) = 24
    'ELSE
    '    spr_reg_dx = spr_reg_dx AND NOT bit
    '    spr_w(SprNr) = 12
    'END IF
    'IF DoubleY THEN
    '    spr_reg_dy = spr_reg_dy OR bit
    '    spr_h(SprNr) = 42
    'ELSE
    '    spr_reg_dy = spr_reg_dy AND NOT bit
    '    spr_h(SprNr) = 21
    'END IF
    'IF Background THEN
    '    spr_reg_back = spr_reg_back OR bit
    'ELSE
    '    spr_reg_back = spr_reg_back AND NOT bit
    'END IF
    spr_c(SprNr) = Color
    'SprColor(SprNr) = Color
END SUB

REM ****************************************************************************
REM INCLUDE "lib_types.bas"
REM CALL SprEnable(0, TRUE)
REM ****************************************************************************
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

REM ****************************************************************************
REM IF SprEnable(0) THEN PRINT "0: enabled"
REM ****************************************************************************
FUNCTION SprEnable AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_e(SprNr)
    'RETURN spr_reg_enable AND bit_pos(SprNr) <> 0
END FUNCTION

REM ****************************************************************************
REM INCLUDE "lib_types.bas"
REM CALL SprDoubleX(0, TRUE)
REM ****************************************************************************
SUB SprDoubleX(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
'    IF Value THEN
'        spr_reg_dx = spr_reg_dx OR bit_pos(SprNr)
'    ELSE
'        spr_reg_dx = spr_reg_dx AND NOT bit_pos(SprNr)
'    END IF
END SUB

REM ****************************************************************************
REM IF SprDoubleX(0) THEN PRINT "width: 48"
REM ****************************************************************************
FUNCTION SprDoubleX AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN 0
'    RETURN spr_reg_dx AND bit_pos(SprNr) <> 0
END FUNCTION

REM ****************************************************************************
REM INCLUDE "lib_types.bas"
REM CALL SprDoubleY(0, TRUE)
REM ****************************************************************************
SUB SprDoubleY(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
'    IF Value THEN
'        spr_reg_dy = spr_reg_dy OR bit_pos(SprNr)
'    ELSE
'        spr_reg_dy = spr_reg_dy AND NOT bit_pos(SprNr)
'    END IF
END SUB

REM ****************************************************************************
REM IF SprDoubleY(0) THEN PRINT "height: 42"
REM ****************************************************************************
FUNCTION SprDoubleY AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN 0
'    RETURN spr_reg_dy AND bit_pos(SprNr) <> 0
END FUNCTION

REM ****************************************************************************
REM CALL spr_shape(0, 192)
REM ****************************************************************************
REM Set sprite's FramePtr (16384 * VIC_BANK + 64 * FramePtr = dest_address)
REM Make sure to place the sprite pattern data to this memory area either
REM with "ORIGIN" -directive or by "CALL SprImportShape(SrcAddr)""
REM ****************************************************************************
SUB SprFrame(SprNr AS BYTE, FramePtr AS BYTE) SHARED STATIC OVERLOAD
    spr_f(SprNr) = FramePtr
    'POKE spr_ptrs + SprNr, FramePtr
END SUB

REM ****************************************************************************
REM PRINT "Sprite 0 address "; 64 * SprFrame(0)
REM ****************************************************************************
REM Returns sprite's FramePtr.
REM (16384 * VIC_BANK + 64 * FramePtr = dest_address)
REM ****************************************************************************
FUNCTION SprFrame AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_f(SprNr)
'    RETURN PEEK(spr_ptrs + SprNr)
END FUNCTION

REM ****************************************************************************
REM CALL SprColor(0, COLOR_WHITE)
REM ****************************************************************************
REM Set sprite's color. See lib_color.bas for color codes.
REM ****************************************************************************
SUB SprColor(SprNr AS BYTE, Color AS BYTE) SHARED STATIC OVERLOAD
    spr_c(SprNr) = Color
    'POKE spr_ptrs + SprNr, FramePtr
END SUB

REM ****************************************************************************
REM PRINT "Sprite 0 color "; SprColor(0)
REM ****************************************************************************
REM Returns sprite's color. See lib_color.bas for color codes.
REM ****************************************************************************
FUNCTION SprColor AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_c(SprNr)
'    RETURN PEEK(spr_ptrs + SprNr)
END FUNCTION

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

REM ****************************************************************************
REM CALL SprRecordCollisions(0)
REM FOR t AS BYTE = 1 TO 7
REM     PRINT t, SprCollision(t)
REM NEXT t
REM ****************************************************************************
REM Records sprite-sprite collisions with defined sprite to SprCollision(8) array
REM ****************************************************************************
SUB SprRecordCollisions(SprNr AS BYTE) SHARED STATIC
    'DIM mask AS BYTE
    ASM
MAXSPR = 16                            ;Maximum number of sprites
        ldy {SprNr}
        lda {spr_e},y
        bne spr_collision_detect

        ldx #MAXSPR
spr_collision_disabled_loop:
        dex
        sta {SprCollision},x
        bne spr_collision_disabled_loop

        jmp spr_check_collision_end

spr_collision_detect:
        ldx #MAXSPR-1
spr_collision_loop:
        lda {spr_e},x
        beq spr_check_no_coll

        lda {spr_y},x                       ; Load Enemy Y position
        sec
        sbc {spr_y},y                       ; Subtract Player Y position
        bcs spr_dy_positive
        eor #$ff                            ; Negate result
        adc #1
        cmp {spr_h},x                       ; Compare distance to enemy height
        jmp spr_check_y_branch

spr_dy_positive:
        cmp {spr_h},y                       ; Compare distance to player height
spr_check_y_branch:
        bcs spr_check_no_coll

        lda {spr_x},x                       ; Compare x coordinates
        sec
        sbc {spr_x},y                       ; Subtract Player X position
        bcs spr_dx_positive
        eor #$ff                            ; Negate result
        adc #1
        cmp {spr_w},x                       ; compare distance to enemy width
        jmp spr_check_x_branch

spr_dx_positive:
        cmp {spr_w},y                       ; Compare distance to player width
spr_check_x_branch:
        bcs spr_check_no_coll
        lda #$ff
        dc.b $2c                            ; BIT instruction that skips next LDA
spr_check_no_coll:
        lda #$00
        sta {SprCollision},x
        dex                                 ; Goes to next sprite
        bpl spr_collision_loop
spr_check_collision_end:
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

REM FAST variables for sprite multiplexing
DIM numsprites AS BYTE FAST
DIM sprupdateflag AS BYTE FAST
DIM sortedsprites AS BYTE FAST
DIM tempvariable AS BYTE FAST
DIM sprirqcounter AS BYTE FAST

DIM sortorder(MAX_NUM_SPRITES) AS BYTE FAST

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
    FOR t AS BYTE = 0 TO MAX_NUM_SPRITES-1
        sortorder(t) = t
    NEXT t

    ASM
MAXSPR          = 16                            ;Maximum number of sprites
IRQ1LINE        = $fc                           ;This is the place on screen where the sorting IRQ happens

;Routine to init the raster interrupt system
initraster:
                lda {spr_ptrs}
                sta irq2_sprf+1
                lda {spr_ptrs}+1
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
                jmp the_end

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

irq1_beginsort: inc $d020
                ldx #MAXSPR
                dex
                cpx {sortedsprites}
                bcc irq1_cleardone
                ;lda #$ff                        ;Mark unused sprites with the
irq1_clearloop: ;sta {spr_y},x                    ;lowest Y-coordinate ($ff);
                ;dex                             ;these will "fall" to the
                ;cpx {sortedsprites}             ;bottom of the sorted table
                ;bcs irq1_clearloop
irq1_cleardone: ldx #$00
irq1_sortloop:  ldy {sortorder}+1,x             ;Sorting code. Algorithm
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
irq1_sortreload:ldx #$00
irq1_sortskip:  inx
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
                lda {spr_f},y
                sta sortsprf,x
                lda {spr_c},y
                sta sortsprc,x

                inx
                cpx {sortedsprites}
                bcc irq1_sortloop3
                dec $d020
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
                sta {spr_ptrs},x              ;for color & frame
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
the_end:
                nop
    END ASM
END SUB
