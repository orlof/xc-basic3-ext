REM **************************************
REM LIBRARY CONVENTIONS
REM   TRUE  = $ff
REM   FALSE = $00
REM **************************************

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
REM Records sprite-sprite collisions with defined sprite to SprCollision(8) array
REM ****************************************************************************
DECLARE SUB SprRecordCollisions(SprNr AS BYTE) SHARED STATIC

REM **************************************
REM PUBLIC FIELDS
REM **************************************
REM ****************************************************************************
REM This field is an alias for io registers that store sprite colors. R/W
REM ****************************************************************************
DIM SHARED SprColor(8) AS BYTE @$d027

REM ****************************************************************************
REM "CALL SprRecordCollisions(SprNr)" updates collison data (TRUE/FALSE) for each sprite
REM ****************************************************************************
DIM SHARED SprCollision(8) AS BYTE @_SprCollision


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
DIM VicBank AS WORD
    VicBank = 0
DIM spr_ptrs AS WORD
    spr_ptrs = 2040

DIM spr_reg_xy(8) AS TYPE_SPR_REG @$d000
DIM spr_reg_msb AS BYTE @$d010
DIM spr_reg_mc AS BYTE @$d01c
DIM spr_reg_mc1 AS BYTE @$d025
DIM spr_reg_mc2 AS BYTE @$d026
DIM spr_reg_dx AS BYTE @$d01d
DIM spr_reg_dy AS BYTE @$d017
DIM spr_reg_back AS BYTE @$d01b
DIM spr_reg_enable AS BYTE @$d015

REM **************************************
REM INTERNAL COLLISION DETECTION
REM **************************************
DIM spr_x(8) AS BYTE @_spr_x
DIM spr_y(8) AS BYTE @_spr_y
DIM spr_w(8) AS BYTE @_spr_w
DIM spr_h(8) AS BYTE @_spr_h

REM **************************************
REM INTERNAL HELPER
REM **************************************
DIM bit_pos(8) AS BYTE @_bit_pos
DIM ZP_W0 AS WORD FAST
DIM ZP_W1 AS WORD FAST

REM ****************************************************************************
REM CALL SprInit()
REM ****************************************************************************
REM Call before using the library if you change VIC bank or screen memory 
REM address
REM ****************************************************************************
SUB SprInit() SHARED STATIC
    VicBank = 16384 * ((PEEK($dd00) AND %00000011) XOR %00000011)
    spr_ptrs = VicBank + SHL(CWORD(PEEK($d018) AND %11110000), 6) + 1016
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
    MEMCPY SrcAddr, VicBank + SHL(CWORD(FramePtr), 6), 63
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
    MEMSET VicBank + SHL(CWORD(FramePtr), 6), 63, 0
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
    ZP_W0 = VicBank + SHL(CWORD(SrcFramePtr), 6)
    ZP_W1 = VicBank + SHL(CWORD(DstFramePtr), 6)

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
    ZP_W0 = VicBank + SHL(CWORD(SrcFramePtr), 6)
    ZP_W1 = VicBank + SHL(CWORD(DstFramePtr), 6) + 60
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
    DIM bit AS BYTE
        bit = bit_pos(SprNr)
    IF IsMultiColor THEN
        spr_reg_mc = spr_reg_mc OR bit
    ELSE
        spr_reg_mc = spr_reg_mc AND NOT bit
    END IF
    IF DoubleX THEN
        spr_reg_dx = spr_reg_dx OR bit
        spr_w(SprNr) = 24
    ELSE
        spr_reg_dx = spr_reg_dx AND NOT bit
        spr_w(SprNr) = 12
    END IF
    IF DoubleY THEN
        spr_reg_dy = spr_reg_dy OR bit
        spr_h(SprNr) = 42
    ELSE
        spr_reg_dy = spr_reg_dy AND NOT bit
        spr_h(SprNr) = 21
    END IF
    IF Background THEN
        spr_reg_back = spr_reg_back OR bit
    ELSE
        spr_reg_back = spr_reg_back AND NOT bit
    END IF
    SprColor(SprNr) = Color
END SUB

REM ****************************************************************************
REM INCLUDE "lib_types.bas"
REM CALL SprEnable(0, TRUE)
REM ****************************************************************************
SUB SprEnable(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
    IF Value THEN
        spr_reg_enable = spr_reg_enable OR bit_pos(SprNr)
    ELSE
        spr_reg_enable = spr_reg_enable AND NOT bit_pos(SprNr)
    END IF
END SUB

REM ****************************************************************************
REM IF SprEnable(0) THEN PRINT "0: enabled"
REM ****************************************************************************
FUNCTION SprEnable AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_reg_enable AND bit_pos(SprNr) <> 0
END FUNCTION

REM ****************************************************************************
REM INCLUDE "lib_types.bas"
REM CALL SprDoubleX(0, TRUE)
REM ****************************************************************************
SUB SprDoubleX(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
    IF Value THEN
        spr_reg_dx = spr_reg_dx OR bit_pos(SprNr)
    ELSE
        spr_reg_dx = spr_reg_dx AND NOT bit_pos(SprNr)
    END IF
END SUB

REM ****************************************************************************
REM IF SprDoubleX(0) THEN PRINT "width: 48"
REM ****************************************************************************
FUNCTION SprDoubleX AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_reg_dx AND bit_pos(SprNr) <> 0
END FUNCTION

REM ****************************************************************************
REM INCLUDE "lib_types.bas"
REM CALL SprDoubleY(0, TRUE)
REM ****************************************************************************
SUB SprDoubleY(SprNr AS BYTE, Value AS BYTE) SHARED STATIC OVERLOAD
    IF Value THEN
        spr_reg_dy = spr_reg_dy OR bit_pos(SprNr)
    ELSE
        spr_reg_dy = spr_reg_dy AND NOT bit_pos(SprNr)
    END IF
END SUB

REM ****************************************************************************
REM IF SprDoubleY(0) THEN PRINT "height: 42"
REM ****************************************************************************
FUNCTION SprDoubleY AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_reg_dy AND bit_pos(SprNr) <> 0
END FUNCTION

REM ****************************************************************************
REM CALL spr_shape(0, 192)
REM ****************************************************************************
REM Set sprite's FramePtr (16384 * VIC_BANK + 64 * FramePtr = dest_address)
REM Make sure to place the sprite pattern data to this memory area either
REM with "ORIGIN" -directive or by "CALL SprImportShape(SrcAddr)""
REM ****************************************************************************
SUB SprFrame(SprNr AS BYTE, FramePtr AS BYTE) SHARED STATIC OVERLOAD
    POKE spr_ptrs + SprNr, FramePtr
END SUB

REM ****************************************************************************
REM PRINT "Sprite 0 address "; 64 * SprFrame(0)
REM ****************************************************************************
REM Returns sprite's FramePtr.
REM (16384 * VIC_BANK + 64 * FramePtr = dest_address)
REM ****************************************************************************
FUNCTION SprFrame AS BYTE(SprNr AS BYTE) SHARED STATIC OVERLOAD
    RETURN PEEK(spr_ptrs + SprNr)
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
        lda {SprNr}
        tax                     ; [x] = SprNr
        asl
        tay                     ; [y] = 2 * SprNr

        clc                     ; spr_reg_xy(SprNr).y = y + 50
        lda {y}
        adc #50
        sta $d001,y
        sta {spr_y},x           ; spr_y(SprNr) = y + 50

        clc                     ; x = (x + 24) && $1ff
        lda {x}
        adc #24
        sta {x}
        lda {x}+1
        adc #0
        and #1
        sta {x}+1

        lsr                     ; spr_x(SprNr) = x >> 1
        lda {x}
        ror
        sta {spr_x},x

        lda {x}+1               ; IF x >= 480
        beq spr_xy_no_bad_zone
        lda {x}
        cmp #224
        bcc spr_xy_no_bad_zone

        sec                     ; THEN x -= 8
        sbc #8
        sta {x}                 ; END IF

spr_xy_no_bad_zone:
        lda {x}                 ; spr_reg_xy(SprNr).x = x && $ff
        sta $d000,y
        lda {x}+1               ; IF x > 255 THEN spr_xy_msb1
        bne spr_xy_msb1
        
spr_xy_msb0:
        lda {bit_pos},x         ; spr_reg_msb = spr_reg_msb OR bit_pos(SprNr)
        eor #$ff
        and $d010
        sta $d010
        jmp spr_xy_end

spr_xy_msb1:
        lda {bit_pos},x         ; spr_reg_msb AND NOT bit_pos(SprNr)
        ora $d010
        sta $d010
spr_xy_end:
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
    DIM mask AS BYTE
    ASM
        lda $d015
        sta {mask}
        ldy {SprNr}
        and {bit_pos},y
        bne spr_collision_detect

        sta {SprCollision}                       ; no collisions for SprNr
        sta {SprCollision}+1
        sta {SprCollision}+2
        sta {SprCollision}+3
        sta {SprCollision}+4
        sta {SprCollision}+5
        sta {SprCollision}+6
        sta {SprCollision}+7
        jmp spr_check_collision_end

spr_collision_detect:
        lda {bit_pos},Y                     ; no collision with itself
        eor #$ff
        and {mask}
        sta {mask}
        
        ldx #7
spr_collision_loop:
        asl {mask}
        bcc spr_check_no_coll

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
        bpl spr_dx_positive
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
    END ASM
END SUB

REM **************************************
REM Sprites data tables
REM **************************************
_spr_x:
DATA AS BYTE $A0, $80, $A0, $D0, $50, $70, $90, $A0
_spr_y:
DATA AS BYTE $64, $80, $A0, $D0, $50, $70, $90, $A0
_SprCollision:
DATA AS BYTE $00, $00, $00, $00, $00, $00, $00, $00
_spr_w:
DATA AS BYTE $00, $00, $00, $00, $00, $00, $00, $00
_spr_h:
DATA AS BYTE $00, $00, $00, $00, $00, $00, $00, $00

_bit_pos:
DATA AS BYTE 1, 2, 4, 8, 16, 32, 64, 128

