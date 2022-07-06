REM **************************************
REM LIBRARY CONVENTIONS
REM   TRUE  = $ff
REM   FALSE = $00
REM **************************************

REM ****************************************************************************
REM Must be called before other methods if VIC bank or videoram is moved
REM ****************************************************************************
DECLARE SUB spr_init() SHARED STATIC

REM ****************************************************************************
REM Define colors for multicolor sprites
REM ****************************************************************************
DECLARE SUB spr_mcolors(mc1 AS BYTE, mc2 AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Copies sprite pattern from DATA AS BYTE statements to the end of the current 
REM VIC bank. Returns pattern_ptr for "CALL spr_pattern(spr_nr, pattern_ptr)"
REM ****************************************************************************
DECLARE FUNCTION spr_import_pattern AS BYTE(src_addr AS WORD) SHARED STATIC

REM ****************************************************************************
REM Copies sprite pattern from DATA AS BYTE statements to address specified by 
REM given pattern_ptr (16384 * VIC_BANK + 64 * pattern_ptr = dest_address)
REM ****************************************************************************
DECLARE SUB spr_import_pattern_to(src_addr AS WORD, pattern_ptr AS WORD) SHARED STATIC

REM ****************************************************************************
REM Following methods set and get sprite properties
REM ****************************************************************************
DECLARE SUB spr_config(spr_nr AS BYTE, is_mcolor AS BYTE, double_x AS BYTE, double_y AS BYTE, background AS BYTE, color AS BYTE) SHARED STATIC
DECLARE SUB spr_enable(spr_nr AS BYTE, enable AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION spr_enable AS BYTE(spr_nr AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB spr_double_x(spr_nr AS BYTE, enable AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION spr_double_x AS BYTE(spr_nr AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB spr_double_y(spr_nr AS BYTE, enable AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION spr_double_y AS BYTE(spr_nr AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB spr_pattern(spr_nr AS BYTE, pattern_ptr AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION spr_pattern AS BYTE(spr_nr AS BYTE) SHARED STATIC OVERLOAD

REM ****************************************************************************
REM Set x, y position of sprite.
REM ****************************************************************************
DECLARE SUB spr_xy(spr_nr AS BYTE, x AS WORD, y AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Records sprite-sprite collisions with defined sprite to spr_col(8) array
REM ****************************************************************************
DECLARE SUB spr_detect(spr_nr AS BYTE) SHARED STATIC


REM **************************************
REM PUBLIC FIELDS
REM **************************************
REM ****************************************************************************
REM This field is an alias for io registers that store sprite colors. R/W
REM ****************************************************************************
DIM SHARED spr_color(8) AS BYTE @$d027

REM ****************************************************************************
REM "CALL spr_detect(spr_nr)" updates collison data (TRUE/FALSE) for each sprite
REM ****************************************************************************
DIM SHARED spr_col(8) AS BYTE @_spr_col


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
DIM spr_bank AS WORD
    spr_bank = 0
DIM spr_ptrs AS WORD
    spr_ptrs = 2040
DIM spr_next_pattern_ptr AS BYTE
    spr_next_pattern_ptr = 0

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

REM ****************************************************************************
REM CALL spr_init()
REM ****************************************************************************
REM Call before using the library if you change VIC bank or screen memory 
REM address
REM ****************************************************************************
SUB spr_init() SHARED STATIC
    spr_bank = 16384 * ((PEEK($dd00) AND %11111100) XOR %00000011)
    spr_ptrs = spr_bank + SHL(CWORD(PEEK($d018) AND %11110000), 6) + 1016
END SUB

REM ****************************************************************************
REM INCLUDE "color.bas"
REM CALL spr_mcolors(COLOR_BLACK, COLOR_WHITE)
REM ****************************************************************************
REM Set color values for multicolor sprites
REM ****************************************************************************
SUB spr_mcolors(mc1 AS BYTE, mc2 AS BYTE) SHARED STATIC
    spr_reg_mc1 = mc1
    spr_reg_mc2 = mc2
END SUB

REM ****************************************************************************
REM DIM pattern_ptr AS BYTE
REM     pattern_ptr = spr_import_pattern(@PLAYER_SHIP)
REM CALL spr_pattern(0, pattern_ptr)
REM ...
REM PLAYER_SHIP:
REM DATA AS BYTE 1,2,3,4,5...
REM ****************************************************************************
REM Copies sprite pattern from DATA AS BYTE statements to the end of the current 
REM VIC bank. 
REM Return: pattern_ptr (16384 * VIC_BANK + 64 * pattern_ptr = dest_address)
REM [Developer is responsible that the area is free]
REM ****************************************************************************
FUNCTION spr_import_pattern AS BYTE(src_addr AS WORD) SHARED STATIC OVERLOAD
    spr_next_pattern_ptr = spr_next_pattern_ptr - 1
    CALL spr_import_pattern_to(src_addr, spr_next_pattern_ptr)
    RETURN spr_next_pattern_ptr
END FUNCTION

REM ****************************************************************************
REM CALL spr_import_pattern_to(@PLAYER_SHIP, 255)
REM CALL spr_pattern(0, 255)
REM ...
REM PLAYER_SHIP:
REM DATA AS BYTE 1,2,3,4,5...
REM ****************************************************************************
REM Copies sprite pattern from DATA AS BYTE statements to address specified by 
REM given pattern_ptr (16384 * VIC_BANK + 64 * pattern_ptr = dest_address)
REM [Developer is responsible that the area is free]
REM ****************************************************************************
SUB spr_import_pattern_to(src_addr AS WORD, pattern_ptr AS BYTE) SHARED STATIC OVERLOAD
    ASM
        sei                     ; turn off interrupts  
        dec 1                   ; can use also io memory for sprites
        dec 1                   ; by disabling kernel and io
    END ASM
    MEMCPY src_addr, spr_bank + SHL(pattern_ptr, 6), 63
    ASM
        inc 1                   ; restore io, kernel and interrupts
        inc 1
        cli
    END ASM
END SUB

REM ****************************************************************************
REM INCLUDE "lib_color.bas"
REM INCLUDE "lib_types.bas"
REM CALL spr_config(0, FALSE, FALSE, FALSE, FALSE, COLOR_WHITE)
REM ****************************************************************************
REM Configure all multiple sprite attributes with one call
REM ****************************************************************************
SUB spr_config(spr_nr AS BYTE, is_mcolor AS BYTE, double_x AS BYTE, double_y AS BYTE, background AS BYTE, color AS BYTE) SHARED STATIC
    DIM bit AS BYTE
        bit = bit_pos(spr_nr)
    IF is_mcolor THEN
        spr_reg_mc = spr_reg_mc OR bit
    ELSE
        spr_reg_mc = spr_reg_mc AND NOT bit
    END IF
    IF double_x THEN
        spr_reg_dx = spr_reg_dx OR bit
        spr_w(spr_nr) = 24
    ELSE
        spr_reg_dx = spr_reg_dx AND NOT bit
        spr_w(spr_nr) = 12
    END IF
    IF double_y THEN
        spr_reg_dy = spr_reg_dy OR bit
        spr_h(spr_nr) = 42
    ELSE
        spr_reg_dy = spr_reg_dy AND NOT bit
        spr_h(spr_nr) = 21
    END IF
    IF background THEN
        spr_reg_back = spr_reg_back OR bit
    ELSE
        spr_reg_back = spr_reg_back AND NOT bit
    END IF
    spr_color(spr_nr) = color
END SUB

REM ****************************************************************************
REM INCLUDE "lib_types.bas"
REM CALL spr_enable(0, TRUE)
REM ****************************************************************************
SUB spr_enable(spr_nr AS BYTE, enable AS BYTE) SHARED STATIC OVERLOAD
    IF enable THEN
        spr_reg_enable = spr_reg_enable OR bit_pos(spr_nr)
    ELSE
        spr_reg_enable = spr_reg_enable AND NOT bit_pos(spr_nr)
    END IF
END SUB

REM ****************************************************************************
REM IF spr_enable(0) THEN PRINT "0: enabled"
REM ****************************************************************************
FUNCTION spr_enable AS BYTE(spr_nr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_reg_enable AND bit_pos(spr_nr) <> 0
END FUNCTION

REM ****************************************************************************
REM INCLUDE "lib_types.bas"
REM CALL spr_double_x(0, TRUE)
REM ****************************************************************************
SUB spr_double_x(spr_nr AS BYTE, enable AS BYTE) SHARED STATIC OVERLOAD
    IF enable THEN
        spr_reg_dx = spr_reg_dx OR bit_pos(spr_nr)
    ELSE
        spr_reg_dx = spr_reg_dx AND NOT bit_pos(spr_nr)
    END IF
END SUB

REM ****************************************************************************
REM IF spr_double_x(0) THEN PRINT "width: 48"
REM ****************************************************************************
FUNCTION spr_double_x AS BYTE(spr_nr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_reg_dx AND bit_pos(spr_nr) <> 0
END FUNCTION

REM ****************************************************************************
REM INCLUDE "lib_types.bas"
REM CALL spr_double_y(0, TRUE)
REM ****************************************************************************
SUB spr_double_y(spr_nr AS BYTE, enable AS BYTE) SHARED STATIC OVERLOAD
    IF enable THEN
        spr_reg_dy = spr_reg_dy OR bit_pos(spr_nr)
    ELSE
        spr_reg_dy = spr_reg_dy AND NOT bit_pos(spr_nr)
    END IF
END SUB

REM ****************************************************************************
REM IF spr_double_y(0) THEN PRINT "height: 42"
REM ****************************************************************************
FUNCTION spr_double_y AS BYTE(spr_nr AS BYTE) SHARED STATIC OVERLOAD
    RETURN spr_reg_dy AND bit_pos(spr_nr) <> 0
END FUNCTION

REM ****************************************************************************
REM CALL spr_shape(0, 192)
REM ****************************************************************************
REM Set sprite's pattern_ptr (16384 * VIC_BANK + 64 * pattern_ptr = dest_address)
REM Make sure to place the sprite pattern data to this memory area either
REM with "ORIGIN" -directive or by "CALL spr_import_pattern(src_addr)""
REM ****************************************************************************
SUB spr_pattern(spr_nr AS BYTE, pattern_ptr AS BYTE) SHARED STATIC OVERLOAD
    POKE spr_ptrs + spr_nr, pattern_ptr
END SUB

REM ****************************************************************************
REM PRINT "Sprite 0 address "; 64 * spr_pattern(0)
REM ****************************************************************************
REM Returns sprite's pattern_ptr.
REM (16384 * VIC_BANK + 64 * pattern_ptr = dest_address)
REM ****************************************************************************
FUNCTION spr_pattern AS BYTE(spr_nr AS BYTE) SHARED STATIC OVERLOAD
    RETURN PEEK(spr_ptrs + spr_nr)
END FUNCTION

REM ****************************************************************************
REM CALL spr_xy(0, 0, 0)
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
SUB spr_xy(spr_nr AS BYTE, x AS WORD, y AS BYTE) SHARED STATIC
    ASM
        lda {spr_nr}
        tax                     ; [x] = spr_nr
        asl
        tay                     ; [y] = 2 * spr_nr

        clc                     ; spr_reg_xy(spr_nr).y = y + 50
        lda {y}
        adc #50
        sta $d001,y
        sta {spr_y},x           ; spr_y(spr_nr) = y + 50

        clc                     ; x = (x + 24) && $1ff
        lda {x}
        adc #24
        sta {x}
        lda {x}+1
        adc #0
        and #1
        sta {x}+1

        lsr                     ; spr_x(spr_nr) = x >> 1
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
        lda {x}                 ; spr_reg_xy(spr_nr).x = x && $ff
        sta $d000,y
        lda {x}+1               ; IF x > 255 THEN spr_xy_msb1
        bne spr_xy_msb1
        
spr_xy_msb0:
        lda {bit_pos},x         ; spr_reg_msb = spr_reg_msb OR bit_pos(spr_nr)
        eor #$ff
        and $d010
        sta $d010
        jmp spr_xy_end

spr_xy_msb1:
        lda {bit_pos},x         ; spr_reg_msb AND NOT bit_pos(spr_nr)
        ora $d010
        sta $d010
spr_xy_end:
    END ASM
END SUB

REM ****************************************************************************
REM CALL spr_detect(0)
REM FOR t AS BYTE = 1 TO 7
REM     PRINT t, spr_col(t)
REM NEXT t
REM ****************************************************************************
REM Records sprite-sprite collisions with defined sprite to spr_col(8) array
REM ****************************************************************************
SUB spr_detect(spr_nr AS BYTE) SHARED STATIC
    DIM mask AS BYTE
    ASM
        lda $d015
        sta {mask}
        ldy {spr_nr}
        and {bit_pos},y
        bne spr_collision_detect

        sta {spr_col}                       ; no collisions for spr_nr
        sta {spr_col}+1
        sta {spr_col}+2
        sta {spr_col}+3
        sta {spr_col}+4
        sta {spr_col}+5
        sta {spr_col}+6
        sta {spr_col}+7
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
        bpl spr_check_y_no_minus
        eor #$ff                            ; Invert result sign
        adc #1
        cmp {spr_h},x
        jmp spr_check_y_branch

spr_check_y_no_minus:
        cmp {spr_h},y                       ; Check for enemy sprite distance Y
spr_check_y_branch:
        bcs spr_check_no_coll

        lda {spr_x},x                       ; Compare x coordinates
        sec
        sbc {spr_x},y                       ; Subtract Player X position
        bpl spr_check_no_minus
        eor #$ff                            ; Invert result sign
        cmp {spr_w},x
        jmp spr_check_x_branch
spr_check_no_minus:
        cmp {spr_w},y                       ; Check for enemy sprite distance X
spr_check_x_branch:
        bcs spr_check_no_coll
        lda #$ff
        dc.b $2c ;bit
spr_check_no_coll:
        lda #$00
        sta {spr_col},x
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
_spr_col:
DATA AS BYTE $00, $00, $00, $00, $00, $00, $00, $00
_spr_w:
DATA AS BYTE $00, $00, $00, $00, $00, $00, $00, $00
_spr_h:
DATA AS BYTE $00, $00, $00, $00, $00, $00, $00, $00

_bit_pos:
DATA AS BYTE 1, 2, 4, 8, 16, 32, 64, 128
