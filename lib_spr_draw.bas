'INCLUDE "lib_memory.bas"
'INCLUDE "lib_types.bas"
'INCLUDE "lib_irq.bas"
'INCLUDE "lib_spr.bas"

DECLARE SUB SprDraw_Init() SHARED STATIC

DECLARE SUB SprDraw_SetAngle(SprNr AS BYTE, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprDraw_SetGeometry(SprNr AS BYTE, GeometryAddr AS WORD) SHARED STATIC
DECLARE SUB SprDraw_SetDirty(SprNr AS BYTE) SHARED STATIC
DECLARE SUB SprDraw_UpdateDirty() SHARED STATIC

DECLARE SUB SprDraw_UpdateSprite(SprNr AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB SprDraw_UpdateSprite(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB SprDraw_DrawGeometry(SprNR AS BYTE, FramePtr AS BYTE) SHARED STATIC

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM _angle(MAX_NUM_SPRITES) AS BYTE
DIM _geometry_addr_hi(MAX_NUM_SPRITES) AS BYTE
DIM _geometry_addr_lo(MAX_NUM_SPRITES) AS BYTE
DIM _spr_draw_dirty(MAX_NUM_SPRITES) AS BYTE SHARED

DIM sprite_line_x1 AS BYTE FAST
DIM sprite_line_y1 AS BYTE FAST
DIM sprite_line_x2 AS BYTE FAST
DIM sprite_line_y2 AS BYTE FAST
DIM sprite_line_dx AS BYTE FAST
DIM sprite_line_dy AS BYTE FAST
DIM sprite_line_err AS BYTE FAST

'DIM RotX(256) AS BYTE @ _RotX SHARED
'DIM RotY(256) AS BYTE @ _RotY SHARED

DIM pixel_mask(24) AS BYTE @_pixel_mask

DIM _bb_x0 AS BYTE
DIM _bb_x1 AS BYTE
DIM _bb_y0 AS BYTE
DIM _bb_y1 AS BYTE

SHARED CONST END_SHAPE  = $10
SHARED CONST NO_DRAW    = $20

REM ****************************************************************************
REM CALL SprDrawInit()
REM ****************************************************************************
REM Call before using the library
REM ****************************************************************************
SUB SprDraw_Init() SHARED STATIC
    ASM
        lda #0
        ldx {spr_num_sprites}
sprdraw_init_loop
        dex
        bmi sprdraw_init_end

        sta {_angle},x
        sta {_spr_draw_dirty},x
        sta {_geometry_addr_hi},x
        sta {_geometry_addr_lo},x

        jmp sprdraw_init_loop
sprdraw_init_end
    END ASM
END SUB

SUB SprDraw_SetAngle(SprNr AS BYTE, Angle AS BYTE) SHARED STATIC
    ASM
        ldx {SprNr}

        lda {Angle}
        and #%11111000
        cmp {_angle},x
        beq sprdraw_setangle_end

        sta {_angle},x
        lda #$ff
        sta {_spr_draw_dirty},x
sprdraw_setangle_end        
    END ASM
END SUB

SUB SprDraw_SetGeometry(SprNr AS BYTE, GeometryAddr AS WORD) SHARED STATIC
    ASM
        ldx {SprNr}

        lda {GeometryAddr}
        cmp {_geometry_addr_lo},x
        bne sprdraw_setgeometry_changed

        lda {GeometryAddr}+1
        cmp {_geometry_addr_hi},x
        beq sprdraw_setgeometry_end

sprdraw_setgeometry_changed
        lda {GeometryAddr}+1
        sta {_geometry_addr_hi},x
        lda {GeometryAddr}
        sta {_geometry_addr_lo},x
        lda #$ff
        sta {_spr_draw_dirty},x
sprdraw_setgeometry_end        
    END ASM
END SUB

SUB SprDraw_SetDirty(SprNr AS BYTE) SHARED STATIC
    ASM
        ldx {SprNr}
        lda #$ff
        sta {_spr_draw_dirty},x
    END ASM
END SUB

SUB SprDraw_SetClean(SprNr AS BYTE) SHARED STATIC
    ASM
        ldx {SprNr}
        lda #$00
        sta {_spr_draw_dirty},x
    END ASM
END SUB

SUB SprDraw_UpdateDirty() SHARED STATIC
    ASM
        ldx {spr_num_sprites}
sprdraw_updatedirty_loop
        dex
        bpl sprdraw_updatedirty_continue
        rts

sprdraw_updatedirty_continue
        lda {_spr_draw_dirty},x
        beq sprdraw_updatedirty_loop
        stx {ZP_B0}
    END ASM
    CALL SprDraw_UpdateSprite(ZP_B0)
END SUB

SUB SprDraw_FlipFrame(SprNr AS BYTE) SHARED STATIC
    ASM
        ldx {SprNr}
        lda {SprFrame},x
        eor #1
        sta {SprFrame},x
    END ASM
END SUB

SUB SprDraw_Clear(SprNr AS BYTE) SHARED STATIC
    ASM
        ldx {SprNr}
        lda {SprFrame},x
        eor #1
        sta {SprFrame},x
    END ASM
    CALL SprClearFrame(SprFrame(SprNr))
END SUB

SUB SprDraw_UpdateSprite(SprNr AS BYTE) SHARED STATIC OVERLOAD
    ASM
        ;swap buffer
        ldx {SprNr}
        lda {SprFrame},x
        eor #1
        sta {SprFrame},x
        sta {ZP_B0}
    END ASM

    CALL SprClearFrame(ZP_B0)
    CALL SprDraw_DrawGeometry(SprNr, ZP_B0)

    ASM
        ldx {SprNr}
        lda #0
        sta {_spr_draw_dirty},x
        
        sec
        lda #12
        sbc {_bb_x0}
        lsr
        sta {Spr_EdgeWest},x

        sec
        lda {_bb_x1}
        sbc #12
        lsr
        sta {Spr_EdgeEast},x

        sec
        lda #10
        sbc {_bb_y0}
        sta {Spr_EdgeNorth},x

        sec
        lda {_bb_y1}
        sbc #10
        sta {Spr_EdgeSouth},x
    END ASM
END SUB

SUB SprDraw_UpdateSprite(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC OVERLOAD
    ASM
        ldx {SprNr}
        lda {Angle}
        and #%11111000
        sta {_angle},x
        lda {GeometryAddr}
        sta {_geometry_addr_lo},x
        lda {GeometryAddr}+1
        sta {_geometry_addr_hi},x
    END ASM

    CALL SprDraw_UpdateSprite(SprNr)
END SUB

REM Geometry specifies a polyline with a sequence of angular coordinates.
REM Points in polyline are connected by drawing a line between then.
REM 
REM Internal polyline format packs each point to a single byte.
REM Angle is coded into 5 highest bits and Radial is coded into the
REM lowest 3 bits:
REM
REM  76543210
REM %AAAAARRR
REM 
REM Radial (point's distance from center) 0-7: 
REM Value  Distance
REM    0      0 px
REM    1      2 px
REM    2      4 px
REM    3      6 px
REM    4      7 px
REM    5      8 px
REM    6      9 px
REM    7     10 px
REM
REM Angular (point's direction) 0-31 about 11.6 deg/step:
REM Value  Direction
REM    0     EAST
REM    2     ENE
REM    4     NE
REM    6     NNE
REM    8     NORT
REM   10     NNW
REM   12     NW
REM   14     WNW
REM   16     WEST
REM   18     WSW
REM   20     SW
REM   22     SSW
REM   24     SOUTH
REM   26     SSE
REM   28     SE
REM   30     ESE
REM
REM SPECIAL VALUES
REM 
REM NO DRAW  $20  Don't draw line between previous and next points
REM END      $10  End of shape data
REM 
REM Special Values occupy unused Angles in Radial 0 circle and thus center point
REM must always be addressed with 0, 0 - even thou in theory all angles with
REM Radial 0 represent same point (center). 
SUB SprDraw_DrawGeometry(SprNr AS BYTE, FramePtr AS BYTE) SHARED STATIC
    DIM Draw AS BYTE

    _bb_x0 = 23
    _bb_x1 = 0
    _bb_y0 = 20
    _bb_y1 = 0

    ASM
        ;ZP_W0 = spr_vic_bank_addr + SHL(CWORD(FramePtr), 6)
        sta $408
        lda #0
        sta {ZP_W0} ; $19
        sta {Draw}
        tay

        lda {FramePtr}
        lsr
        ror {ZP_W0}
        lsr
        ror {ZP_W0}

        clc
        adc {spr_vic_bank_addr}+1
        sta {ZP_W0}+1

        ldx {SprNr}
        lda {_geometry_addr_lo},x
        sta {ZP_W1}
        lda {_geometry_addr_hi},x
        sta {ZP_W1}+1

sprdraw_drawgeometry_loop
            ;Index = PEEK(GeometryAddr)
            lda ({ZP_W1}),y ;4de1
            ;GeometryAddr = GeometryAddr + 1
            iny
            ;IF Index = END_SHAPE THEN EXIT DO
            cmp #$10
            bne sprdraw_continue
            jmp sprdraw_end
            ;rts
sprdraw_continue
            ;IF Index = NO_DRAW THEN
            cmp #$20
            bne sprdraw_noskip

            ;Draw = $00
            lda #0
            sta {Draw}
            ;Index = PEEK(GeometryAddr)
            lda ({ZP_W1}),y
            ;GeometryAddr = GeometryAddr + 1
            iny

sprdraw_noskip
            sty {FramePtr}

            ;Index = Index + Angle
            ldx {SprNr}
            clc
            adc {_angle},x
            tax

            ;sprite_line_x1 = sprite_line_x2
            ;sprite_line_y1 = sprite_line_y2
            lda {sprite_line_x2}
            sta {sprite_line_x1}
            lda {sprite_line_y2}
            sta {sprite_line_y1}

            ;sprite_line_x2 = RotX(Index)
            ;sprite_line_y2 = RotY(Index)
            lda {RotX},x
            sta {sprite_line_x2}
            lda {RotY},x
            sta {sprite_line_y2}

bb_left:
            lda {sprite_line_x2}
            cmp {_bb_x0}
            bcs bb_right                ;if x2 >= left then bb_right
            sta {_bb_x0}
bb_right:
            cmp {_bb_x1}
            bcc bb_top                  ;if x2 < right then bb_top
            sta {_bb_x1}

bb_top:
            lda {sprite_line_y2}
            cmp {_bb_y0}
            bcs bb_bottom               ;if y2 >= top then bb_bottom
            sta {_bb_y0}
bb_bottom:
            cmp {_bb_y1}
            bcc bb_end                  ;if y2 < bottom then bb_end
            sta {_bb_y1}
bb_end:
            lda {Draw}
            beq sprdraw_skip_drawline
            jsr shape_draw_line
sprdraw_skip_drawline
            lda #$ff
            sta {Draw}
            ldy {FramePtr}
            jmp sprdraw_drawgeometry_loop

shape_draw_line:
        ldx #$c6                ; calc dy, sy
        lda {sprite_line_y1}
        sec
        sbc {sprite_line_y2}
        bpl sprite_line_dy_negative
        ldx #$e6
        eor #$ff                ; neg
        clc
        adc #1
sprite_line_dy_negative
        sta {sprite_line_dy}
        stx sprite_line_commit_sy

        ldx #$c6                ; calc dx, sx
        lda {sprite_line_x1}
        sec
        sbc {sprite_line_x2}
        bpl sprite_line_dx_negative
        ldx #$e6
        eor #$ff                ; neg
        clc
        adc #1
sprite_line_dx_negative
        sta {sprite_line_dx}
        stx sprite_line_commit_sx

        cmp {sprite_line_dy}
        beq sprite_line_err_dy
        bpl sprite_line_err_dx
sprite_line_err_dy
        lda {sprite_line_dy}
        eor #$ff                ; neg
        clc
        adc #1
sprite_line_err_dx
        sta {sprite_line_err}

        asl {sprite_line_dx}
        asl {sprite_line_dy}

sprite_line_loop
        ; plot
        lda {sprite_line_x1}	    ; addr offset to y
        lsr
        lsr
        lsr
        clc
        adc {sprite_line_y1}
        adc {sprite_line_y1}
        adc {sprite_line_y1}
        tay
        
	    lda {sprite_line_x1}      ; bitmask offset to x
        and #%00000111
        tax
        
        lda ({ZP_W0}),y
        ora {pixel_mask},x
        sta ({ZP_W0}),y
        
        ; x1 != x2 ?
        lda {sprite_line_x1}
        cmp {sprite_line_x2}
        bne sprite_line_step

        ; y1 != y2 ?
        lda {sprite_line_y1}
        cmp {sprite_line_y2}
        bne sprite_line_step

        rts

sprite_line_step
        lda {sprite_line_err}
        pha

        clc
        adc {sprite_line_dx}
        bmi sprite_line_no_dx
        beq sprite_line_no_dx

        lda {sprite_line_err}
        sec
        sbc {sprite_line_dy}
        sta {sprite_line_err}
        
sprite_line_commit_sx
        inc {sprite_line_x1}

sprite_line_no_dx
        pla
        cmp {sprite_line_dy}
        bpl sprite_line_no_dy
        
        lda {sprite_line_err}
        clc
        adc {sprite_line_dx}
        sta {sprite_line_err}

sprite_line_commit_sy
        inc {sprite_line_y1}

sprite_line_no_dy
        jmp sprite_line_loop

sprdraw_end
    END ASM
END SUB

GOTO THE_END

'_RotX:
'INCLUDE "ext/x_rotation_table.bas"
'_RotY:
'INCLUDE "ext/y_rotation_table.bas"

_pixel_mask:
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01

THE_END:
