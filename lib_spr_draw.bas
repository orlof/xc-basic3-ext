'INCLUDE "lib_memory.bas"
'INCLUDE "lib_types.bas"
'INCLUDE "lib_irq.bas"
'INCLUDE "lib_spr.bas"

DECLARE SUB SprDraw_Init() SHARED STATIC
DECLARE SUB SprDraw_PrepareGeometry(GeometryAddr AS WORD) SHARED STATIC

DECLARE SUB SprDraw_SetAngle(SprNr AS BYTE, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprDraw_SetGeometry(SprNr AS BYTE, GeometryAddr AS WORD) SHARED STATIC
DECLARE SUB SprDraw_SetDirty(SprNr AS BYTE) SHARED STATIC
DECLARE SUB SprDraw_UpdateDirty(MaxNumUpdates AS BYTE) SHARED STATIC

DECLARE SUB SprDraw_UpdateSprite(SprNr AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB SprDraw_UpdateSprite(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC OVERLOAD
DECLARE SUB SprDraw_DrawGeometry(FramePtr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprDraw_DrawLine(FramePtr AS BYTE, x0 AS BYTE, y0 AS BYTE, x1 AS BYTE, y1 AS BYTE) SHARED STATIC
DECLARE SUB _DrawLine() STATIC

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM _angle(MAX_NUM_SPRITES) AS BYTE
DIM _geometry_addr(MAX_NUM_SPRITES) AS WORD
DIM _dirty(MAX_NUM_SPRITES) AS BYTE

DIM sprite_line_x1 AS BYTE FAST
DIM sprite_line_y1 AS BYTE FAST
DIM sprite_line_x2 AS BYTE FAST
DIM sprite_line_y2 AS BYTE FAST
DIM sprite_line_dx AS BYTE FAST
DIM sprite_line_dy AS BYTE FAST
DIM sprite_line_err AS BYTE FAST

DIM pixel_mask(24) AS BYTE @_pixel_mask

DIM _bounding_box_left AS BYTE
DIM _bounding_box_right AS BYTE
DIM _bounding_box_top AS BYTE
DIM _bounding_box_bottom AS BYTE

SHARED CONST END_SHAPE  = $10
SHARED CONST NO_DRAW    = $20

REM ****************************************************************************
REM CALL SprDrawInit()
REM ****************************************************************************
REM Call before using the library
REM ****************************************************************************
SUB SprDraw_Init() SHARED STATIC
    FOR ZP_B0 = 0 TO spr_num_sprites - 1
        _angle(ZP_B0) = 0
        _geometry_addr(ZP_B0) = 0
        _dirty(ZP_B0) = FALSE
    NEXT
END SUB

REM Convert human friendly data to packet polyline format.
REM Data has a byte pair for each Point. First byte represents the Angle
REM (0-31) and second byte represents Radial (0-7). 
REM
REM Packet data overwrites the human friendly data and requires 1/2 memory.
REM This assures that you can use the same label pointers for ShapePrepare
REM and ShapeDrawGeometry
REM 
REM Special Values
REM   NO_DRAW  DATA AS WORD $0400
REM   END      DATA AS WORD $0200
REM   0,0      DATA AS BYTE 0, 0
SUB SprDraw_PrepareGeometry(GeometryAddr AS WORD) SHARED STATIC
    ZP_W0 = 0   ' offset
    DO
        DIM Angular AS BYTE
        Angular = PEEK(GeometryAddr + ZP_W0)

        DIM Radial AS BYTE
        Radial = PEEK(GeometryAddr + ZP_W0 + 1)

        POKE GeometryAddr + SHR(ZP_W0, 1), SHL(Angular, 3) OR Radial
        ZP_W0 = ZP_W0 + 2
    LOOP UNTIL Angular = 2 AND Radial = 0
END SUB

SUB SprDraw_SetAngle(SprNr AS BYTE, Angle AS BYTE) SHARED STATIC
    ZP_B0 = Angle AND %11111000
    IF ZP_B0 <> _angle(SprNr) THEN
        _angle(SprNr) = ZP_B0
        _dirty(SprNr) = TRUE
    END IF
END SUB

SUB SprDraw_SetGeometry(SprNr AS BYTE, GeometryAddr AS WORD) SHARED STATIC
    IF GeometryAddr <> _geometry_addr(SprNr) THEN
        _geometry_addr(SprNr) = GeometryAddr
        _dirty(SprNr) = TRUE
    END IF
END SUB

SUB SprDraw_SetDirty(SprNr AS BYTE) SHARED STATIC
    _dirty(SprNr) = TRUE
END SUB

DIM _next_spr_nr AS BYTE
    _next_spr_nr = 0
SUB SprDraw_UpdateDirty(MaxNumUpdates AS BYTE) SHARED STATIC
    DIM _start_spr_nr AS BYTE
        _start_spr_nr = _next_spr_nr
    DO
        IF _dirty(_next_spr_nr) THEN
            CALL SprDraw_UpdateSprite(_next_spr_nr) 
            MaxNumUpdates = MaxNumUpdates - 1
        END IF

        _next_spr_nr = _next_spr_nr + 1
        IF _next_spr_nr = spr_num_sprites THEN _next_spr_nr = 0 
    LOOP UNTIL _next_spr_nr = _start_spr_nr OR MaxNumUpdates = 0
    _next_spr_nr = 0
END SUB

SUB SprDraw_Clear(SprNr AS BYTE) SHARED STATIC
    SprFrame(SprNr) = SprFrame(SprNr) XOR 1
    CALL SprClearFrame(SprFrame(SprNr))
END SUB

SUB SprDraw_UpdateSprite(SprNr AS BYTE) SHARED STATIC OVERLOAD
    ZP_B0 = SprFrame(SprNr) XOR 1
    SprFrame(SprNr) = ZP_B0

    CALL SprClearFrame(ZP_B0)
    CALL SprDraw_DrawGeometry(ZP_B0, _geometry_addr(SprNr), _angle(SprNr))
    _dirty(SprNr) = FALSE

    SprBoundingBoxLeft(SprNr) = SHR(_bounding_box_left, 1)
    SprBoundingBoxRight(SprNr) = SHR(_bounding_box_right, 1)
    SprBoundingBoxTop(SprNr) = _bounding_box_top
    SprBoundingBoxBottom(SprNr) = _bounding_box_bottom
END SUB

SUB SprDraw_UpdateSprite(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC OVERLOAD
    _geometry_addr(SprNr) = GeometryAddr
    _angle(SprNr) = Angle
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
SUB SprDraw_DrawGeometry(FramePtr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    ZP_W0 = spr_vic_bank_addr + SHL(CWORD(FramePtr), 6)
    Angle = Angle AND %11111000

    DIM Index AS BYTE
    DIM Draw AS BYTE

    _bounding_box_left = 23
    _bounding_box_right = 0
    _bounding_box_top = 20
    _bounding_box_bottom = 0

    Draw = $00
    DO UNTIL 0
        Index = PEEK(GeometryAddr)
        GeometryAddr = GeometryAddr + 1
        IF Index = END_SHAPE THEN EXIT DO
        IF Index = NO_DRAW THEN
            Draw = $00
            Index = PEEK(GeometryAddr)
            GeometryAddr = GeometryAddr + 1
        END IF

        sprite_line_x1 = sprite_line_x2
        sprite_line_y1 = sprite_line_y2

        Index = Index + Angle
        sprite_line_x2 = RotX(Index)
        sprite_line_y2 = RotY(Index)

        ASM
bb_left:
            lda {sprite_line_x2}
            cmp {_bounding_box_left}
            bcs bb_right                ;if x2 >= left then bb_right
            sta {_bounding_box_left}
bb_right:
            cmp {_bounding_box_right}
            bcc bb_top                  ;if x2 < right then bb_top
            sta {_bounding_box_right}

bb_top:
            lda {sprite_line_y2}
            cmp {_bounding_box_top}
            bcs bb_bottom               ;if y2 >= top then bb_bottom
            sta {_bounding_box_top}
bb_bottom:
            cmp {_bounding_box_bottom}
            bcc bb_end                  ;if y2 < bottom then bb_end
            sta {_bounding_box_bottom}
bb_end:
        END ASM

        IF Draw THEN CALL _DrawLine()
        Draw = $ff
    LOOP
END SUB

SUB SprDraw_DrawLine(FramePtr AS BYTE, x0 AS BYTE, y0 AS BYTE, x1 AS BYTE, y1 AS BYTE) SHARED STATIC
    ZP_W0 = spr_vic_bank_addr + SHL(CWORD(FramePtr), 6)
    sprite_line_x1 = x0
    sprite_line_y1 = y0
    sprite_line_x2 = x1
    sprite_line_y2 = y1
    CALL _DrawLine()
END SUB

SUB _DrawLine() STATIC
    ASM
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
    END ASM
END SUB

GOTO THE_END

_RotX:
'INCLUDE "x_rotation_table.bas"
_RotY:
'INCLUDE "y_rotation_table.bas"

_pixel_mask:
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01

THE_END:
