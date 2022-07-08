DECLARE SUB ShapeDrawGeometry(ShapePtr AS WORD, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
DECLARE SUB ShapeClear(ShapePtr AS WORD) SHARED STATIC

DIM VicBank AS WORD
    VicBank = 0

DIM ZP_W0 AS WORD FAST

DIM SHARED RotX(256) AS BYTE @ _RotX
DIM SHARED RotY(256) AS BYTE @ _RotY
DIM PixelMask(24) AS BYTE @_pixel_mask

CONST END_SHAPE  = $10
CONST NO_DRAW    = $20

SUB ShapeInit() SHARED STATIC
    VicBank = 16384 * ((PEEK($dd00) AND %00000011) XOR %00000011)
END SUB

REM ShapePrepare converts human friendly data to packet polyline format.
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
SUB ShapePrepare(GeometryAddr AS WORD) SHARED STATIC
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

SUB ShapeClear(ShapePtr AS WORD) SHARED STATIC
    MEMSET VicBank + SHL(ShapePtr, 6), 63, 0
END SUB

DIM sprite_line_x1 AS BYTE FAST
DIM sprite_line_y1 AS BYTE FAST
DIM sprite_line_x2 AS BYTE FAST
DIM sprite_line_y2 AS BYTE FAST
DIM sprite_line_dx AS BYTE FAST
DIM sprite_line_dy AS BYTE FAST
DIM sprite_line_err AS BYTE FAST

SUB ShapeDrawLine() STATIC
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
        ora {PixelMask},x
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
SUB ShapeDrawGeometry(ShapePtr AS WORD, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    ZP_W0 = VicBank + SHL(ShapePtr, 6)
    Angle = Angle AND %11111000

    DIM Index AS BYTE
    DIM Draw AS BYTE

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

        IF Draw THEN CALL ShapeDrawLine()
        Draw = $ff
    LOOP
END SUB

_pixel_mask:
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01

_RotX:
DATA AS BYTE 11,13,15,17,18,19,20,21
DATA AS BYTE 11,13,15,17,18,19,20,21
DATA AS BYTE 11,13,15,17,17,18,19,20
DATA AS BYTE 11,13,14,16,17,18,18,19
DATA AS BYTE 11,12,14,15,16,17,17,18
DATA AS BYTE 11,12,13,14,15,15,16,17
DATA AS BYTE 11,12,13,13,14,14,14,15
DATA AS BYTE 11,11,12,12,12,13,13,13
DATA AS BYTE 11,11,11,11,11,11,11,11
DATA AS BYTE 11,11,10,10,10,9,9,9
DATA AS BYTE 11,10,9,9,8,8,8,7
DATA AS BYTE 11,10,9,8,7,7,6,5
DATA AS BYTE 11,10,8,7,6,5,5,4
DATA AS BYTE 11,9,8,6,5,4,4,3
DATA AS BYTE 11,9,7,5,5,4,3,2
DATA AS BYTE 11,9,7,5,4,3,2,1
DATA AS BYTE 11,9,7,5,4,3,2,1
DATA AS BYTE 11,9,7,5,4,3,2,1
DATA AS BYTE 11,9,7,5,5,4,3,2
DATA AS BYTE 11,9,8,6,5,4,4,3
DATA AS BYTE 11,10,8,7,6,5,5,4
DATA AS BYTE 11,10,9,8,7,7,6,5
DATA AS BYTE 11,10,9,9,8,8,8,7
DATA AS BYTE 11,11,10,10,10,9,9,9
DATA AS BYTE 11,11,11,11,11,11,11,11
DATA AS BYTE 11,11,12,12,12,13,13,13
DATA AS BYTE 11,12,13,13,14,14,14,15
DATA AS BYTE 11,12,13,14,15,15,16,17
DATA AS BYTE 11,12,14,15,16,17,17,18
DATA AS BYTE 11,13,14,16,17,18,18,19
DATA AS BYTE 11,13,15,17,17,18,19,20
DATA AS BYTE 11,13,15,17,18,19,20,21
_RotY:
DATA AS BYTE 10,10,10,10,10,10,10,10
DATA AS BYTE 10,10,9,9,9,8,8,8
DATA AS BYTE 10,9,8,8,7,7,7,6
DATA AS BYTE 10,9,8,7,6,6,5,4
DATA AS BYTE 10,9,7,6,5,4,4,3
DATA AS BYTE 10,8,7,5,4,3,3,2
DATA AS BYTE 10,8,6,4,4,3,2,1
DATA AS BYTE 10,8,6,4,3,2,1,0
DATA AS BYTE 10,8,6,4,3,2,1,0
DATA AS BYTE 10,8,6,4,3,2,1,0
DATA AS BYTE 10,8,6,4,4,3,2,1
DATA AS BYTE 10,8,7,5,4,3,3,2
DATA AS BYTE 10,9,7,6,5,4,4,3
DATA AS BYTE 10,9,8,7,6,6,5,4
DATA AS BYTE 10,9,8,8,7,7,7,6
DATA AS BYTE 10,10,9,9,9,8,8,8
DATA AS BYTE 10,10,10,10,10,10,10,10
DATA AS BYTE 10,10,11,11,11,12,12,12
DATA AS BYTE 10,11,12,12,13,13,13,14
DATA AS BYTE 10,11,12,13,14,14,15,16
DATA AS BYTE 10,11,13,14,15,16,16,17
DATA AS BYTE 10,12,13,15,16,17,17,18
DATA AS BYTE 10,12,14,16,16,17,18,19
DATA AS BYTE 10,12,14,16,17,18,19,20
DATA AS BYTE 10,12,14,16,17,18,19,20
DATA AS BYTE 10,12,14,16,17,18,19,20
DATA AS BYTE 10,12,14,16,16,17,18,19
DATA AS BYTE 10,12,13,15,16,17,17,18
DATA AS BYTE 10,11,13,14,15,16,16,17
DATA AS BYTE 10,11,12,13,14,14,15,16
DATA AS BYTE 10,11,12,12,13,13,13,14
DATA AS BYTE 10,10,11,11,11,12,12,12
