INCLUDE "../lib_color.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_spr.bas"
INCLUDE "../lib_sprgeom.bas"
INCLUDE "../lib_sprbuf.bas"
INCLUDE "../lib_scr.bas"

DIM Shape(16) AS WORD
Shape(0) = @GeomShip0
Shape(1) = @GeomShip1
Shape(2) = @GeomShip2
Shape(3) = @GeomShip3
Shape(4) = @GeomShip4
Shape(5) = @GeomShip5
Shape(6) = @GeomShip6
Shape(7) = @GeomShip7
Shape(8) = @GeomShip8
Shape(9) = @GeomShip9
Shape(10) = @GeomShip10
Shape(11) = @GeomShip11
Shape(12) = @GeomShip12
Shape(13) = @GeomShip13
Shape(14) = @GeomShip14
Shape(15) = @GeomShip15
FOR t AS BYTE = 0 TO 15
    CALL SprGeomPrepare(Shape(t))
NEXT t

CALL SprBufInit(240)

FOR t AS BYTE = 0 TO 7
    CALL SprConfig(t, FALSE, TRUE, TRUE, TRUE, 2*t+1)
NEXT t

DIM Angle(8) AS BYTE @_Angle
DIM X(8) AS WORD @_X
DIM Y(8) AS BYTE @_Y

DIM NumSprites AS BYTE
    NumSprites = -1

GAME_LOOP:
    IF NumSprites <> 7 AND (Angle(0) AND %0011111) = 0 THEN
        NumSprites = NumSprites + 1
        CALL SprEnable(NumSprites, TRUE)
    END IF
    FOR t AS BYTE = 0 TO NumSprites
        CALL SprBufRequestGeometry(t, Shape(t), Angle(t))
    NEXT t
    CALL SprBufUpdate(1)
    FOR t AS BYTE = 0 TO NumSprites
        CALL SprXY(t, X(t), Y(t))

        X(t) = X(t) + RotX((Angle(t) AND %11111000) OR 1) - 11
        Y(t) = Y(t) + RotY((Angle(t) AND %11111000) OR 1) - 10
        Angle(t) = Angle(t) + 1
    NEXT t

    'CALL scr_wait_bottom()
    CALL SprBufSwapAll()
GOTO GAME_LOOP

_Angle:
DATA AS BYTE 0,0,0,0,0,0,0,0
_X:
DATA AS WORD 130,130,130,130,130,130,130,130
_Y:
DATA AS BYTE 160,160,160,160,160,160,160,160

GeomShip0:
DATA AS BYTE 0, 3
DATA AS BYTE 20, 3
DATA AS BYTE 12, 3
DATA AS BYTE 0, 3
DATA AS WORD $0002
GeomShip1:
DATA AS BYTE 0, 7
DATA AS BYTE 4, 2
DATA AS BYTE 8, 7
DATA AS BYTE 12, 2
DATA AS BYTE 20, 2
DATA AS BYTE 24, 7
DATA AS BYTE 28, 2
DATA AS BYTE 0, 7
DATA AS WORD $0002
GeomShip2:
DATA AS BYTE 6, 7
DATA AS BYTE 10, 7
DATA AS BYTE 0, 5
DATA AS BYTE 22, 7
DATA AS BYTE 26, 7
DATA AS WORD $0004
DATA AS BYTE 22, 7
DATA AS BYTE 10, 7
DATA AS WORD $0002
GeomShip3:
DATA AS BYTE 6, 7
DATA AS BYTE 10, 7
DATA AS BYTE 4, 2
DATA AS BYTE 0, 7
DATA AS BYTE 28, 2
DATA AS BYTE 22, 7
DATA AS BYTE 26, 7
DATA AS WORD $0004
DATA AS BYTE 22, 7
DATA AS BYTE 10, 7
DATA AS WORD $0002
GeomShip4:
DATA AS BYTE 0, 7
DATA AS BYTE 12, 7
DATA AS BYTE 20, 7
DATA AS BYTE 0, 7
DATA AS WORD $0002
GeomShip5:
DATA AS BYTE 0, 7
DATA AS BYTE 4, 3
DATA AS BYTE 8, 7
DATA AS BYTE 12, 7
DATA AS BYTE 16, 1
DATA AS BYTE 20, 7
DATA AS BYTE 24, 7
DATA AS BYTE 28, 3
DATA AS BYTE 0, 7
DATA AS WORD $0002
GeomShip6:
DATA AS BYTE 0, 7
DATA AS BYTE 4, 4
DATA AS BYTE 8, 7
DATA AS BYTE 0, 2
DATA AS BYTE 24, 7
DATA AS BYTE 28, 4
DATA AS BYTE 0, 7
DATA AS WORD $0002
GeomShip7:
DATA AS BYTE 0, 7
DATA AS BYTE 12, 7
DATA AS BYTE 16, 0
DATA AS BYTE 20, 7
DATA AS BYTE 0, 7
DATA AS WORD $0002
GeomShip8:
DATA AS BYTE 0, 7
DATA AS BYTE 12, 7
DATA AS BYTE 20, 7
DATA AS BYTE 0, 7
DATA AS WORD $0004
DATA AS BYTE 12, 2
DATA AS BYTE 20, 2
DATA AS BYTE 0, 2
DATA AS WORD $0002
GeomShip9:
DATA AS BYTE 0, 4
DATA AS BYTE 18, 6
DATA AS BYTE 14, 6
DATA AS BYTE 0, 4
DATA AS BYTE 0, 7
DATA AS WORD $0002
GeomShip10:
DATA AS BYTE 0, 5
DATA AS BYTE 12, 5
DATA AS BYTE 20, 5
DATA AS BYTE 0, 5
DATA AS WORD $0004
DATA AS BYTE 14, 7
DATA AS BYTE 16, 3
DATA AS BYTE 18, 7
DATA AS BYTE 20, 5
DATA AS WORD $0002
GeomShip11:
DATA AS BYTE 0, 6
DATA AS BYTE 30, 2
DATA AS BYTE 28, 6
DATA AS BYTE 19, 3
DATA AS BYTE 16, 1
DATA AS BYTE 13, 3
DATA AS BYTE 4, 6
DATA AS BYTE 2, 2
DATA AS BYTE 0, 6
DATA AS WORD $0002
GeomShip12:
DATA AS BYTE 0, 7
DATA AS BYTE 26, 4
DATA AS BYTE 22, 4
DATA AS BYTE 10, 4
DATA AS BYTE 6, 4
DATA AS BYTE 0, 7
DATA AS WORD $0002
GeomShip13:
DATA AS BYTE 0, 7
DATA AS BYTE 30, 5
DATA AS BYTE 18, 5
DATA AS BYTE 14, 5
DATA AS BYTE 2, 5
DATA AS BYTE 0, 7
DATA AS WORD $0002
GeomShip14:
DATA AS BYTE 28, 4
DATA AS BYTE 20, 5
DATA AS BYTE 4, 4
DATA AS BYTE 12, 5
DATA AS BYTE 28, 4
DATA AS BYTE 0, 7
DATA AS BYTE 4, 4
DATA AS WORD $0002
GeomShip15:
DATA AS BYTE 0, 6
DATA AS BYTE 25, 5
DATA AS BYTE 20, 6
DATA AS BYTE 12, 6
DATA AS BYTE 7, 5
DATA AS BYTE 0, 6
DATA AS WORD $0004
DATA AS BYTE 20, 3
DATA AS BYTE 12, 3
DATA AS BYTE 0, 3
DATA AS WORD $0002
