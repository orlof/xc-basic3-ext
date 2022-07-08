INCLUDE "../lib_color.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_spr.bas"
INCLUDE "../lib_shape.bas"
INCLUDE "../lib_sprbuf.bas"
INCLUDE "../lib_scr.bas"

CALL ShapePrepare(@GeomTriangle)
CALL ShapePrepare(@GeomShip)

CALL SprBufInit(240)
CALL spr_config(0, FALSE, TRUE, TRUE, TRUE, COLOR_BLACK)
CALL spr_config(1, FALSE, TRUE, TRUE, TRUE, COLOR_WHITE)
CALL spr_enable(0, TRUE)
CALL spr_enable(1, TRUE)

DIM Angle(2) AS BYTE
    Angle(0) = 0
    Angle(1) = 124
DIM X(2) AS WORD
    X(0) = 130
    X(1) = 130
DIM Y(2) AS BYTE
    Y(0) = 160
    Y(1) = 160

GAME_LOOP:
    CALL SprBufDrawGeometry(0, @GeomTriangle, Angle(0))
    CALL SprBufDrawGeometry(1, @GeomTriangle, Angle(1))
    CALL spr_xy(0, X(0), Y(0))
    CALL spr_xy(1, X(1), Y(1))

    Angle(0) = Angle(0) + 1
    Angle(1) = Angle(1) - 1
    X(0) = X(0) + RotX((Angle(0) AND %11111000) OR 1) - 11
    Y(0) = Y(0) + RotY((Angle(0) AND %11111000) OR 1) - 10
    X(1) = X(1) + RotX((Angle(1) AND %11111000) OR 1) - 11
    Y(1) = Y(1) + RotY((Angle(1) AND %11111000) OR 1) - 10
    CALL scr_wait_bottom()
    CALL SprBufSwapAll()
GOTO game_loop

GeomTriangle:
DATA AS BYTE 0, 3
DATA AS BYTE 20, 3
DATA AS BYTE 12, 3
DATA AS BYTE 0, 3
DATA AS BYTE 2, 0
GeomShip:
DATA AS BYTE 0, 7
DATA AS BYTE 4, 2
DATA AS BYTE 8, 7
DATA AS BYTE 12, 2
DATA AS BYTE 20, 2
DATA AS BYTE 24, 7
DATA AS BYTE 28, 2
DATA AS BYTE 0, 7
DATA AS BYTE 2, 0
