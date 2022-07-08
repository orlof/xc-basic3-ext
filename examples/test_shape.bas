INCLUDE "../lib_color.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_spr.bas"
INCLUDE "../lib_shape.bas"
INCLUDE "../lib_scr.bas"

CALL ShapePrepare(@GeomTriangle)
CALL ShapePrepare(@GeomShip)

CALL spr_init()
CALL spr_config(0, FALSE, TRUE, TRUE, TRUE, COLOR_WHITE)
CALL spr_pattern(0, 255)
CALL spr_enable(0, TRUE)

DIM Angle AS BYTE
    Angle = 0
DIM X AS WORD
    X = 130
DIM Y AS BYTE
    Y = 160
GAME_LOOP:
    CALL ShapeClear(255)
    CALL ShapeDrawGeometry(255, @GeomShip, Angle)
    CALL spr_xy(0, X, Y)
    Angle = Angle + 1
    X = X + RotX((Angle AND %11111000) OR 1) - 11
    Y = Y + RotY((Angle AND %11111000) OR 1) - 10
    CALL scr_wait_bottom()
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
