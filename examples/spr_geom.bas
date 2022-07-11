INCLUDE "../lib_color.bas"
INCLUDE "../lib_memory.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_spr.bas"
INCLUDE "../lib_spr_geom.bas"
INCLUDE "../lib_scr.bas"

CALL SprGeomPrepare(@GeomTriangle)

CALL SprInit(SPR_MODE_8)
CALL SprGeomInit()

SprDoubleX(0) = TRUE
SprDoubleY(0) = TRUE
SprPriority(0) = TRUE
SprColor(0) = COLOR_WHITE
SprFrame(0) = 254

CALL SprEnable(0, TRUE)

DIM Angle AS BYTE
    Angle = 0
DIM X AS WORD
    X = 130
DIM Y AS BYTE
    Y = 160

GAME_LOOP:
    CALL SprGeomUpdateSprite(0, @GeomTriangle, Angle)
    CALL SprXY(0, X, Y)

    Angle = Angle + 1
    X = X + RotX((Angle AND %11111000) OR 1) - 11
    Y = Y + RotY((Angle AND %11111000) OR 1) - 10

    CALL SprUpdate(TRUE)
GOTO GAME_LOOP

GeomTriangle:
DATA AS BYTE 0, 3
DATA AS BYTE 20, 3
DATA AS BYTE 12, 3
DATA AS BYTE 0, 3
DATA AS BYTE 2, 0
