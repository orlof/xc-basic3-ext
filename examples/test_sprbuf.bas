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
SprColor(0) = COLOR_BLACK
SprFrame(0) = 254

SprDoubleX(1) = TRUE
SprDoubleY(1) = TRUE
SprPriority(1) = TRUE
SprColor(1) = COLOR_WHITE
SprFrame(1) = 252

CALL SprEnable(0, TRUE)
CALL SprEnable(1, TRUE)

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
    CALL SprGeomUpdateSprite(0, @GeomTriangle, Angle(0))
    CALL SprGeomUpdateSprite(1, @GeomTriangle, Angle(1))
    CALL SprXY(0, X(0), Y(0))
    CALL SprXY(1, X(1), Y(1))

    Angle(0) = Angle(0) + 1
    Angle(1) = Angle(1) - 1
    X(0) = X(0) + RotX((Angle(0) AND %11111000) OR 1) - 11
    Y(0) = Y(0) + RotY((Angle(0) AND %11111000) OR 1) - 10
    X(1) = X(1) + RotX((Angle(1) AND %11111000) OR 1) - 11
    Y(1) = Y(1) + RotY((Angle(1) AND %11111000) OR 1) - 10

    CALL SprUpdate(TRUE)
GOTO GAME_LOOP

GeomTriangle:
DATA AS BYTE 0, 3
DATA AS BYTE 20, 3
DATA AS BYTE 12, 3
DATA AS BYTE 0, 3
DATA AS BYTE 2, 0
