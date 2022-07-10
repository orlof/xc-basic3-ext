INCLUDE "../lib_memory.bas"
INCLUDE "../lib_color.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_spr.bas"
INCLUDE "../lib_spr_geom.bas"
INCLUDE "../lib_scr.bas"

CALL SprInit(SPR_MODE_8)
CALL SprGeomInit()
CALL SprGeomPrepare(@GeomShip)

SprDoubleX(0) = TRUE
SprDoubleY(0) = TRUE
SprPriority(0) = TRUE
SprColor(0) = COLOR_WHITE
SprFrame(0) = 255
CALL SprEnable(0, TRUE)

DIM Angle AS BYTE
    Angle = 0
DIM X AS WORD
    X = 130
DIM Y AS BYTE
    Y = 160
GAME_LOOP:
    CALL SprGeomUpdateSprite(0, @GeomShip, Angle)
    CALL SprXY(0, X, Y)
    Angle = Angle + 1
 
    ' MISUSE THE ROTATION DATA FOR MOVEMENT :-)
    X = X + RotX((Angle AND %11111000) OR 1) - 11
    Y = Y + RotY((Angle AND %11111000) OR 1) - 10
 
    CALL SprUpdate(TRUE)
GOTO game_loop

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
