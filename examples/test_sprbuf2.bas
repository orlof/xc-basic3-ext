INCLUDE "../lib_color.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_memory.bas"
INCLUDE "../lib_spr.bas"
INCLUDE "../lib_sprgeom.bas"
INCLUDE "../lib_sprbuf.bas"
INCLUDE "../lib_scr.bas"

CONST MAX_NUM_SPRITES = 4

CALL Scr_Clear()
ScreenColor = COLOR_BLACK
BorderColor = COLOR_BLACK

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

CALL SprInit(MAX_NUM_SPRITES)
CALL SprBufInit(256 - 2 * MAX_NUM_SPRITES, MAX_NUM_SPRITES)
print "init "; MAX_NUM_SPRITES

DIM X(MAX_NUM_SPRITES) AS WORD
DIM Y(MAX_NUM_SPRITES) AS BYTE

FOR t AS BYTE = 0 TO MAX_NUM_SPRITES-1
    IF t = (ScreenColor AND %111) THEN
        SprColor(t) = t XOR %100
    ELSE
        SprColor(t) = t
    END IF
    SprDoubleX(t) = (MAX_NUM_SPRITES <= 8)
    SprDoubleY(t) = (MAX_NUM_SPRITES <= 8)
    X(t) = 130
    Y(t) = 160
NEXT t

DIM NumSprites AS BYTE
    NumSprites = 0
DIM Angle AS BYTE
    Angle = 0
DIM Trigger AS BYTE
    Trigger = 256 / MAX_NUM_SPRITES
DIM NumUpdates AS BYTE
    NumUpdates = 2
    IF MAX_NUM_SPRITES <= 8 THEN NumUpdates = 1

print "start"
GAME_LOOP:
    IF (NumSprites < MAX_NUM_SPRITES) AND ((Angle AND (Trigger-1)) = 0) THEN
        CALL SprEnable(NumSprites, TRUE)
        NumSprites = NumSprites + 1
        PRINT NumSprites
    END IF
    FOR t AS BYTE = 0 TO NumSprites-1
        DIM a AS BYTE
            a = Angle - Trigger * t
        CALL SprBufRequestGeometry(t, Shape(t), a + 4)
        CALL SprXY(t, X(t), Y(t))

        X(t) = X(t) + RotX((a AND %11111000) OR 1) - 11
        Y(t) = Y(t) + RotY((a AND %11111000) OR 1) - 10
    NEXT t
    Angle = Angle + 1
    CALL SprBufUpdate(NumUpdates)
    CALL SprBufSwapAll()
    CALL SpriteUpdate(TRUE)
GOTO GAME_LOOP

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
DATA AS BYTE 0, 2
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
DATA AS BYTE 12, 5
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
DATA AS BYTE 0, 3
DATA AS BYTE 20, 3
DATA AS BYTE 12, 3
DATA AS BYTE 0, 3
DATA AS WORD $0002
