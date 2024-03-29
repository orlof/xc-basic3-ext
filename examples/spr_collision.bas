
INCLUDE "../lib_color.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_memory.bas"
INCLUDE "../lib_char.bas"
INCLUDE "../lib_irq.bas"
INCLUDE "../lib_scr.bas"
INCLUDE "../lib_spr.bas"
INCLUDE "../lib_spr_shape.bas"
INCLUDE "../lib_joy.bas"
INCLUDE "../lib_random.bas"

RANDOMIZE TI()

CALL SprInit(0, 1)
CALL SprShapeImport(@SPRITE_BLOCK, 255)

FOR t AS BYTE = 0 TO 15
    SprColor(t) = t AND 3
    SprFrame(t) = 255
    CALL SprXY(t, random_word(0, 320-48), random(0, 200-42))
NEXT t

DIM x AS INT
    x = (320-24) / 2
DIM y AS INT
    y = (200-21) / 2

game_loop:
    CALL Joy1.Update()
    IF joy1.North() THEN 
        y = y - 1
    END IF
    IF joy1.South() THEN 
        y = y + 1
    END IF
    IF joy1.East() THEN 
        x = x + 1
    END IF
    IF joy1.West() THEN 
        x = x - 1
    END IF
    
    CALL SprXY(0, x, y)

    LOCATE 0,0
    PRINT x;"    "
    PRINT y;"    "

    IF SprRecordCollisions(0) THEN
        FOR t AS BYTE = 0 TO 15
            IF SprCollision(t) THEN
                CALL SprXY(t, random_word(0, 320-48), random(0, 200-42))
            END IF
        NEXT t
    END IF

    CALL SprUpdate(TRUE)
    GOTO game_loop

SPRITE_BLOCK:
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
DATA AS BYTE %11111111,%11111111,%11111111
