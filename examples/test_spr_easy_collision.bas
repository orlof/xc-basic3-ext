INCLUDE "../lib_color.bas"
INCLUDE "../lib_scr.bas"
INCLUDE "../lib_spr_easy.bas"
INCLUDE "../lib_joy.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_random.bas"

RANDOMIZE TI()

DIM LB AS BYTE
    LB = spr_import_pattern(@SPRITE_BLOCK)
DIM RB AS BYTE
    RB = spr_flip_x_pattern(LB)
DIM LT AS BYTE
    LT = spr_flip_y_pattern(LB)
DIM RT AS BYTE
    RT = spr_flip_y_pattern(RB)

FOR t AS BYTE = 0 TO 7
    CALL spr_config(t, FALSE, t=2 OR t=3 OR t=7, t=3 OR t=4 OR t=7, TRUE, 2*t+1)
    CALL spr_enable(t, TRUE)
    CALL spr_pattern(t, random(252, 255))
    CALL spr_xy(t, random_word(0, 319), RNDB())
NEXT t

DIM x AS INT
    x = (320-48) / 2
DIM y AS INT
    y = (200-42) / 2

CALL scr_clear()

game_loop:
    IF joy1_up() THEN 
        y = y - 1
    END IF
    IF joy1_down() THEN 
        y = y + 1
    END IF
    IF joy1_right() THEN 
        x = x + 1
    END IF
    IF joy1_left() THEN 
        x = x - 1
    END IF
    
    CALL spr_xy(7, x, y)

    LOCATE 0,0
    PRINT x;"    "
    PRINT y;"    "

    CALL spr_detect(7)
    FOR t AS BYTE = 0 TO 7
        IF spr_col(t) THEN
            CALL spr_pattern(t, random(252, 255))
            spr_color(t) = spr_color(t) + 1
        END IF
    NEXT t

    CALL scr_wait_bottom()
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
DATA AS BYTE %11111111,%11111111,%00000000
DATA AS BYTE %11111111,%11111111,%00000000
DATA AS BYTE %11111111,%11111111,%00000000
DATA AS BYTE %11111111,%11111111,%00000000
