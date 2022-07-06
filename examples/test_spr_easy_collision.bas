INCLUDE "../lib_color.bas"
INCLUDE "../lib_scr.bas"
INCLUDE "../lib_spr_easy.bas"
INCLUDE "../lib_joy.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_random.bas"

RANDOMIZE TI()

DIM pattern(4) AS BYTE
pattern(0) = spr_import_pattern(@SPRITE_BLOCK)
pattern(1) = spr_flip_x_pattern(pattern(0))
pattern(2) = spr_flip_y_pattern(pattern(0))
pattern(3) = spr_flip_y_pattern(pattern(1))

FOR t AS BYTE = 0 TO 7
    CALL spr_config(t, FALSE, TRUE, TRUE, TRUE, 2*t+1)
    CALL spr_pattern(t, pattern(t AND 3))
    CALL spr_xy(t, random_word(0, 320-48), random(0, 200-42))
    CALL spr_enable(t, TRUE)
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
            CALL spr_pattern(t, pattern(((spr_pattern(t)+1) AND 3)))
            spr_color(t) = spr_color(t) + 1
            CALL spr_xy(t, random_word(0, 320-48), random(0, 200-42))
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
