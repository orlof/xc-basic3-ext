INCLUDE "../lib_color.bas"
INCLUDE "../lib_scr.bas"
INCLUDE "../lib_spr_easy.bas"
INCLUDE "../lib_joy.bas"
INCLUDE "../lib_types.bas"
INCLUDE "../lib_random.bas"

RANDOMIZE TI()

DIM B AS BYTE
    B = spr_import_pattern(@SPRITE_BLOCK)

FOR t AS INT = 0 TO 7
    CALL spr_config(t, FALSE, t=2 OR t=3 OR t=7, t=3 OR t=4 OR t=7, TRUE, 2*t+1)
    CALL spr_enable(t, TRUE)
    CALL spr_pattern(t, B)
    CALL spr_xy(t, random_word(0, 319), RNDB())
NEXT t

DIM x AS INT
    x = 100-24
DIM y AS INT
    y = 100-21

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
    DIM real_x AS WORD
        real_x = (x + 24) AND 511
    IF real_x >= 480 THEN real_x = real_x - 8
    PRINT x;" -> ";real_x;"    "
    PRINT y;" -> ";(y + 50) AND 255;"    "

    CALL spr_detect(7)
    FOR t AS BYTE = 0 TO 7
        IF spr_col(t) THEN
            spr_color(t) = spr_color(t) + 1
        END IF
    NEXT t

    CALL scr_wait_bottom()
    GOTO game_loop
END

SPRITE_BLOCK:
rem sprite Block / singlecolor
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
