include "../xcb_sprmux.bas"

REM init sprite
MEMSET $3000, 64, 255

FOR spr_nr AS BYTE = 0 TO 15
    CALL SpriteColor(spr_nr, spr_nr)
    CALL SpriteAt(spr_nr, 15+9*spr_nr, 20+14*spr_nr)
    CALL SpriteShape(spr_nr, 192)
NEXT spr_nr

REM start raster interrupts
CALL SpriteInit()

loop:
    FOR spr_nr AS BYTE = 0 TO 15
        CALL SpriteMove(spr_nr, 0, 1)
    NEXT spr_nr
    CALL SpriteUpdate()
goto loop
