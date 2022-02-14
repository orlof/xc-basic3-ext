include "xcb_sprmux.bas"

REM init sprite
MEMSET CWORD(192) * CWORD(64), 64, 255

FOR t AS BYTE = 0 TO 15
    CALL SpriteColor(t, t)
    CALL SpriteAt(t, 15+9*t, 20+14*t)
    CALL SpriteShape(t, 192)
NEXT t

REM start raster interrupts
CALL SpriteInit()

loop:
    FOR spr_nr AS BYTE = 0 TO 15
        CALL SpriteMove(spr_nr, 0, 1)
    NEXT spr_nr
    CALL SpriteUpdate()
goto loop
