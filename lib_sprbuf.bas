'INCLUDE "lib_sprgeom.bas"
'INCLUDE "lib_spr.bas"

CONST MAX_NUM_SPRITES = 16

DECLARE SUB SprBufDrawGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC
DECLARE SUB SprBufClear(SprNr AS BYTE) SHARED STATIC

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM ZP_W0 AS WORD FAST
DIM ZP_B0 AS BYTE FAST

DIM SwapAvailable(MAX_NUM_SPRITES) AS BYTE
DIM PrevAngle(MAX_NUM_SPRITES) AS BYTE
DIM PrevGeometry(MAX_NUM_SPRITES) AS WORD
DIM NextAngle(MAX_NUM_SPRITES) AS BYTE
DIM NextGeometry(MAX_NUM_SPRITES) AS WORD

DIM DrawCompleted AS BYTE
DIM num_sprites AS BYTE

REM ****************************************************************************
REM CALL spr_init()
REM ****************************************************************************
REM Call before using the library if you change VIC bank or screen memory 
REM address
REM ****************************************************************************
SUB SprBufInit(FrameStart AS BYTE, NumSprites AS BYTE) SHARED STATIC
    num_sprites = NumSprites
    FOR t AS BYTE = 0 TO num_sprites-1
        SwapAvailable(t) = $00
        PrevAngle(t) = 0
        PrevGeometry(t) = 0
        NextAngle(t) = 0
        NextGeometry(t) = 0
        CALL SprClearFrame(FrameStart + 2 * t)
        CALL SprClearFrame(FrameStart + 2 * t + 1)
        CALL SprFrame(t, FrameStart + 2 * t)
    NEXT t
END SUB

SUB SprBufRequestGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    NextAngle(SprNr) = Angle
    NextGeometry(SprNr) = GeometryAddr
END SUB

DIM NextUpdate AS BYTE
    NextUpdate = 0
SUB SprBufUpdate(MaxUpdates AS BYTE) SHARED STATIC
    FOR t AS BYTE = 0 TO num_sprites-1
        CALL SprBufDrawGeometry(NextUpdate, NextGeometry(NextUpdate), NextAngle(NextUpdate)) 
        IF DrawCompleted THEN
            MaxUpdates = MaxUpdates - 1
            IF MaxUpdates=0 THEN 
                NextUpdate = NextUpdate + 1
                IF NextUpdate = num_sprites THEN NextUpdate = 0 
                EXIT SUB
            END IF
        END IF
        NextUpdate = NextUpdate + 1
        IF NextUpdate = num_sprites THEN NextUpdate = 0 
    NEXT t    
END SUB

SUB SprBufDrawGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    DrawCompleted = $00
    Angle = Angle AND %11111000
    IF GeometryAddr = 0 THEN EXIT SUB
    IF PrevAngle(SprNr) = Angle AND PrevGeometry(SprNr) = GeometryAddr THEN EXIT SUB

    ZP_B0 = SprFrame(SprNr) XOR 1
    CALL SprClearFrame(ZP_B0)
    CALL SprGeomDraw(ZP_B0, GeometryAddr, Angle)
    PrevAngle(SprNr) = Angle
    PrevGeometry(SprNr) = GeometryAddr
    SwapAvailable(SprNr) = $ff
    DrawCompleted = $ff
END SUB

SUB SprBufClear(SprNr AS BYTE) SHARED STATIC
    CALL SprClearFrame(SprFrame(SprNr) XOR 1)
END SUB

SUB SprBufSwapAll() SHARED STATIC
    FOR t AS BYTE = 0 TO num_sprites-1
        CALL SprBufSwap(t)
    NEXT t
END SUB

SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC
    IF SwapAvailable(SprNr) THEN
        CALL SprFrame(SprNr, SprFrame(SprNr) XOR 1)
        SwapAvailable(SprNr) = $00
    END IF
END SUB        
