'INCLUDE "lib_sprgeom.bas"
'INCLUDE "lib_spr.bas"

CONST MAX_NUM_SPRITES = 16

DECLARE SUB SprBufInit(FrameStart AS BYTE, NumSprites AS BYTE) SHARED STATIC
DECLARE SUB SprBufRequestGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprBufUpdate(MaxUpdates AS BYTE) SHARED STATIC
DECLARE SUB SprBufDrawGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprBufSwapAll() SHARED STATIC
DECLARE SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM ZP_B0 AS BYTE FAST

DIM SwapAvailable(MAX_NUM_SPRITES) AS BYTE
DIM PrevAngle(MAX_NUM_SPRITES) AS BYTE
DIM PrevGeometry(MAX_NUM_SPRITES) AS WORD
DIM NextAngle(MAX_NUM_SPRITES) AS BYTE
DIM NextGeometry(MAX_NUM_SPRITES) AS WORD

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
        SprFrame(t) = FrameStart + 2 * t
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
        IF NextGeometry(NextUpdate) <> 0 AND (PrevAngle(NextUpdate) <> NextAngle(NextUpdate) OR PrevGeometry(NextUpdate) <> NextGeometry(NextUpdate)) THEN
            CALL SprBufDrawGeometry(NextUpdate, NextGeometry(NextUpdate), NextAngle(NextUpdate)) 
            MaxUpdates = MaxUpdates - 1
        END IF

        NextUpdate = NextUpdate + 1
        IF NextUpdate = num_sprites THEN NextUpdate = 0 

        IF MaxUpdates=0 THEN EXIT SUB
    NEXT t    
END SUB

SUB SprBufDrawGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    Angle = Angle AND %11111000
    ZP_B0 = SprFrame(SprNr) XOR 1
    CALL SprClearFrame(ZP_B0)
    CALL SprGeomDraw(ZP_B0, GeometryAddr, Angle)
    SwapAvailable(SprNr) = $ff
    PrevAngle(SprNr) = Angle
    PrevGeometry(SprNr) = GeometryAddr
END SUB

SUB SprBufSwapAll() SHARED STATIC
    FOR t AS BYTE = 0 TO num_sprites-1
        CALL SprBufSwap(t)
    NEXT t
END SUB

SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC
    IF SwapAvailable(SprNr) THEN
        SprFrame(SprNr) = SprFrame(SprNr) XOR 1
        SwapAvailable(SprNr) = $00
    END IF
END SUB        
