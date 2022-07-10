'INCLUDE "lib_memory.bas"
'INCLUDE "lib_types.bas"
'INCLUDE "lib_sprgeom.bas"
'INCLUDE "lib_spr.bas"

CONST MAX_NUM_SPRITES = 16

DECLARE SUB SprBufInit(FrameStart AS BYTE, NumSprites AS BYTE) SHARED STATIC
DECLARE SUB SprBufDrawGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprBufRequestGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprBufUpdate(MaxUpdates AS BYTE) SHARED STATIC
DECLARE SUB SprBufSwapAll() SHARED STATIC
DECLARE SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM ZP_B0 AS BYTE FAST

DIM ReqDone(MAX_NUM_SPRITES) AS BYTE
DIM CurAngle(MAX_NUM_SPRITES) AS BYTE
DIM CurGeom(MAX_NUM_SPRITES) AS WORD
DIM ReqAngle(MAX_NUM_SPRITES) AS BYTE
DIM ReqGeom(MAX_NUM_SPRITES) AS WORD

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
        ReqDone(t) = FALSE
        CurAngle(t) = 0
        CurGeom(t) = 0
        ReqAngle(t) = 0
        ReqGeom(t) = 0
        CALL SprClearFrame(FrameStart + 2 * t)
        CALL SprClearFrame(FrameStart + 2 * t + 1)
        SprFrame(t) = FrameStart + 2 * t
    NEXT t
END SUB

SUB SprBufRequestGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    ReqAngle(SprNr) = Angle
    ReqGeom(SprNr) = GeometryAddr
END SUB

DIM NextSprNr AS BYTE
    NextSprNr = 0
SUB SprBufUpdate(MaxUpdates AS BYTE) SHARED STATIC
    DIM StartSprNr AS BYTE
        StartSprNr = NextSprNr
    DO
        IF ReqGeom(NextSprNr) <> 0 AND (CurAngle(NextSprNr) <> ReqAngle(NextSprNr) OR CurGeom(NextSprNr) <> ReqGeom(NextSprNr)) THEN
            CALL SprBufDrawGeometry(NextSprNr, ReqGeom(NextSprNr), ReqAngle(NextSprNr)) 
            MaxUpdates = MaxUpdates - 1
        END IF

        NextSprNr = NextSprNr + 1
        IF NextSprNr = num_sprites THEN NextSprNr = 0 
    LOOP UNTIL NextSprNr = startSprNr OR MaxUpdates = 0
END SUB

SUB SprBufDrawGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    DIM FramePtr AS BYTE
        FramePtr = SprFrame(SprNr) XOR 1
    Angle = Angle AND %11111000

    CALL SprClearFrame(FramePtr)
    CALL SprGeomDraw(FramePtr, GeometryAddr, Angle)

    ReqDone(SprNr) = TRUE
    CurAngle(SprNr) = Angle
    CurGeom(SprNr) = GeometryAddr

    CALL SprBufSwap(SprNr)
END SUB

SUB SprBufSwapAll() SHARED STATIC
    FOR t AS BYTE = 0 TO num_sprites-1
        CALL SprBufSwap(t)
    NEXT t
END SUB

SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC
    IF ReqDone(SprNr) THEN
        SprFrame(SprNr) = SprFrame(SprNr) XOR 1
        ReqDone(SprNr) = $00
    END IF
END SUB        
