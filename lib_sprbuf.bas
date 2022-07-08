'INCLUDE "lib_shape.bas"
'INCLUDE "lib_spr.bas"

DECLARE SUB SprBufDrawGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC
DECLARE SUB SprBufClear(SprNr AS BYTE) SHARED STATIC

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM ZP_W0 AS WORD FAST
DIM ZP_B0 AS BYTE FAST

DIM SwapAvailable(8) AS BYTE @_SwapAvailable
DIM PrevAngle(8) AS BYTE @_PrevAngle
DIM PrevGeometry(8) AS WORD @_PrevGeometry
DIM NextAngle(8) AS BYTE @_NextAngle
DIM NextGeometry(8) AS WORD @_NextGeometry

DIM DrawCompleted AS BYTE

REM ****************************************************************************
REM CALL spr_init()
REM ****************************************************************************
REM Call before using the library if you change VIC bank or screen memory 
REM address
REM ****************************************************************************
SUB SprBufInit(FrameStart AS BYTE) SHARED STATIC
    FOR t AS BYTE = 0 TO 7
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
    FOR t AS BYTE = 0 TO 7
        CALL SprBufDrawGeometry(NextUpdate, NextGeometry(NextUpdate), NextAngle(NextUpdate)) 
        IF DrawCompleted THEN
            MaxUpdates = MaxUpdates - 1
            IF MaxUpdates=0 THEN 
                NextUpdate = NextUpdate + 1
                IF NextUpdate = 8 THEN NextUpdate = 0 
                EXIT SUB
            END IF
        END IF
        NextUpdate = NextUpdate + 1
        IF NextUpdate = 8 THEN NextUpdate = 0 
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
    ZP_B0 = SprFrame(SprNr) XOR 1
    CALL SprClearFrame(ZP_B0)
END SUB

SUB SprBufSwapAll() SHARED STATIC
    FOR t AS BYTE = 0 TO 7
        CALL SprBufSwap(t)
    NEXT t
END SUB

SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC
    IF SwapAvailable(SprNr) THEN
        CALL SprFrame(SprNr, SprFrame(SprNr) XOR 1)
        SwapAvailable(SprNr) = $00
    END IF
END SUB        

_SwapAvailable:
DATA AS BYTE 0,0,0,0,0,0,0,0
_PrevAngle:
DATA AS BYTE 0,0,0,0,0,0,0,0
_NextAngle:
DATA AS BYTE 0,0,0,0,0,0,0,0
_PrevGeometry:
DATA AS WORD 0,0,0,0,0,0,0,0
_NextGeometry:
DATA AS WORD 0,0,0,0,0,0,0,0
