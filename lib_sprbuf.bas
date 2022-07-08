'INCLUDE "lib_shape.bas"
'INCLUDE "lib_spr.bas"

DECLARE FUNCTION SprBufDrawGeometry AS BYTE(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
DECLARE SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC


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

REM ****************************************************************************
REM CALL spr_init()
REM ****************************************************************************
REM Call before using the library if you change VIC bank or screen memory 
REM address
REM ****************************************************************************
SUB SprBufInit(FrameStart AS BYTE) SHARED STATIC
    FOR t AS BYTE = 0 TO 7
        CALL spr_pattern(t, FrameStart + 2 * t)
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
        IF SprBufDrawGeometry(NextUpdate, NextGeometry(NextUpdate), NextAngle(NextUpdate)) THEN
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

FUNCTION SprBufDrawGeometry AS BYTE(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    Angle = Angle AND %11111000
    IF GeometryAddr = 0 THEN RETURN $00
    IF PrevAngle(SprNr) = Angle AND PrevGeometry(SprNr) = GeometryAddr THEN RETURN $00

    ZP_B0 = spr_pattern(SprNr) XOR 1
    CALL ShapeClear(ZP_B0)
    CALL ShapeDrawGeometry(ZP_B0, GeometryAddr, Angle)
    PrevAngle(SprNr) = Angle
    PrevGeometry(SprNr) = GeometryAddr
    SwapAvailable(SprNr) = $ff
    RETURN $ff
END FUNCTION

SUB SprBufClear(SprNr AS BYTE) SHARED STATIC
    ZP_B0 = spr_pattern(SprNr) XOR 1
    CALL ShapeClear(ZP_B0)
END SUB

SUB SprBufSwapAll() SHARED STATIC
    FOR t AS BYTE = 0 TO 7
        CALL SprBufSwap(t)
    NEXT t
END SUB

SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC
    IF SwapAvailable(SprNr) THEN
        CALL spr_pattern(SprNr, spr_pattern(SprNr) XOR 1)
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
