'INCLUDE "lib_shape.bas"
'INCLUDE "lib_spr.bas"

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM ZP_W0 AS WORD FAST
DIM ZP_B0 AS BYTE FAST

DIM SwapAvailable(8) AS BYTE
DIM PrevAngle(8) AS BYTE
DIM PrevGeometry(8) AS WORD

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

SUB SprBufDrawGeometry(SprNr AS BYTE, GeometryAddr AS WORD, Angle AS BYTE) SHARED STATIC
    Angle = Angle AND %11111000
    IF PrevAngle(SprNr) <> Angle OR PrevGeometry(SprNr) <> GeometryAddr THEN
        ZP_B0 = spr_pattern(SprNr) XOR 1
        CALL ShapeClear(ZP_B0)
        CALL ShapeDrawGeometry(ZP_B0, GeometryAddr, Angle)
        PrevAngle(SprNr) = Angle
        PrevGeometry(SprNr) = GeometryAddr
        SwapAvailable(SprNr) = $ff
    END IF
END SUB

SUB SprBufClear(SprNr AS BYTE) SHARED STATIC
    ZP_B0 = spr_pattern(SprNr) XOR 1
    CALL ShapeClear(ZP_B0)
END SUB

SUB SprBufSwapAll() SHARED STATIC
    FOR t AS BYTE = 0 TO 7
        IF SwapAvailable(t) THEN
            CALL spr_pattern(t, spr_pattern(t) XOR 1)
            SwapAvailable(t) = $00
        END IF
    NEXT t
END SUB

SUB SprBufSwap(SprNr AS BYTE) SHARED STATIC
    IF SwapAvailable(SprNr) THEN
        CALL spr_pattern(SprNr, spr_pattern(SprNr) XOR 1)
        SwapAvailable(SprNr) = $00
    END IF
END SUB        

