REM ****************************************************************************
REM 
REM ****************************************************************************
DIM SHARED RegBorderColor AS BYTE @53280
DIM SHARED RegScreenColor AS BYTE @53281

DIM SHARED ZP_W0 AS WORD FAST
DIM SHARED ZP_W1 AS WORD FAST

DIM SHARED ZP_B0 AS BYTE FAST
DIM SHARED ZP_B1 AS BYTE FAST
DIM SHARED ZP_B2 AS BYTE FAST
DIM SHARED ZP_B3 AS BYTE FAST

CONST TRASH_LO = $00
CONST TRASH_HI = $df

DIM bitmap_y_tbl_hi(256) AS BYTE SHARED
DIM bitmap_y_tbl_lo(256) AS BYTE SHARED

SUB InitYTables(bitmap_addr AS WORD) SHARED STATIC
    FOR ZP_B0 = 0 TO 199
        ZP_W0 = bitmap_addr + (ZP_B0 AND 7) + CWORD(320) * SHR(ZP_B0, 3)
        bitmap_y_tbl_lo(ZP_B0) = PEEK(@ZP_W0)
        bitmap_y_tbl_hi(ZP_B0) = PEEK(@ZP_W0 + 1)
    NEXT ZP_B0
    FOR ZP_B0 = 200 TO 255
        bitmap_y_tbl_lo(ZP_B0) = TRASH_LO
        bitmap_y_tbl_hi(ZP_B0) = TRASH_HI
    NEXT ZP_B0
END SUB
