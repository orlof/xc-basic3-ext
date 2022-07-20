REM ****************************************************************************
REM 
REM ****************************************************************************
DECLARE SUB MemoryVicBank(BankNro AS BYTE) SHARED STATIC

DIM SHARED RegBorderColor AS BYTE @53280
DIM SHARED RegScreenColor AS BYTE @53281

DIM SHARED ZP_W0 AS WORD FAST
DIM SHARED ZP_W1 AS WORD FAST

DIM SHARED ZP_B0 AS BYTE FAST
DIM SHARED ZP_B1 AS BYTE FAST
DIM SHARED ZP_B2 AS BYTE FAST
DIM SHARED ZP_B3 AS BYTE FAST

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM SHARED vic_bank_addr AS WORD
vic_bank_addr = 0

SUB MemoryVicBank(BankNro AS BYTE) SHARED STATIC
    POKE $dd00, (PEEK($dd00) AND %11111100) OR (BankNro XOR %11)
    vic_bank_addr = 16384 * ((PEEK($dd00) AND %00000011) XOR %00000011)
END SUB
