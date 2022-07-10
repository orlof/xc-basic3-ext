REM ****************************************************************************
REM 
REM ****************************************************************************
DECLARE SUB MemoryVicBank(BankNro AS BYTE) SHARED STATIC

DIM SHARED ZP_W0 AS WORD FAST
DIM SHARED ZP_W1 AS WORD FAST

REM **************************************
REM INTERNAL FIELDS
REM **************************************
DIM SHARED vic_bank_addr AS WORD
vic_bank_addr = 0

SUB MemoryVicBank(BankNro AS BYTE) SHARED STATIC
    POKE $dd00, (PEEK($dd00) AND %11111100) OR (BankNro XOR %11)
    vic_bank_addr = 16384 * ((PEEK($dd00) AND %00000011) XOR %00000011)
END SUB
