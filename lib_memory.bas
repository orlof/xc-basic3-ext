REM ****************************************************************************
REM 
REM ****************************************************************************
DIM SHARED RegBorderColor AS BYTE @53280
DIM SHARED RegScreenColor AS BYTE @53281

DIM SHARED ZP_L0 AS LONG FAST
DIM SHARED ZP_L1 AS LONG FAST
DIM SHARED ZP_I0 AS INT FAST

DIM SHARED ZP_W0 AS WORD FAST
DIM SHARED ZP_W1 AS WORD FAST
DIM SHARED ZP_W2 AS WORD FAST

DIM SHARED ZP_B0 AS BYTE FAST
DIM SHARED ZP_B1 AS BYTE FAST
DIM SHARED ZP_B2 AS BYTE FAST
DIM SHARED ZP_B3 AS BYTE FAST

CONST TRASH_LO = $00
CONST TRASH_HI = $de

DIM bitmap_y_tbl_hi(256) AS BYTE SHARED
DIM bitmap_y_tbl_lo(256) AS BYTE SHARED

SUB InitYTables(bitmap_addr AS WORD) SHARED STATIC
    ASM
        ldy #199
y_tbl_init_loop
        lda #0
        sta {ZP_B0}

        tya
        lsr
        lsr
        lsr
        sta {ZP_W0}+1       ; * 256

        tya
        and #%11111000
        asl
        rol {ZP_B0}
        asl
        rol {ZP_B0}
        asl
        rol {ZP_B0}         ; * 64

        sta {ZP_W0}

        clc
        lda {ZP_B0}
        adc {ZP_W0}+1
        sta {ZP_W0}+1       ; 256y + 64y

        tya
        and #7
        clc
        adc {ZP_W0}
        sta {ZP_W0}
        lda {ZP_W0}+1
        adc #0
        sta {ZP_W0}+1       ; +(y & 7)

        clc
        lda {bitmap_addr}
        adc {ZP_W0}
        sta {bitmap_y_tbl_lo},y

        lda {bitmap_addr}+1
        adc {ZP_W0}+1
        sta {bitmap_y_tbl_hi},y

        cpy #0
        beq y_tbl_init_tail

        dey
        jmp y_tbl_init_loop

y_tbl_init_tail
        ldy #200
y_tbl_init_tail_loop
        lda #$de
        sta {bitmap_y_tbl_hi},y
        lda #$00
        sta {bitmap_y_tbl_lo},y

        iny
        bne y_tbl_init_tail_loop
    END ASM
END SUB
