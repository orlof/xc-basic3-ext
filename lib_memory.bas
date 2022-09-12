REM ****************************************************************************
REM 
REM ****************************************************************************
DIM SHARED RegBorderColor AS BYTE @53280
DIM SHARED RegScreenColor AS BYTE @53281

DIM SHARED ZP_L0 AS LONG @$24
DIM SHARED ZP_L1 AS LONG @$21
DIM SHARED ZP_I0 AS INT @$1f

DIM SHARED ZP_W0 AS WORD @$19
DIM SHARED ZP_W1 AS WORD @$1b
DIM SHARED ZP_W2 AS WORD @$1d

DIM SHARED ZP_B0 AS BYTE @$15
DIM SHARED ZP_B1 AS BYTE @$16
DIM SHARED ZP_B2 AS BYTE @$17
DIM SHARED ZP_B3 AS BYTE @$18

CONST TRASH_LO = $00
CONST TRASH_HI = $de

DIM SHARED bitmap_y_tbl_hi(256) AS BYTE
DIM SHARED bitmap_y_tbl_lo(256) AS BYTE

DIM SHARED bit_shift_left(8) AS BYTE @_bit_shift_left

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
        lda #$04
        sta {bitmap_y_tbl_hi},y
        lda #$00
        sta {bitmap_y_tbl_lo},y

        iny
        bne y_tbl_init_tail_loop
    END ASM
END SUB

SUB ImportRle(SrcAddr AS WORD, DstAddr AS WORD) SHARED STATIC
    ZP_W0 = SrcAddr
    ZP_W1 = DstAddr
    ASM
rle_next
        ldy #0          ; ZP_B0 = runlength
        lda ({ZP_W0}),y
        beq rle_end
        bpl rle_runlength
rle_singles
        eor #$ff
        clc
        adc #2
        sta {ZP_B0}
        tay
        dey

        sec
        lda {ZP_W1}
        sbc #1
        sta {ZP_W1}
        lda {ZP_W1}+1
        sbc #0
        sta {ZP_W1}+1

rle_singles_loop
        lda ({ZP_W0}),y
        sta ({ZP_W1}),y

        dey
        bne rle_singles_loop

        clc
        lda {ZP_W0}
        adc {ZP_B0}
        sta {ZP_W0}
        lda {ZP_W0}+1
        adc #0
        sta {ZP_W0}+1

        jmp rle_advance_output

rle_runlength
        sta {ZP_B0}

        iny             ; a = byte
        lda ({ZP_W0}),y

        ldy {ZP_B0}
rle_runlength_loop
        dey
        sta ({ZP_W1}),y
        bne rle_runlength_loop

        clc
        lda {ZP_W0}
        adc #2
        sta {ZP_W0}
        lda {ZP_W0}+1
        adc #0
        sta {ZP_W0}+1

rle_advance_output
        clc
        lda {ZP_W1}
        adc {ZP_B0}
        sta {ZP_W1}
        lda {ZP_W1}+1
        adc #0
        sta {ZP_W1}+1

        jmp rle_next
rle_end
        clc
        lda {ZP_W0}
        adc #1
        sta {ZP_W0}
        lda {ZP_W0}+1
        adc #0
        sta {ZP_W0}+1
    END ASM
END SUB

GOTO THE_END

_bit_shift_left:
DATA AS BYTE 1,2,4,8,16,32,64,128

THE_END: