INCLUDE "lib_memory.bas"

TYPE TextScreen
    BorderColor AS BYTE
    ScreenColor AS BYTE

    vic_bank_ptr AS BYTE
    vic_bank_addr AS WORD
    screen_mem_ptr AS BYTE
    screen_mem_addr AS WORD
    bitmap_ptr AS BYTE
    bitmap_addr AS WORD

END TYPE

CONST DEFAULT_BITMAP = 1
CONST DEFAULT_SCRMEM = 7

DECLARE SUB hires_setup(Bitmap AS BYTE, ScrMem AS BYTE) SHARED STATIC
DECLARE SUB hires_on() SHARED STATIC
DECLARE SUB hires_off() SHARED STATIC
DECLARE SUB hires_clear() SHARED STATIC
DECLARE SUB hires_color(inkcol AS BYTE, bgcol AS BYTE) SHARED STATIC
DECLARE SUB hires_text(x AS BYTE, y AS BYTE, len AS BYTE, text AS STRING * 40) SHARED STATIC

DECLARE SUB hires_set(x AS BYTE, y AS BYTE) SHARED STATIC
DECLARE SUB hires_unset() SHARED STATIC

DIM _bitmap AS BYTE
DIM _scrmem AS BYTE

DIM HiresSetAddr AS WORD FAST
DIM HiresUnsetAddr AS WORD FAST

DIM bitmap_addr AS WORD
DIM scrmem_addr AS WORD
    scrmem_addr = 1024

DIM y_tbl_hi(200) AS BYTE
DIM y_tbl_lo(200) AS BYTE

DIM x_tbl(200) AS BYTE
DIM pixel_mask(200) AS BYTE
    FOR t AS BYTE = 0 TO 192 STEP 8
        FOR t2 AS BYTE = 0 TO 7
            x_tbl(t+t2) = t
            pixel_mask(t+t2) = SHR($80, t2)
        NEXT t2
    NEXT t

REM BitMap 0-1
REM ScrMem 0-15
SUB HiresSetup(Bitmap AS BYTE, ScrMem AS BYTE) SHARED STATIC
    _bitmap = Bitmap
    _scrmem = ScrMem

    bitmap_addr = vic_bank_addr + 8192 * _bitmap
    scrmem_addr = vic_bank_addr + 1024 * _scrmem

    FOR t AS BYTE = 0 TO 199
        DIM addr AS WORD
        addr = bitmap_addr + (t AND 7) + CWORD(320) * SHR(t, 3)
        y_tbl_lo(t) = PEEK(@addr)
        y_tbl_hi(t) = PEEK(@addr+1)
    NEXT t
END SUB

SUB hires_mode() SHARED STATIC
    rem -- BITMAP 0 to 1, SCRMEM 0 to 15
    poke $d018, SHL(_scrmem, 4) OR SHL(_bitmap, 3)

    rem -- Bitmap mode on
    poke $d011, peek($d011) OR %00100000

    rem -- Multicolor mode off
    poke $d016, peek($d016) AND %11101111
END SUB

SUB hires_clear() SHARED STATIC
    MEMSET BitmapAddr, 8000, 0
END SUB

SUB hires_color(inkcol AS BYTE, bgcol AS BYTE) SHARED STATIC
    MEMSET ScrMemAddr, 1000, SHL(inkcol, 4) OR bgcol
END SUB

SUB hires_unset() SHARED STATIC
    ASM
        lda #0
        jmp ({HiresUnsetAddr})
        REPEAT 32
            sta $dead
        REPEND

hires_unset_end
        lda #<hires_unset_end
        sta {HiresUnsetAddr}
        lda #>hires_unset_end
        sta {HiresUnsetAddr}+1
    END ASM
END SUB

SUB hires_set(x AS BYTE, y AS BYTE) SHARED STATIC
    ASM
        ldx {x}
        cpx #200
        bcs hires_set_end

        ldy {y}
        cpy #200
        bcs hires_set_end

        lda {y_tbl_lo},y
        clc
        adc {x_tbl},x
        sta {HiresSetAddr}
        lda #0
        adc {y_tbl_hi},y
        sta {HiresSetAddr}+1

        lda {pixel_mask},x
        ldy #0

        ora ({HiresSetAddr}),y
        sta ({HiresSetAddr}),y

        lda {HiresUnsetAddr}                     ; add to erase queue
        sec
        sbc #3
        sta {HiresUnsetAddr}
        bcs hires_set_no_borrow
        dec {HiresUnsetAddr}+1
hires_set_no_borrow
        ldy #1
        lda {HiresSetAddr}
        sta ({HiresUnsetAddr}),y
        iny
        lda {HiresSetAddr}+1
        sta ({HiresUnsetAddr}),y
hires_set_end
    END ASM
END SUB

DIM PETSCII_TO_SCREENCODE(8) AS BYTE @ _PETSCII_TO_SCREENCODE
_PETSCII_TO_SCREENCODE:
DATA AS BYTE $80, $00, $c0, $e0, $40, $c0, $80, $80

SUB hires_text(x AS BYTE, y AS BYTE, len AS BYTE, text AS STRING * 40) SHARED STATIC
    rem disable interrupt and enable char rom
    ASM
        sei
    END ASM
    POKE 1, PEEK(1) AND %11111011

    DIM dst AS WORD: dst = SHL(y_tbl_hi(8*y), 8) + y_tbl_lo(8*y) + CWORD(8) * CWORD(x)
    MEMSET dst, CWORD(8) * len, 0
    FOR t AS BYTE = 1 TO LEN(text)
        DIM c AS BYTE: c = PEEK(@text + t)
        IF c=$ff THEN
            c = $5e
        ELSE
            c = c + PETSCII_TO_SCREENCODE(SHR(c, 5)) 
        END IF
        MEMSHIFT $D800 + CWORD(8) * CWORD(c), dst, 8
        dst = dst + 8
    NEXT t

    rem disable char ROM and enable interrupts
    POKE 1, PEEK(1) OR %00000100
    ASM
        cli
    END ASM
END SUB
