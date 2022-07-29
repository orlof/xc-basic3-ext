'INCLUDE "lib_memory.bas"

DIM SHARED TRASH AS BYTE @$df00

DIM hires_y_tbl_hi(256) AS BYTE SHARED
DIM hires_y_tbl_lo(256) AS BYTE SHARED

DIM hires_mask0(8) AS BYTE @_hires_mask0 SHARED
_hires_mask0:
DATA AS BYTE $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
DIM hires_mask1(8) AS BYTE @_hires_mask1 SHARED
_hires_mask1:
DATA AS BYTE $80, $40, $20, $10, $08, $04, $02, $01

TYPE ScreenHires
    BorderColor AS BYTE
    ScreenColor AS BYTE

    vic_bank_ptr AS BYTE
    vic_bank_addr AS WORD
    screen_mem_ptr AS BYTE
    screen_mem_addr AS WORD
    bitmap_ptr AS BYTE
    bitmap_addr AS WORD

    REM BitMap 0-1
    REM ScrMem 0-15
    SUB Init(VicBankPtr AS BYTE, BitmapPtr AS BYTE, ScreenMemPtr AS BYTE) STATIC
        THIS.vic_bank_ptr = VicBankPtr
        THIS.bitmap_ptr = BitmapPtr
        THIS.screen_mem_ptr = ScreenMemPtr

        THIS.vic_bank_addr = 16384 * CWORD(VicBankPtr)
        THIS.bitmap_addr = THIS.vic_bank_addr + 8192 * BitmapPtr
        THIS.screen_mem_addr = THIS.vic_bank_addr + 1024 * ScreenMemPtr

        FOR ZP_B0 AS BYTE = 0 TO 199
            ZP_W0 = THIS.bitmap_addr + (ZP_B0 AND 7) + CWORD(320) * SHR(ZP_B0, 3)
            hires_y_tbl_lo(ZP_B0) = PEEK(@ZP_W0)
            hires_y_tbl_hi(ZP_B0) = PEEK(@ZP_W0+1)
        NEXT ZP_B0
        FOR ZP_B0 AS BYTE = 200 TO 255
            hires_y_tbl_lo(ZP_B0) = CBYTE(@TRASH)
            hires_y_tbl_hi(ZP_B0) = CBYTE(SHR(@TRASH, 8))
        NEXT ZP_B0
    END SUB

    SUB Activate() STATIC
        REM -- Vic Bank 0 to 3
        POKE $dd00, (PEEK($dd00) AND %11111100) OR (THIS.vic_bank_ptr XOR %11)

        REM -- BITMAP 0 to 1, SCRMEM 0 to 15
        POKE $d018, SHL(THIS.screen_mem_ptr, 4) OR SHL(THIS.bitmap_ptr, 3)

        REM -- Bitmap mode on
        POKE $d011, (PEEK($d011) AND %01111111) OR %00100000

        REM -- Multicolor mode
        POKE $d016, PEEK($d016) AND %11101111

        RegBorderColor = THIS.BorderColor
        RegScreenColor = THIS.ScreenColor
    END SUB

    SUB Clear(Color0 AS BYTE, Color1 AS BYTE) STATIC
        MEMSET THIS.bitmap_addr, 8000, 0
        MEMSET THIS.screen_mem_addr, 1000, SHL(Color1, 4) OR Color0
    END SUB

    SUB Import(BitmapAddr AS WORD, ScreenAddr AS WORD) STATIC
        MEMCPY BitMapAddr, THIS.bitmap_addr, 8000
        MEMCPY ScreenAddr, THIS.screen_mem_addr, 1000
    END SUB

    SUB Color(x AS BYTE, y AS BYTE, Color0 AS BYTE, Color1 AS BYTE) STATIC
        POKE THIS.screen_mem_addr + 40 * y + x, SHL(Color0, 4) OR Color1
    END SUB

    SUB Plot(x AS WORD, y AS BYTE, Color AS BYTE) STATIC
        ASM
            ldy {y}
            lda {hires_y_tbl_lo},y
            sta {ZP_W0}
            lda {hires_y_tbl_hi},y
            sta {ZP_W0}+1

            lda {x}+1
            beq msb0
            inc {ZP_W0}+1

msb0:
            lda {x}
            and #%11111000
            tay

            lda {x}
            and #%00000111
            tax

            lda {Color}
            beq unset
set:
            lda {hires_mask1},x
            ora ({ZP_W0}),y
            sta ({ZP_W0}),y
            jmp bitmap_plot_end

unset:
            lda {hires_mask0},x
            and ({ZP_W0}),y
            sta ({ZP_W0}),y
bitmap_plot_end:
        END ASM
    END SUB

    SUB Text(x AS BYTE, y AS BYTE, text AS STRING * 40, CharMemAddr AS WORD) STATIC OVERLOAD
        ZP_B0 = SHL(y, 3)
        ZP_W0 = (SHL(CWORD(hires_y_tbl_hi(ZP_B0)), 8) OR hires_y_tbl_lo(ZP_B0)) + SHL(CWORD(x), 3)
        MEMSET ZP_W0, SHL(CWORD(LEN(text)), 3), 0
        FOR ZP_B0 = 1 TO LEN(text)
            ZP_B1 = PEEK(@text + ZP_B0)
            ZP_B1 = PetsciiToScreenCode(ZP_B1)
            MEMSHIFT CharMemAddr + SHL(CWORD(ZP_B1), 3), ZP_W0, 8
            ZP_W0 = ZP_W0 + 8
        NEXT ZP_B0
    END SUB

    SUB Text(x AS BYTE, y AS BYTE, text AS STRING * 40) STATIC OVERLOAD
        rem disable interrupt and enable char rom
        ASM
            sei
        END ASM
        POKE 1, %11111010        
        CALL THIS.Text(x, y, text, $d800)
        POKE 1, %11111110
        ASM
            cli
        END ASM
    END SUB
END TYPE
