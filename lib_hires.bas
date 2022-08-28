'INCLUDE "lib_memory.bas"

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
    scr_mem_ptr AS BYTE
    scr_mem_addr AS WORD
    bitmap_ptr AS BYTE
    bitmap_addr AS WORD

    REM BitMap 0-1
    REM ScrMem 0-15
    SUB Init(VicBankPtr AS BYTE, BitmapPtr AS BYTE, ScreenMemPtr AS BYTE) STATIC
        THIS.vic_bank_ptr = VicBankPtr
        THIS.bitmap_ptr = BitmapPtr
        THIS.scr_mem_ptr = ScreenMemPtr

        THIS.vic_bank_addr = 16384 * CWORD(VicBankPtr)
        THIS.bitmap_addr = THIS.vic_bank_addr + 8192 * BitmapPtr
        THIS.scr_mem_addr = THIS.vic_bank_addr + 1024 * ScreenMemPtr

        CALL InitYTables(THIS.bitmap_addr)
    END SUB

    SUB Activate() STATIC
        CALL WaitRasterLine256()

        ZP_B0 = THIS.vic_bank_ptr
        ZP_B1 = THIS.scr_mem_ptr
        ZP_B2 = THIS.bitmap_ptr
        
        ASM
            ; REM -- Vic Bank 0 to 3
            ; POKE $dd00, (PEEK($dd00) AND %11111100) OR (THIS.vic_bank_ptr XOR %11)
            lda $dd00
            and #%11111100
            ora {ZP_B0}
            eor #%00000011
            sta $dd00

            ; REM -- BITMAP 0 to 1, SCRMEM 0 to 15
            ; POKE $d018, SHL(THIS.scr_mem_ptr, 4) OR SHL(THIS.bitmap_ptr, 3)
            lda {ZP_B1}
            asl
            asl
            asl
            asl
            sta {ZP_B1}
            lda {ZP_B2}
            asl
            asl
            asl
            ora {ZP_B1}
            sta $d018

            ; REM -- Bitmap mode on
            ; POKE $d011, (PEEK($d011) AND %01111111) OR %00100000
            lda $d011
            and #%01111111
            ora #%00100000
            sta $d011

            ; REM -- Multicolor mode off
            ; POKE $d016, PEEK($d016) AND %11101111
            lda $d016
            and #%11101111
            sta $d016
        END ASM

        RegBorderColor = THIS.BorderColor
        RegScreenColor = THIS.ScreenColor
    END SUB

    SUB Clear(Color0 AS BYTE, Color1 AS BYTE) STATIC
        MEMSET THIS.bitmap_addr, 8000, 0
        ASM
            lda {Color1}
            asl
            asl
            asl
            asl
            ora {Color0}
            sta {Color0}
        END ASM
        MEMSET THIS.scr_mem_addr, 1000, Color0
    END SUB

    SUB Import(BitmapAddr AS WORD, ScreenAddr AS WORD) STATIC
        MEMCPY BitMapAddr, THIS.bitmap_addr, 8000
        MEMCPY ScreenAddr, THIS.scr_mem_addr, 1000
    END SUB

    SUB ColorBlock(x AS BYTE, y AS BYTE, Color0 AS BYTE, Color1 AS BYTE) STATIC
        POKE THIS.scr_mem_addr + 40 * y + x, SHL(Color0, 4) OR Color1
    END SUB

    SUB Area(x0 AS BYTE, y0 AS BYTE, x1 AS BYTE, y1 AS BYTE, Pattern AS BYTE) STATIC
        ASM
            ; ZP_W1 = THIS.bitmap_addr + SHL(CWORD(x0), 3) + 320 * CWORD(y0)

            ; POKE @ZP_W1, bitmap_y_tbl_lo(y0)
            ; POKE @ZP_W1+1, bitmap_y_tbl_hi(y0)
            lda {y0}
            asl
            asl
            asl
            tay
            lda {bitmap_y_tbl_lo},y
            sta {ZP_W1}
            lda {bitmap_y_tbl_hi},y
            sta {ZP_W1}+1

            ; ZP_W1 = ZP_W1 + SHL(CWORD(x0), 3)
            lda {x0}
            asl
            asl
            asl
            tax
            lda #0
            rol
            tay
            
            clc
            txa
            adc {ZP_W1}
            sta {ZP_W1}
            tya
            adc {ZP_W1}+1
            sta {ZP_W1}+1
        END ASM
        'POKE @ZP_W1, bitmap_y_tbl_lo(y0)
        'POKE @ZP_W1+1, bitmap_y_tbl_hi(y0)
        'ZP_W1 = ZP_W1 + SHL(CWORD(x0), 3)

        'ZP_W1 = THIS.bitmap_addr + SHL(CWORD(x0), 3) + 320 * CWORD(y0)

        FOR ZP_W0 = ZP_W1 TO ZP_W1 + 320 * CWORD(y1 - y0) STEP 320
            FOR ZP_W1 = ZP_W0 TO ZP_W0 + SHL(CWORD(x1 - x0), 3) + 7
                IF Pattern < 2 THEN
                    POKE ZP_W1, Pattern
                ELSE
                    POKE ZP_W1, PEEK(ZP_W1)
                END IF
            NEXT
        NEXT
    END SUB

    SUB ColorArea(x0 AS BYTE, y0 AS BYTE, x1 AS BYTE, y1 AS BYTE, Colors AS BYTE) STATIC
        ZP_W1 = THIS.scr_mem_addr + 40 * CWORD(y0) + x0
        FOR ZP_W0 = ZP_W1 TO ZP_W1 + 40 * CWORD(y1 - y0) STEP 40
            FOR ZP_W1 = ZP_W0 TO ZP_W0 + x1 - x0
                POKE ZP_W1, Colors
            NEXT
        NEXT
    END SUB

    SUB Plot(x AS WORD, y AS BYTE, Mode AS BYTE) STATIC
        ZP_B0 = (THIS.vic_bank_ptr = 3)
        ASM
            ldy {y}
            lda {bitmap_y_tbl_lo},y
            sta {ZP_W0}
            lda {bitmap_y_tbl_hi},y
            sta {ZP_W0}+1

            lda {x}+1
            beq hires_plot_msb0
            inc {ZP_W0}+1

hires_plot_msb0
            lda {x}
            and #%11111000
            tay

            lda {x}
            and #%00000111
            tax

            lda {ZP_B0}
            beq hires_plot_memory_readable

            sei
            dec 1
            dec 1

hires_plot_memory_readable
            lda {Mode}
            beq hires_plot_unset
hires_plot_set
            lda {hires_mask1},x
            ora ({ZP_W0}),y
            sta ({ZP_W0}),y
            jmp hires_plot_end

hires_plot_unset
            lda {hires_mask0},x
            and ({ZP_W0}),y
            sta ({ZP_W0}),y

hires_plot_end
            lda {ZP_B0}
            beq hires_plot_memory_restored

            inc 1
            inc 1
            cli
hires_plot_memory_restored
        END ASM
    END SUB

    SUB Rect(x0 AS WORD, y0 AS BYTE, x1 AS WORD, y1 AS BYTE, Mode AS BYTE) STATIC
        FOR ZP_W1 = x0 TO x1
            CALL THIS.Plot(ZP_W1, y0, Mode)
            CALL THIS.Plot(ZP_W1, y1, Mode)
        NEXT
        FOR ZP_B1 = y0 TO y1
            CALL THIS.Plot(x0, ZP_B1, Mode)
            CALL THIS.Plot(x1, ZP_B1, Mode)
        NEXT
    END SUB

    SUB Text(x AS BYTE, y AS BYTE, text AS STRING * 40, CharMemAddr AS WORD) STATIC OVERLOAD
        ZP_B0 = SHL(y, 3)
        ZP_W0 = (SHL(CWORD(bitmap_y_tbl_hi(ZP_B0)), 8) OR bitmap_y_tbl_lo(ZP_B0)) + SHL(CWORD(x), 3)
        'MEMSET ZP_W0, SHL(CWORD(LEN(text)), 3), 0
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
            lda 1
            and #%11111011
            sta 1
        END ASM
        CALL THIS.Text(x, y, text, $d800)
        ASM
            lda 1
            ora #%00000100
            sta 1
            cli
        END ASM
    END SUB

    SUB Text(x AS BYTE, y AS BYTE, Text AS STRING * 40, Colors AS BYTE) STATIC OVERLOAD
        CALL THIS.Text(x, y, Text)
        FOR ZP_B0 = x TO x + LEN(Text) - 1
            POKE THIS.scr_mem_addr + 40 * CWORD(y) + ZP_B0, Colors
        NEXT
    END SUB

END TYPE
