'INCLUDE "lib_memory.bas"
'INCLUDE "lib_char.bas"

DIM mc_mask0(4) AS BYTE @_mc_mask0
DIM mc_mask1(4) AS BYTE @_mc_mask1
DIM mc_mask2(4) AS BYTE @_mc_mask2
DIM mc_mask3(4) AS BYTE @_mc_mask3

DIM nible_to_byte(16) AS BYTE @_nible_to_byte
DIM color_pattern(4) AS BYTE @_color_pattern

TYPE ScreenMultiColor
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

        ASM
            lda #0
            sta {ZP_W0}
            sta {ZP_W1}
            sta {ZP_W2}

            lda {VicBankPtr}        ;16384 * CWORD(VicBankPtr)
            lsr
            ror
            ror
            sta {ZP_W0}+1

            lda {BitmapPtr}         ;vic_bank_addr + 8192 * BitmapPtr
            lsr
            ror
            ror
            ror

            clc
            adc {ZP_W0}+1
            sta {ZP_W1}+1

            lda {ScreenMemPtr}      ;vic_bank_addr + 1024 * ScreenMemPtr
            asl
            asl

            clc
            adc {ZP_W0}+1
            sta {ZP_W2}+1
        END ASM

        THIS.vic_bank_addr = ZP_W0
        THIS.bitmap_addr = ZP_W1
        THIS.scr_mem_addr = ZP_W2

        CALL InitYTables(ZP_W1)
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

            ; REM -- Multicolor mode on
            ; POKE $d016, PEEK($d016) AND %11101111
            lda $d016
            ora #%00010000
            sta $d016
        END ASM

        RegBorderColor = THIS.BorderColor
        RegScreenColor = THIS.ScreenColor
    END SUB

    SUB Clear(Color1 AS BYTE, Color2 AS BYTE, Color3 AS BYTE) STATIC
        MEMSET THIS.bitmap_addr, 8000, 0
        MEMSET THIS.scr_mem_addr, 1000, SHL(Color1, 4) OR Color2
        MEMSET $d800, 1000, Color3
    END SUB

    SUB Import(BitmapAddr AS WORD, ScreenMemAddr AS WORD, ColorMemAddr AS WORD) STATIC
        MEMCPY BitMapAddr, THIS.bitmap_addr, 8000
        MEMCPY ScreenMemAddr, THIS.scr_mem_addr, 1000
        MEMCPY ColorMemAddr, $d800, 1000
    END SUB

    SUB ColorBlock(x AS BYTE, y AS BYTE, Color1 AS BYTE, Color2 AS BYTE, Color3 AS BYTE) STATIC
        POKE THIS.scr_mem_addr + 40 * y + x, SHL(Color1, 4) OR Color2
        POKE $d800 + 40 * y + x, Color3
    END SUB

    SUB Plot(x AS WORD, y AS BYTE, Ink AS BYTE) STATIC
        ASM
            ldy {y}
            lda {bitmap_y_tbl_lo},y
            sta {ZP_W0}
            lda {bitmap_y_tbl_hi},y
            sta {ZP_W0}+1

            lda {x}
            and #%11111100
            asl
            bcc msb0
            inc {ZP_W0}+1
msb0:
            tay
            lda {x}
            and #%00000011
            tax

            lda {Ink}
            beq color0
            cmp #3
            beq color3
            cmp #2
            beq color2
color1:            
            lda ({ZP_W0}),y
            and {mc_mask0},x
            ora {mc_mask1},x
            jmp mc_plot_end
color2:            
            lda ({ZP_W0}),y
            and {mc_mask0},x
            ora {mc_mask2},x
            jmp mc_plot_end
color3:            
            lda ({ZP_W0}),y
            and {mc_mask0},x
            ora {mc_mask3},x
            jmp mc_plot_end
color0:            
            lda ({ZP_W0}),y
            and {mc_mask0},x
mc_plot_end:
            sta ({ZP_W0}),y
        END ASM
    END SUB

    SUB Text(Col AS BYTE, Row AS BYTE, text AS STRING * 40, InkColor AS BYTE, BgColor AS BYTE, CharMemAddr AS WORD) STATIC OVERLOAD
        ZP_B0 = SHL(InkColor, 4) OR BgColor
        ZP_W0 = THIS.scr_mem_addr + 40 * CWORD(Row) + Col
        FOR ZP_W0 = ZP_W0 TO ZP_W0 + 2 * LEN(text) - 1
            POKE ZP_W0, ZP_B0
        NEXT ZP_W0

        ZP_B2 = color_pattern(1)
        ZP_B3 = color_pattern(2)

        ASM
            ;W0 = ytbl(8 * Row)
            lda {Row}
            asl
            asl
            asl
            tay
            lda {bitmap_y_tbl_lo},y
            sta {ZP_W0}
            lda {bitmap_y_tbl_hi},y
            sta {ZP_W0}+1

            lda {Col}
            asl
            asl
            asl
            sta {ZP_W1}
            lda #0
            bcc mc_text_1
            lda #1
            clc
mc_text_1
            sta {ZP_W1}+1

            lda {ZP_W0}
            adc {ZP_W1}
            sta {ZP_W0}

            lda {ZP_W0}+1
            adc {ZP_W1}+1
            sta {ZP_W0}+1
            ;ZP_W0 = y_tbl(8 * Row) + 2 * CWORD((4 * Col) AND %11111100)
        END ASM

        FOR ZP_B0 = 1 TO LEN(text)
            ZP_B1 = PEEK(@text + ZP_B0)
            ZP_B1 = PetsciiToScreenCode(ZP_B1)
            ZP_W1 = CharMemAddr + 8 * CWORD(ZP_B1)

            FOR ZP_W0 = ZP_W0 TO ZP_W0 + 7
                ZP_B1 = nible_to_byte(SHR(PEEK(ZP_W1), 4))
                POKE ZP_W0, ((NOT ZP_B1) AND ZP_B3) OR (ZP_B1 AND ZP_B2) 

                ZP_B1 = nible_to_byte(PEEK(ZP_W1) AND %00001111)
                POKE ZP_W0 + 8, ((NOT ZP_B1) AND ZP_B3) OR (ZP_B1 AND ZP_B2)
                ZP_W1 = ZP_W1 + 1
            NEXT ZP_W0
            ZP_W0 = ZP_W0 + 8
        NEXT ZP_B0
    END SUB

    SUB Text(Col AS BYTE, Row AS BYTE, Text AS STRING * 40, InkColor AS BYTE, BgColor AS BYTE, CharSetPtr AS BYTE) STATIC OVERLOAD
        rem disable interrupt and enable char rom
        ASM
            sei
            lda 1
            and #%11111011
            sta 1
        END ASM
        CALL THIS.Text(Col, Row, Text, InkColor, BgColor, $d000 + CWORD(2048) * CWORD(CharSetPtr))
        ASM
            lda 1
            ora #%00000100
            sta 1
            cli
        END ASM
    END SUB

    SUB Centre(Row AS BYTE, Text AS STRING * 40, InkColor AS BYTE, BgColor AS BYTE, CharMemAddr AS WORD) STATIC OVERLOAD
        CALL THIS.Text(20 - LEN(text), Row, Text, InkColor, BgColor, CharMemAddr)
    END SUB

    SUB Centre(Row AS BYTE, Text AS STRING * 40, InkColor AS BYTE, BgColor AS BYTE, CharSetPtr AS BYTE) STATIC OVERLOAD
        CALL THIS.Text(20 - LEN(text), Row, Text, InkColor, BgColor, CharSetPtr)
    END SUB

END TYPE

GOTO THE_END

_mc_mask0:
DATA AS BYTE $3f,$cf,$f3,$fc
_mc_mask1:
DATA AS BYTE $40,$10,$04,$01
_mc_mask2:
DATA AS BYTE $80,$20,$08,$02
_mc_mask3:
DATA AS BYTE $c0,$30,$0c,$03
_nible_to_byte:
DATA AS BYTE %00000000, %00000011, %00001100, %00001111
DATA AS BYTE %00110000, %00110011, %00111100, %00111111
DATA AS BYTE %11000000, %11000011, %11001100, %11001111
DATA AS BYTE %11110000, %11110011, %11111100, %11111111
_color_pattern:
DATA AS BYTE %00000000, %01010101, %10101010, %11111111

THE_END:
