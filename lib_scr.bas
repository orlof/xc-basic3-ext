'INCLUDE "lib_memory.bas"
'INCLUDE "lib_char.bas"

DECLARE SUB WaitRasterLine256() SHARED STATIC

TYPE TextScreen
    BorderColor AS BYTE
    ScreenColor AS BYTE

    vic_bank_ptr AS BYTE
    vic_bank_addr AS WORD
    screen_mem_ptr AS BYTE
    screen_mem_addr AS WORD

    buffer_ptr AS BYTE
    buffer_addr AS WORD 

    char_mem_ptr AS BYTE
    char_mem_addr AS WORD

    SUB Init(VicBankPtr AS BYTE, ScreenMemPtr AS BYTE) STATIC
        THIS.vic_bank_ptr = VicBankPtr
        THIS.vic_bank_addr = 16384 * CWORD(VicBankPtr)
        THIS.screen_mem_ptr = ScreenMemPtr
        THIS.screen_mem_addr = THIS.vic_bank_addr + 1024 * CWORD(ScreenMemPtr)
    END SUB

    SUB DoubleBuffer(ScreenMemPtr AS BYTE) STATIC
        THIS.buffer_ptr = ScreenMemPtr
        THIS.buffer_addr = THIS.vic_bank_addr + 1024 * CWORD(ScreenMemPtr)
    END SUB

    SUB Swap() STATIC
        SWAP THIS.screen_mem_ptr, THIS.buffer_ptr
        SWAP THIS.screen_mem_addr, THIS.buffer_addr
        CALL WaitRasterLine256()
        POKE $d018, SHL(THIS.screen_mem_ptr, 4) OR %0101
        MEMCPY THIS.screen_mem_addr, THIS.buffer_addr, 1024
    END SUB

    SUB Colors(BorderColor AS BYTE, ScreenColor AS BYTE) STATIC
        THIS.BorderColor = BorderColor
        THIS.ScreenColor = ScreenColor
    END SUB

    SUB UseCharSet(CharSet AS TypeCharSet) STATIC
        ' Activate charmem from "addr" relative to VIC bank
        IF CharSet.vic_bank_ptr <> THIS.vic_bank_ptr THEN ERROR 100
        THIS.char_mem_ptr = CharSet.char_mem_ptr
        THIS.char_mem_addr = CharSet.char_mem_addr
    END SUB

    SUB Activate() STATIC
        REM Vic Bank
        POKE $dd00, (PEEK($dd00) AND %11111100) OR (THIS.vic_bank_ptr XOR %11)
        REM -- Bitmap mode off
        POKE $d011, peek($d011) AND %11011111

        REM -- Screen address
        SCREEN THIS.screen_mem_ptr
        ' POKE $d018, SHL(THIS.screen_mem_ptr, 4) OR %0101

        REM -- Font address
        POKE $d018, (PEEK($d018) AND %11110001) OR SHL(THIS.char_mem_ptr, 1)

        RegBorderColor = THIS.BorderColor
        RegScreenColor = THIS.ScreenColor
    END SUB

    SUB Fill(Char AS BYTE, Color AS BYTE) STATIC
        MEMSET THIS.screen_mem_addr, 1000, Char
        MEMSET $D800, 1000, Color
    END SUB

    SUB Clear() STATIC
        CALL THIS.Fill(32, 0)
    END SUB

    SUB Import(ScrAddr AS WORD, ColorAddr AS WORD) STATIC
        MEMCPY ScrAddr, THIS.screen_mem_addr, 1000
        MEMCPY ColorAddr, $d800, 1000
    END SUB

    SUB At(x AS BYTE, y AS BYTE, Char AS BYTE, Color AS BYTE) STATIC OVERLOAD
        ZP_W0 = 40 * CWORD(y) + x
        POKE THIS.screen_mem_addr + ZP_W0, Char
        POKE $d800 + ZP_W0, Color
    END SUB

    SUB At(x AS BYTE, y AS BYTE, Char AS BYTE) STATIC OVERLOAD
        POKE THIS.screen_mem_addr + 40 * CWORD(y) + x, Char
    END SUB

    FUNCTION At AS BYTE(x AS BYTE, y AS BYTE) STATIC OVERLOAD
        RETURN PEEK(THIS.screen_mem_addr + 40 * CWORD(y) + x)
    END FUNCTION

    FUNCTION ColorAt AS BYTE(x AS BYTE, y AS BYTE) STATIC OVERLOAD
        RETURN PEEK($d800 + 40 * CWORD(y) + x)
    END FUNCTION

    SUB ScrCentre(y AS BYTE, s AS STRING * 96) STATIC
        TEXTAT SHR(40 - len(s), 1), y, s
    END SUB
END TYPE

SUB WaitRasterLine256() SHARED STATIC
    ASM
wait1:  bit $d011
        bmi wait1
wait2:  bit $d011
        bpl wait2
    END ASM
END SUB

