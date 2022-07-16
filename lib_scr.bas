INCLUDE "lib_memory.bas"

DECLARE SUB WaitRasterLine256() SHARED STATIC
DECLARE FUNCTION PetsciiToScreenCode AS BYTE(Petscii AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION PetsciiToScreenCode AS BYTE(Petscii AS STRING * 1) SHARED STATIC OVERLOAD
DECLARE SUB CopyCharROM(CharSet AS BYTE, DstAddr AS WORD) SHARED STATIC

DIM SHARED border_color AS BYTE @53280
DIM SHARED screen_color AS BYTE @53281


TYPE TextScreen
    BorderColor AS BYTE
    ScreenColor AS BYTE

    vic_bank_ptr AS BYTE
    vic_bank_addr AS WORD
    screen_mem_ptr AS BYTE
    screen_mem_addr AS WORD
    char_mem_ptr AS BYTE
    char_mem_addr AS WORD

    buffer_ptr AS BYTE
    buffer_addr AS WORD 

    SUB Init(VicBankPtr AS BYTE, ScreenMemPtr AS BYTE) STATIC
        THIS.vic_bank_ptr = VicBankPtr
        THIS.vic_bank_addr = 16384 * CWORD(VicBankPtr)
        THIS.screen_mem_ptr = ScreenMemPtr
        THIS.screen_mem_addr = THIS.vic_bank_addr + 1024 * CWORD(ScreenMemPtr)
    END SUB

    SUB DoubleBuffer(ScreenMemPtr) STATIC
        THIS.buffer_ptr = ScreenMemPtr
        THIS.buffer_addr = THIS.vic_bank_addr + 1024 * CWORD(ScreenMemPtr)
    END SUB

    SUB Swap() STATIC
        SWAP screen_mem_ptr, buffer_ptr
        SWAP screen_mem_addr, buffer_addr
        CALL WaitRasterLine256()
        POKE $d018, SHL(THIS.screen_mem_ptr, 4) OR %0101
        MEMCPY screen_mem_addr, buffer_addr, 1024
    END SUB

    SUB Colors(BorderColor AS BYTE, ScreenColor AS BYTE) STATIC
        THIS.BorderColor = BorderColor
        THIS.ScreenColor = ScreenColor
    END SUB

    SUB CharMem(Ptr AS BYTE) STATIC
        ' Activate charmem from "addr" relative to VIC bank
        ' Allowed values:
        '   $0000 / 0          $0800 / 2048
        '   $1000 / 4096       $1800 / 6144
        '   $2000 / 8192       $2800 / 10240
        '   $3000 / 12288      $3800 / 14336
        '   Values 4096 and 6144 in VIC bank #0 and #2 select Character ROM instead
        ' CALL ScrCharMem(14336)
        THIS.char_mem_ptr = Ptr
        THIS.char_mem_addr = THIS.vic_bank_addr + 2048 * CWORD(Ptr)
    END SUB

    SUB Activate() STATIC
        POKE $dd00, (PEEK($dd00) AND %11111100) OR (THIS.vic_bank_ptr XOR %11)
        REM -- Bitmap mode off
        POKE $d011, peek($d011) AND %11011111

        REM -- Screen address
        SCREEN THIS.screen_mem_ptr
        ' POKE $d018, SHL(THIS.screen_mem_ptr, 4) OR %0101

        REM -- Font address
        POKE $d018, (PEEK($d018) AND %11110001) OR SHL(THIS.char_mem_ptr, 1)

        border_color = THIS.BorderColor
        screen_color = THIS.ScreenColor


    END SUB

    SUB Deactivate() STATIC
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

    SUB ImportChar(ScreenCode AS BYTE, SrcAddr AS WORD) STATIC OVERLOAD
        ' Redefine character glyph
        ZP_W0 = THIS.char_mem_addr + 8 * CWORD(ScreenCode)
        ASM
            sei
            dec 1
            dec 1
        END ASM
        MEMCPY SrcAddr, ZP_W0, 8
        ASM
            inc 1
            inc 1
            cli
        END ASM
    END SUB

    SUB ImportChar(Petscii AS STRING * 1, SrcAddr AS WORD) STATIC OVERLOAD
        ' Replace character glyph with data starting at "SrcAddr"
        '   CALL ScrImportChar("A", @my_a_letter)
        '   END
        '   my_a_letter:
        '   DATA AS BYTE %00011000
        '   DATA AS BYTE %00100100
        '   DATA AS BYTE %01000010
        '   DATA AS BYTE %01000010
        '   DATA AS BYTE %01111110
        '   DATA AS BYTE %01000010
        '   DATA AS BYTE %01000010
        '   DATA AS BYTE %00011000
        CALL THIS.ImportChar(PetsciiToScreenCode(Petscii), SrcAddr)
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

SUB CopyCharROM(CharSet AS BYTE, DstAddr AS WORD) SHARED STATIC
    ASM
        sei         ; disable interrupts while we copy
        lda #$31    ; make the CPU see the Character Generator ROM...
        sta $01     ; ...at $D000 by storing %00110011 into location $01
    END ASM
    MEMCPY $D000 + 2048 * CharSet, DstAddr, 2048
    ASM
        lda #$36    ; switch in I/O mapped registers again...
        sta $01     ; ... with %00110111 so CPU can see them
        cli         ; turn off interrupt disable flag
    END ASM
END SUB

DIM PETSCII_TO_SCREENCODE(8) AS BYTE @ _PETSCII_TO_SCREENCODE
_PETSCII_TO_SCREENCODE:
DATA AS BYTE $80, $00, $c0, $e0, $40, $c0, $80, $80

FUNCTION PetsciiToScreenCode AS BYTE(Petscii AS BYTE) SHARED STATIC OVERLOAD
    IF Petscii = $ff THEN RETURN $5e
    RETURN Petscii + PETSCII_TO_SCREENCODE(SHR(Petscii, 5))
END FUNCTION

FUNCTION PetsciiToScreenCode AS BYTE(Petscii AS STRING * 1) SHARED STATIC OVERLOAD
    RETURN PetsciiToScreenCode(ASC(Petscii))
END FUNCTION

