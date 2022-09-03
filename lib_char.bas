'INCLUDE "lib_memory.bas"

DECLARE FUNCTION PetsciiToScreenCode AS BYTE(Petscii AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION PetsciiToScreenCode AS BYTE(Petscii AS STRING * 1) SHARED STATIC OVERLOAD

TYPE TypeCharSet
    vic_bank_ptr AS BYTE
    vic_bank_addr AS WORD

    char_mem_ptr AS BYTE
    char_mem_addr AS WORD

    SUB Init(VicBankPtr AS BYTE, CharSetPtr AS BYTE) STATIC
        ' Allowed values:
        '   $0000 / 0          $0800 / 2048
        '   $1000 / 4096       $1800 / 6144
        '   $2000 / 8192       $2800 / 10240
        '   $3000 / 12288      $3800 / 14336
        '   Values 4096 and 6144 in VIC bank #0 and #2 select Character ROM instead
        ' CALL ScrCharMem(14336)
        THIS.vic_bank_ptr = VicBankPtr
        THIS.vic_bank_addr = 16384 * CWORD(VicBankPtr)
        THIS.char_mem_ptr = CharSetPtr
        THIS.char_mem_addr = THIS.vic_bank_addr + 2048 * CWORD(CharSetPtr)
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

    SUB CopyCharROM(CharSetPtr AS BYTE) STATIC
        ASM
            sei         ; disable interrupts while we copy
            lda #$31    ; make the CPU see the Character Generator ROM...
            sta $01     ; ...at $D000 by storing %00110011 into location $01
        END ASM
        MEMCPY $D000 + 2048 * CWORD(CharSetPtr), THIS.char_mem_addr, 2048
        ASM
            lda #$36    ; switch in I/O mapped registers again...
            sta $01     ; ... with %00110111 so CPU can see them
            cli         ; turn off interrupt disable flag
        END ASM
    END SUB
END TYPE

DIM PETSCII_TO_SCREENCODE(8) AS BYTE @ _PETSCII_TO_SCREENCODE

FUNCTION PetsciiToScreenCode AS BYTE(Petscii AS BYTE) SHARED STATIC OVERLOAD
    IF Petscii = $ff THEN RETURN $5e
    RETURN Petscii + PETSCII_TO_SCREENCODE(SHR(Petscii, 5))
END FUNCTION

FUNCTION PetsciiToScreenCode AS BYTE(Petscii AS STRING * 1) SHARED STATIC OVERLOAD
    RETURN PetsciiToScreenCode(ASC(Petscii))
END FUNCTION

DIM StringBuilder AS STRING * 24 SHARED
SUB StringBuilder_Clear(Size AS BYTE) SHARED STATIC
    POKE @StringBuilder, Size
    MEMSET @StringBuilder + 1, Size, 32
END SUB

SUB StringBuilder_Left(Pos AS BYTE, Text AS STRING * 24) SHARED STATIC
    MEMCPY @Text + 1, @StringBuilder + Pos + 1, PEEK(@Text)
END SUB

SUB StringBuilder_Right(Pos AS BYTE, Text AS STRING * 24) SHARED STATIC OVERLOAD
    CALL StringBuilder_Left(Pos + 1 - LEN(Text), Text)
END SUB

SUB StringBuilder_Right(Pos AS BYTE, Text AS WORD) SHARED STATIC OVERLOAD
    CALL StringBuilder_Right(Pos, STR$(Text))
END SUB

GOTO THE_END

_PETSCII_TO_SCREENCODE:
DATA AS BYTE $80, $00, $c0, $e0, $40, $c0, $80, $80

THE_END:
