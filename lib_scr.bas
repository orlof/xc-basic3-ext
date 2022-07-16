'INCLUDE "lib_memory.bas"

DECLARE SUB ScrScreenMem(ScrMemPtr AS BYTE) SHARED STATIC
DECLARE SUB ScrTextMode() SHARED STATIC
DECLARE SUB ScrWaitRaster256() SHARED STATIC
DECLARE SUB ScrCentre(y AS BYTE, s AS STRING * 96) SHARED STATIC
DECLARE SUB ScrClear() SHARED STATIC
DECLARE FUNCTION ScrCharAt AS BYTE(x AS BYTE, y AS BYTE) SHARED STATIC
DECLARE FUNCTION ScrPetsciiToScreenCode AS BYTE(Petscii AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION ScrPetsciiToScreenCode AS BYTE(Petscii AS STRING * 1) SHARED STATIC OVERLOAD
DECLARE SUB ScrCopyCharROM(CharSet AS BYTE, DstAddr AS WORD) SHARED STATIC
DECLARE SUB ScrCharMem(Addr AS WORD) SHARED STATIC
DECLARE SUB ScrImportChar(ScreenCode AS BYTE, SrcAddr AS WORD) SHARED STATIC OVERLOAD
DECLARE SUB ScrImportChar(Petscii AS STRING * 1, SrcAddr AS WORD) SHARED STATIC OVERLOAD

SHARED CONST DEFAULT_SCR_MEM_PTR = 1
SHARED CONST CHARSET_GRAPHICS = 0
SHARED CONST CHARSET_LOWERCASE = 2048

DIM SHARED BorderColor AS BYTE @53280
DIM SHARED ScreenColor AS BYTE @53281

DIM SHARED ScrMemAddr AS WORD
ScrMemAddr = 1024

DIM screen_mem_ptr AS BYTE
    screen_mem_ptr = 1

SUB ScrScreenMem(ScrMemPtr AS BYTE) SHARED STATIC
    screen_mem_ptr = ScrMemPtr
    poke $d018, SHL(ScrMemPtr, 4) OR %0101
    ScrMemAddr = vic_bank_addr + 1024 * CWORD(ScrMemPtr)
END SUB

SUB ScrTextMode() SHARED STATIC
    rem -- Bitmap mode off
    poke $d011, peek($d011) AND %11011111
    rem -- Restore screen address to default
    poke $d018, SHL(screen_mem_ptr, 4) OR %0101
END SUB

SUB ScrWaitRaster256() SHARED STATIC
    ASM
wait1:  bit $d011
        bmi wait1
wait2:  bit $d011
        bpl wait2
    END ASM
END SUB

SUB ScrCentre(y AS BYTE, s AS STRING * 96) SHARED STATIC
    TEXTAT SHR(40 - len(s), 1), y, s
END SUB

SUB ScrClear() SHARED STATIC
    SYS $E544 FAST
END SUB

FUNCTION ScrCharAt AS BYTE(x AS BYTE, y AS BYTE) SHARED STATIC
    RETURN PEEK(ScrMemAddr + SHL(CWORD(y), 5) + SHL(CWORD(y), 3) + x)
END FUNCTION

SUB ScrCopyCharROM(CharSet AS BYTE, DstAddr AS WORD) SHARED STATIC
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

FUNCTION ScrPetsciiToScreenCode AS BYTE(Petscii AS BYTE) SHARED STATIC OVERLOAD
    IF Petscii = $ff THEN RETURN $5e
    RETURN Petscii + PETSCII_TO_SCREENCODE(SHR(Petscii, 5))
END FUNCTION

FUNCTION ScrPetsciiToScreenCode AS BYTE(Petscii AS STRING * 1) SHARED STATIC OVERLOAD
    RETURN ScrPetsciiToScreenCode(ASC(Petscii))
END FUNCTION

SUB ScrCharMem(Addr AS WORD) SHARED STATIC
    ' Activate charmem from "addr" relative to VIC bank
    ' Allowed values:
    '   $0000 / 0          $0800 / 2048
    '   $1000 / 4096       $1800 / 6144
    '   $2000 / 8192       $2800 / 10240
    '   $3000 / 12288      $3800 / 14336
    '   Values 4096 and 6144 in VIC bank #0 and #2 select Character ROM instead
    ' CALL ScrCharMem(14336)
    DIM slot AS BYTE: slot = CBYTE(SHR(Addr, 10)) AND %1110
    POKE $d018, (PEEK($d018) AND %11110001) OR slot
END SUB

SUB ScrImportChar(ScreenCode AS BYTE, SrcAddr AS WORD) SHARED STATIC OVERLOAD
    ' Redefine character glyph
    DIM addr AS WORD: addr = 16384 * ((PEEK($dd00) AND %11) XOR %11)
    addr = addr + 1024 * (PEEK($d018) AND %1110)
    addr = addr + 8 * CWORD(ScreenCode)
    ASM
        sei
        dec 1
        dec 1
    END ASM
    MEMCPY SrcAddr, addr, 8
    ASM
        inc 1
        inc 1
        cli
    END ASM
END SUB

SUB ScrImportChar(Petscii AS STRING * 1, SrcAddr AS WORD) SHARED STATIC OVERLOAD
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
    CALL ScrImportChar(ScrPetsciiToScreenCode(Petscii), SrcAddr)
END SUB
