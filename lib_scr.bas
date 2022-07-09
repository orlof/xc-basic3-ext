DECLARE SUB scr_centre(s AS STRING * 96) SHARED STATIC OVERLOAD
DECLARE SUB scr_centre(y AS BYTE, s AS STRING * 96) SHARED STATIC OVERLOAD
DECLARE SUB scr_clear() SHARED STATIC
DECLARE FUNCTION scr_charat AS BYTE(x AS BYTE, y AS BYTE) SHARED STATIC
DECLARE FUNCTION scr_screencode AS BYTE(petscii AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION scr_screencode AS BYTE(petscii AS STRING * 1) SHARED STATIC OVERLOAD
DECLARE SUB scr_charrom(set AS WORD, addr AS WORD) SHARED STATIC
DECLARE SUB scr_charmem(addr AS WORD) SHARED STATIC
DECLARE SUB scr_set_glyph(screencode AS BYTE, from AS WORD) SHARED STATIC OVERLOAD
DECLARE SUB scr_set_glyph(petscii AS STRING * 1, from AS WORD) SHARED STATIC OVERLOAD

SHARED CONST CHARSET_GRAPHICS = 0
SHARED CONST CHARSET_LOWERCASE = 2048

DIM SHARED leading_space AS STRING * 20
leading_space = "                    "

DIM SHARED BorderColor AS BYTE @53280
DIM SHARED ScreenColor AS BYTE @53281

SUB scr_wait_bottom() SHARED STATIC
    ASM
wait1:  bit $d011
        bmi wait1
wait2:  bit $d011
        bpl wait2
    END ASM
END SUB

SUB scr_centre(s AS STRING * 96) SHARED STATIC
    IF len(s) > 38 THEN
        POKE @leading_space, 0
    ELSE
        POKE @leading_space, SHR(40 - len(s), 1)
    END IF
    PRINT leading_space; s
END SUB

SUB scr_centre(y AS BYTE, s AS STRING * 96) SHARED STATIC OVERLOAD
    LOCATE 0, y
    CALL scr_centre(s)
END SUB

SUB scr_clear() SHARED STATIC
    SYS $E544 FAST
END SUB

FUNCTION scr_charat AS BYTE(x AS BYTE, y AS BYTE) SHARED STATIC
    RETURN PEEK(1024 + 40 * CWORD(y) + x)
END FUNCTION

SUB scr_charrom(set AS WORD, addr AS WORD) SHARED STATIC
    ASM
        sei         ; disable interrupts while we copy
        lda #$31    ; make the CPU see the Character Generator ROM...
        sta $01     ; ...at $D000 by storing %00110011 into location $01
    END ASM
    MEMCPY $D000 + set, addr, 2048
    ASM
        lda #$36    ; switch in I/O mapped registers again...
        sta $01     ; ... with %00110111 so CPU can see them
        cli         ; turn off interrupt disable flag
    END ASM
END SUB

DIM PETSCII_TO_SCREENCODE(8) AS BYTE @ _PETSCII_TO_SCREENCODE
_PETSCII_TO_SCREENCODE:
DATA AS BYTE $80, $00, $c0, $e0, $40, $c0, $80, $80

FUNCTION scr_screencode AS BYTE(petscii AS BYTE) SHARED STATIC OVERLOAD
    IF petscii = $ff THEN
        RETURN $5e
    END IF
    RETURN petscii + PETSCII_TO_SCREENCODE(SHR(petscii, 5))
END FUNCTION

FUNCTION scr_screencode AS BYTE(petscii AS STRING * 1) SHARED STATIC OVERLOAD
    RETURN scr_screencode(ASC(petscii))
END FUNCTION

SUB scr_charmem(addr AS WORD) SHARED STATIC
    ' Activate charmem from "addr" relative to VIC bank
    ' Allowed values:
    '   $0000 / 0          $0800 / 2048
    '   $1000 / 4096       $1800 / 6144
    '   $2000 / 8192       $2800 / 10240
    '   $3000 / 12288      $3800 / 14336
    '   Values 4096 and 6144 in VIC bank #0 and #2 select Character ROM instead
    ' CALL scr_charmem(14336)
    DIM slot AS BYTE: slot = CBYTE(SHR(addr, 10)) AND %1110
    IF SHL(CWORD(slot), 10) <> addr THEN ERROR 100

    POKE $d018, (PEEK($d018) AND %11110000) OR slot
END SUB

SUB scr_set_glyph(screencode AS BYTE, from AS WORD) SHARED STATIC OVERLOAD
    ' Redefine character glyph
    DIM addr AS WORD: addr = 16384 * ((PEEK($dd00) AND %11) XOR %11)
    addr = addr + 1024 * (PEEK($d018) AND %1110)
    addr = addr + 8 * CWORD(screencode)
    ASM
        sei
        dec 1
        dec 1
    END ASM
    MEMCPY from, addr, 8
    ASM
        inc 1
        inc 1
        cli
    END ASM
END SUB

SUB scr_set_glyph(petscii AS STRING * 1, from AS WORD) SHARED STATIC OVERLOAD
    ' Replace character glyph with data starting at "from"
    '   CALL scr_set_glyph("A", @my_a_letter)
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
    CALL scr_set_glyph(scr_screencode(petscii), from)
END SUB

