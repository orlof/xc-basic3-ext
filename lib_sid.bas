INCLUDE "lib_hex.bas"

TYPE SidInfo
    init AS WORD
    play AS WORD
    base AS WORD
    length AS WORD

    SUB debug() STATIC
        PRINT "init", hex(THIS.init)
        PRINT "play", hex(THIS.play)
        PRINT "base", hex(THIS.base)
        PRINT "length", THIS.length
    END SUB
END TYPE

DECLARE FUNCTION sid_load AS SidInfo(sid_start AS WORD, sid_end AS WORD) SHARED STATIC
DECLARE SUB sid_play(init AS WORD, play AS WORD) SHARED STATIC

DIM SHARED sid_debug AS BYTE
sid_debug = 0

FUNCTION sid_load AS SidInfo(sid_start AS WORD, sid_end AS WORD) SHARED STATIC
    POKE @sid_load.init, PEEK(sid_start + $0b)
    POKE @sid_load.init+1, PEEK(sid_start + $0a)

    POKE @sid_load.play, PEEK(sid_start + $0d)
    POKE @sid_load.play+1, PEEK(sid_start + $0c)

    POKE @sid_load.base, PEEK(sid_start + $7c)
    POKE @sid_load.base+1, PEEK(sid_start + $7d)

    sid_load.length = sid_end - (sid_start + $7e)

    IF sid_debug THEN 
        CALL sid_load.debug()
    END IF

    MEMCPY sid_start + $7e, sid_load.base, sid_load.length
END FUNCTION

SUB sid_play(init AS WORD, play AS WORD) SHARED STATIC
    ASM
        lda {init}
        sta jsr_init + 1
        lda {init}+1
        sta jsr_init + 2

        lda {play}
        sta jsr_play + 1
        lda {play}+1
        sta jsr_play + 2

        sei 
        lda #<irq
        sta $0314
        lda #>irq
        sta $0315

        lda #$7f        ; CIA interrupt off
        sta $dc0d

        lda #$01        ; Raster interrupt on
        sta $d01a

        lda $d011
        and #%01111111  ; High bit of interrupt position = 0
        sta $d011

        lda #$00        ; Line where next IRQ happens
        sta $d012

        lda $dc0d       ; Acknowledge IRQ (to be sure)
jsr_init:
        jsr $dead       ; Initialize music
        cli

        rts

irq:
        dec $d019       ; ACK any raster IRQs
jsr_play:
        jsr $dead       ; Play the music

        jmp $ea31
    END ASM
END SUB
