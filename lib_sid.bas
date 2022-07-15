'INCLUDE "lib_hex.bas"

TYPE SidInfo
    Init AS WORD
    Play AS WORD
    Base AS WORD
    Length AS WORD
    Tunes AS WORD

    SUB Debug() STATIC
        PRINT "init", hex(THIS.Init)
        PRINT "play", hex(THIS.Play)
        PRINT "base", hex(THIS.Base)
        PRINT "length", THIS.Length
        PRINT "tunes", THIS.Tunes
    END SUB

    SUB Import(StartAddr AS WORD, EndAddr AS WORD) STATIC
        POKE @THIS.Init, PEEK(StartAddr + $0b)
        POKE @THIS.Init+1, PEEK(StartAddr + $0a)

        POKE @THIS.Play, PEEK(StartAddr + $0d)
        POKE @THIS.Play+1, PEEK(StartAddr + $0c)

        POKE @THIS.Base, PEEK(StartAddr + $7c)
        POKE @THIS.Base+1, PEEK(StartAddr + $7d)

        THIS.Length = EndAddr - (StartAddr + $7e)

        POKE @THIS.Tunes, PEEK(StartAddr + $0f)
        POKE @THIS.Tunes+1, PEEK(StartAddr + $0e)

        MEMCPY StartAddr + $7e, THIS.Base, THIS.Length
    END SUB

    SUB Play(TuneNr AS BYTE) STATIC
        DIM InitAddr AS WORD
            InitAddr = THIS.Init
        DIM PlayAddr AS WORD
            PlayAddr = THIS.Play
        ASM
            ; Reset SID
            lda #$ff
reset_sid_loop:    
            ldx #$17
reset_sid_0:
            sta $d400,x
            dex
            bpl reset_sid_0
            tax
            bpl reset_sid_1
            lda #$08
            bpl reset_sid_loop
reset_sid_1:
reset_sid_2:
            bit $d011
            bpl reset_sid_2
reset_sid_3:
            bit $d011
            bmi reset_sid_3
            eor #$08
            beq reset_sid_loop

            lda #$0f
            sta $d418

            ; Set addresses
            lda {InitAddr}
            sta jsr_init + 1
            lda {InitAddr}+1
            sta jsr_init + 2

            lda {PlayAddr}
            sta jsr_play + 1
            lda {PlayAddr}+1
            sta jsr_play + 2

            ; Set IRQ
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
            lda {TuneNr}
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

END TYPE

