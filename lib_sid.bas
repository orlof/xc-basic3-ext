'INCLUDE "lib_irq.bas"

TYPE SidInfo
    Init AS WORD
    Play AS WORD
    Base AS WORD
    Length AS WORD
    Tunes AS WORD

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

    SUB Stop() STATIC
        CALL InstallIrqRoutine(4, $ffff)

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
the_end:
        END ASM
    END SUB

    SUB Play(TuneNr AS BYTE) STATIC
        DIM InitAddr AS WORD
            InitAddr = THIS.Init
        DIM PlayAddr AS WORD
            PlayAddr = THIS.Play
        ' CALL THIS.Stop()
        ASM
            ; Set addresses
            lda {InitAddr}
            sta jsr_init+1
            lda {InitAddr}+1
            sta jsr_init+2

            lda {PlayAddr}
            sta jsr_play+1
            lda {PlayAddr}+1
            sta jsr_play+2

            lda {TuneNr}
jsr_init:
            jsr $dead       ; Initialize music

            lda #<jsr_play
            sta {ZP_W0}
            lda #>jsr_play
            sta {ZP_W0}+1

            jmp sid_irq_end

jsr_play:
            jsr $dead
            rts

sid_irq_end:
        END ASM
        CALL InstallIrqRoutine(4, ZP_W0)
    END SUB
END TYPE

