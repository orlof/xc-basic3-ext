INCLUDE "lib_memory.bas"
INCLUDE "lib_types.bas"
INCLUDE "lib_irq.bas"
INCLUDE "lib_joy.bas"

SHARED CONST NOISE = 128
SHARED CONST PULSE = 64
SHARED CONST SAWTOOTH = 32
SHARED CONST TRIANGLE = 16
SHARED CONST NO_BOUNCE = $00
SHARED CONST INFINITE = $ff

DIM SfxRequest AS Word
    SfxRequest = 0

DIM ptr AS WORD FAST

TYPE SFX
    Priority AS BYTE
    Time AS BYTE
    WaveForm AS BYTE
    AttackDecay AS BYTE
    SustainRelease AS BYTE
    Frequency AS WORD
    Slide AS LONG
    Bounce AS BYTE
    Pulse AS WORD

    SUB Play() STATIC
        SfxRequest = @THIS
    END SUB

    SUB Stop() STATIC
        SfxRequest = $ffff
    END SUB
END TYPE

SUB InstallToIrq() SHARED STATIC
    ASM
sid1 = $d400
initsfx
        lda #$00
        ldx #$17
initsfx_reset_loop
        sta sid1,x
        dex
        bpl initsfx_reset_loop

        lda #$0f
        sta $d418

        lda #<sfx_play
        sta {ZP_W0}
        lda #>sfx_play
        sta {ZP_W0}+1

        jmp initsfx_end

sfx_priority    = 0
sfx_time        = 1
sfx_waveform    = 2
sfx_atdc        = 3
sfx_ssrl        = 4
sfx_frequency   = 5
sfx_slide       = 7
sfx_bounce      = 10
sfx_pulse       = 11

sfx_play
        lda {SfxRequest}            ;Jump here from interrupt
        bne sfx_new_or_stop
        lda {SfxRequest}+1
        bne sfx_new_or_stop

        lda sid1_time
        beq sfx_return
        
        cmp #$ff
        beq sfx_continue
        dec sid1_time        ;sound still playing
        bne sfx_continue

sfx_end
        lda #$00
        sta sid1_time
        sta sid1+4        ;sound over
sfx_return
        rts

sfx_continue
        lda sid1_slide0
        bne sfx_slide_effect       ;slide on sound?
        lda sid1_slide1
        bne sfx_slide_effect       ;slide on sound?
        rts

sfx_slide_effect
        clc
        lda sid1_freqlo        ;get voice freq lo byte and add
        adc sid1_slide0
        sta sid1
        sta sid1_freqlo
        
        lda sid1_freqhi        ;get voice freq hi byte and add
        adc sid1_slide1  
        sta sid1+1
        sta sid1_freqhi

        ldx sid1_bouncetime
        bne sfx_bounceslide
        rts

sfx_bounceslide
        dex
        beq sfx_bounce_switch
        stx sid1_bouncetime
        rts

sfx_bounce_switch
        lda #$ff
        eor sid1_slide0
        sta sid1_slide0
        lda #$ff
        eor sid1_slide1
        sta sid1_slide1
        lda #$ff
        eor sid1_slide2
        sta sid1_slide2

        inc sid1_slide0
        bne sfx_bounce_switch_done
        inc sid1_slide1
        bne sfx_bounce_switch_done
        inc sid1_slide2

sfx_bounce_switch_done
        lda sid1_bouncemax
        sta sid1_bouncetime  ;reset timer
        rts

sfx_new_or_stop
        lda {SfxRequest}            ; init ptr to sfx struct
        cmp #$ff
        bne sfx_new
        lda {SfxRequest}+1
        cmp #$ff
        beq sfx_end

        lda {SfxRequest}
sfx_new
        sta {ptr}
        lda {SfxRequest}+1
        sta {ptr}+1

        ldy #sfx_priority
        lda ({ptr}),y
        sta sid1_priority

        ldy #sfx_time
        lda ({ptr}),y
        sta sid1_time

        ldy #sfx_bounce
        lda ({ptr}),y
        sta sid1_bouncetime
        sta sid1_bouncemax

        ldy #sfx_slide
        lda ({ptr}),y
        sta sid1_slide0
        iny
        lda ({ptr}),y
        sta sid1_slide1
        iny
        lda ({ptr}),y
        sta sid1_slide2
        
        ldy #sfx_frequency
        lda ({ptr}),y
        sta sid1_freqlo
        sta sid1
        iny
        lda ({ptr}),y
        sta sid1_freqhi
        sta sid1+1

        ldy #sfx_pulse
        lda ({ptr}),y
        sta sid1+2
        iny
        lda ({ptr}),y
        sta sid1+3

        ldy #sfx_atdc
        lda ({ptr}),y
        sta sid1+5

        ldy #sfx_ssrl
        lda ({ptr}),y
        sta sid1+6

        ldy #sfx_waveform
        lda ({ptr}),y
        ora #1
        sta sid1+4

        lda #$00
        sta {SfxRequest}
        sta {SfxRequest}+1
        rts

sid1_priority
        .byte 0

sid1_time
        .byte 0 ;decrement

sid1_bouncetime
        .byte 0 ;time until slide reversed
sid1_bouncemax
        .byte 0 ;holds the reset value for bouncetime

sid1_slide0
        .byte 0
sid1_slide1
        .byte 0
sid1_slide2
        .byte 0

sid1_freqlo
        .byte 0
sid1_freqhi
        .byte 0

initsfx_end
    END ASM
    CALL InstallIrqRoutine(4, ZP_W0)
END SUB

DIM StartSfx AS SFX
    StartSfx.Priority = 0
    StartSfx.Time = 70
    StartSfx.WaveForm = TRIANGLE
    StartSfx.AttackDecay = $71
    StartSfx.SustainRelease = $a9
    StartSfx.Frequency = $0764
    StartSfx.Slide = 63
    StartSfx.Bounce = 0
    StartSfx.Pulse = 0

CALL InstallToIrq()

DO WHILE TRUE
    CALL Joy1.WaitClick()
    PRINT "play"
    CALL StartSfx.Play()
    CALL Joy1.WaitClick()
    PRINT "stop"
    CALL StartSfx.Stop()
LOOP

END

ASM
;-------------------------------------------
;SFX DEFINITIONS
;Priority for least to greatest
; 0 Walking
; 1 Sword
; 2 Monster Explode
; 3 Player Hurt
; 4 Collect Key
; 5 Collect Treasure
; 6 Boss Attack
; 7 Boss Hurt
; 8 Boss Explode
; 9 Chalice
; 10 Game Start
; 11 Player Dies
;-------------------------------------------

END ASM

