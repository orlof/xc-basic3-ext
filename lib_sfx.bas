'INCLUDE "lib_memory.bas"
'INCLUDE "lib_types.bas"
'INCLUDE "lib_irq.bas"

SHARED CONST NOISE = 128
SHARED CONST PULSE = 64
SHARED CONST SAWTOOTH = 32
SHARED CONST TRIANGLE = 16

DIM sfx_request(3) AS BYTE
DIM sfx_request_hi(3) AS BYTE
DIM sfx_request_lo(3) AS BYTE
    FOR ZP_B0 = 0 TO 2
        sfx_request(ZP_B0) = FALSE
        sfx_request_hi(ZP_B0) = 0
        sfx_request_lo(ZP_B0) = 0
    NEXT

DIM ptr AS WORD FAST

TYPE SFX
    Duration AS BYTE
    Waveform AS BYTE
    AttackDecay AS BYTE
    SustainRelease AS BYTE
    Frequency AS WORD
    FrequencySlide AS LONG
    Bounce AS BYTE
    Pulse AS WORD
END TYPE

SUB SfxPlay(VoiceNr AS BYTE, Effect AS WORD) SHARED STATIC
    sfx_request_lo(VoiceNr) = PEEK(@Effect)
    sfx_request_hi(VoiceNr) = PEEK(@Effect + 1)
    sfx_request(VoiceNr) = TRUE
END SUB

SUB SfxStop(VoiceNr AS BYTE) SHARED STATIC
    sfx_request_lo(VoiceNr) = $ff
    sfx_request_hi(VoiceNr) = $ff
    sfx_request(VoiceNr) = TRUE
    ASM
        ldx {VoiceNr}
sfx_stop_loop
        lda {sfx_request},x
        bne sfx_stop_loop
    END ASM
END SUB

SUB SfxUninstall() SHARED STATIC
    CALL InstallIrqRoutine(4, $ffff)
    ASM
sid = $d400
sfx_reset
        lda #$00
        ldx #$18
sfx_reset_loop
        sta sid,x
        dex
        bpl sfx_reset_loop
    END ASM
END SUB

SUB SfxInstall() SHARED STATIC
    ASM
sid = $d400
initsfx
        lda #$00
        ldx #$17
initsfx_reset_loop
        sta sid,x
        dex
        bpl initsfx_reset_loop

        lda #$0f
        sta $d418

        lda #<sfx_play
        sta {ZP_W0}
        lda #>sfx_play
        sta {ZP_W0}+1

        jmp initsfx_end

sfx_duration            = 0
sfx_waveform            = 1
sfx_atdc                = 2
sfx_ssrl                = 3
sfx_frequency           = 4
sfx_frequency_slide     = 6
sfx_frequency_bounce    = 9
sfx_pulse               = 10

sfx_play
        ldx #3
sfx_loop
        dex
        bpl sfx_loop_1
        rts

sfx_loop_1
        ldy sid_voice_offset,x

        lda {sfx_request},x            ;Jump here from interrupt
        beq sfx_loop_2
        jmp sfx_new_or_stop

sfx_loop_2
        lda sid_duration,x
        beq sfx_loop
        
        cmp #$ff
        beq sfx_continue
        dec sid_duration,x        ;sound still playing
        bne sfx_continue

sfx_release
        lda #$00
        sta sid_duration,x
        lda sid_waveform,x
        and #254
        sta sid+4,y        ;sound over
        jmp sfx_loop

sfx_stop
        lda #$00
        sta sid_duration,x
        sta sid+4,y        ;sound over
        jmp sfx_loop

sfx_continue
        lda sid_frequency_slide0,x
        bne sfx_frequency_slide_effect       ;frequency_slide on sound?
        lda sid_frequency_slide1,x
        bne sfx_frequency_slide_effect       ;frequency_slide on sound?
        jmp sfx_loop

sfx_frequency_slide_effect
        clc
        lda sid_frequency_lo,x        ;get voice freq lo byte and add
        adc sid_frequency_slide0,x
        sta sid,y
        sta sid_frequency_lo,x
        
        lda sid_frequency_hi,x        ;get voice freq hi byte and add
        adc sid_frequency_slide1,x  
        sta sid+1,y
        sta sid_frequency_hi,x

        lda sid_bouncetime,x
        bne sfx_frequency_slide_bounce
        jmp sfx_loop

sfx_frequency_slide_bounce
        sec
        sbc #1
        beq sfx_frequency_slide_bounce_switch
        sta sid_bouncetime,x
        jmp sfx_loop

sfx_frequency_slide_bounce_switch
        lda #$ff
        eor sid_frequency_slide0,x
        sta sid_frequency_slide0,x
        lda #$ff
        eor sid_frequency_slide1,x
        sta sid_frequency_slide1,x
        lda #$ff
        eor sid_frequency_slide2,x
        sta sid_frequency_slide2,x

        inc sid_frequency_slide0,x
        bne sfx_frequency_slide_bounce_switch_done
        inc sid_frequency_slide1,x
        bne sfx_frequency_slide_bounce_switch_done
        inc sid_frequency_slide2,x

sfx_frequency_slide_bounce_switch_done
        lda sid_bouncemax,x
        sta sid_bouncetime,x  ;reset timer
        jmp sfx_loop

sfx_new_or_stop
        lda #0
        sta {sfx_request},x

        lda {sfx_request_lo},x            ; init ptr to sfx struct
        cmp #$ff
        bne sfx_new
        lda {sfx_request_hi},x
        cmp #$ff
        bne sfx_new_or_stop_1
        jmp sfx_stop

sfx_new_or_stop_1
        lda {sfx_request_lo},x
sfx_new
        sta {ptr}
        lda {sfx_request_hi},x
        sta {ptr}+1

        ldy #sfx_duration
        lda ({ptr}),y
        sta sid_duration,x

        ldy #sfx_frequency_bounce
        lda ({ptr}),y
        sta sid_bouncetime,x
        sta sid_bouncemax,x

        ldy #sfx_frequency_slide
        lda ({ptr}),y
        sta sid_frequency_slide0,x
        iny
        lda ({ptr}),y
        sta sid_frequency_slide1,x
        iny
        lda ({ptr}),y
        sta sid_frequency_slide2,x
        
        ldy #sfx_frequency
        lda ({ptr}),y
        sta sid_frequency_lo,x
        iny
        lda ({ptr}),y
        sta sid_frequency_hi,x

        ldy #sfx_pulse
        lda ({ptr}),y
        sta sid_pulse_lo,x
        iny
        lda ({ptr}),y
        sta sid_pulse_hi,x

        ldy #sfx_atdc
        lda ({ptr}),y
        sta sid_atdc,x

        ldy #sfx_ssrl
        lda ({ptr}),y
        sta sid_ssrl,x

        ldy #sfx_waveform
        lda ({ptr}),y
        ora #1
        sta sid_waveform,x

        ldy sid_voice_offset,x

        lda sid_frequency_lo,x
        sta sid,y
        lda sid_frequency_hi,x
        sta sid+1,y
        lda sid_pulse_lo,x
        sta sid+2,y
        lda sid_pulse_hi,x
        sta sid+3,y
        lda sid_atdc,x
        sta sid+5,y
        lda sid_ssrl,x
        sta sid+6,y
        lda sid_waveform,x
        sta sid+4,y

        lda #$00
        sta {sfx_request_lo},x
        sta {sfx_request_hi},x
        jmp sfx_loop

sid_duration
        .byte 0,0,0 ;decrement
sid_bouncetime
        .byte 0,0,0 ;time until slide reversed
sid_bouncemax
        .byte 0,0,0 ;holds the reset value for bouncetime
sid_frequency_slide0
        .byte 0,0,0
sid_frequency_slide1
        .byte 0,0,0
sid_frequency_slide2
        .byte 0,0,0
sid_frequency_lo
        .byte 0,0,0
sid_frequency_hi
        .byte 0,0,0
sid_pulse_lo
        .byte 0,0,0
sid_pulse_hi
        .byte 0,0,0
sid_atdc
        .byte 0,0,0
sid_ssrl
        .byte 0,0,0
sid_waveform
        .byte 0,0,0

sid_voice_offset
        .byte 0, 7, 14

initsfx_end
    END ASM
    CALL InstallIrqRoutine(4, ZP_W0)
END SUB
