'INCLUDE "lib_sysinfo.bas"

TYPE TOD
    frac AS BYTE
    second AS BYTE
    minute AS BYTE
    hour AS BYTE
END TYPE

' CIA1_TOD = $DC08
' CIA2_TOD = $DD08
DIM ctime AS TOD @$DD08

DECLARE SUB time_pause(jiffys AS BYTE) SHARED STATIC
DECLARE SUB time_reset(hour AS BYTE, minute AS BYTE, second AS BYTE, frac AS BYTE) SHARED STATIC OVERLOAD
DECLARE FUNCTION time_tod AS TOD() SHARED STATIC

DIM hour_to_pm(24) AS BYTE @_HOUR_24_TO_PM

FUNCTION bin_to_bcd AS BYTE(b AS BYTE) SHARED STATIC
    ASM
        jmp bin_2_bcd
        ; table of BCD values for each binary bit, put this somewhere.
        ; note! values are -1 as the ADC is always done with the carry set
b2b_table:
        dc.b $63,$31,$15,$07,$03,$01,$00
bin_2_bcd:
        sed			        ; all adds in decimal mode
        lda #$00    	    ; clear A
        ldx #$07            ; set bit count
bit_loop:
        lsr {b}             ; bit to carry
        bcc skip_add        ; branch if no add
        adc b2b_table-1,X   ; else add BCD value
skip_add:
        dex                 ; decrement bit count
        bne bit_loop        ; loop if more to do
        sta {bin_to_bcd}    ; save result low byte
        cld                 ; clear decimal mode
    END ASM
END FUNCTION

SUB time_pause(jiffys AS BYTE) SHARED STATIC
    ASM
        ldx {jiffys}
time_pause_wait_positive
        bit $d011
        bmi time_pause_wait_positive
time_pause_wait_negative
        bit $d011
        bpl time_pause_wait_negative

        dex
        bne time_pause_wait_positive
    END ASM
END SUB

SUB time_reset(hour AS BYTE, minute AS BYTE, second AS BYTE, frac AS BYTE) SHARED STATIC
    IF sysinfo_pal() THEN
        POKE $DC0E, PEEK($DC0E) OR %10000000
        POKE $DD0E, PEEK($DC0E) OR %10000000
    END IF
    ctime.hour = hour_to_pm(hour)
    ctime.minute = bin_to_bcd(minute)
    ctime.second = bin_to_bcd(second)
    ctime.frac = bin_to_bcd(frac)
END SUB

FUNCTION time_tod AS TOD() SHARED STATIC
    DIM pm AS BYTE: pm = (ctime.hour AND %10000000)
    time_tod.hour = ctime.hour AND %01111111
    time_tod.hour = 10 * SHR(time_tod.hour, 4) + (time_tod.hour AND $0f)
    IF time_tod.hour = 12 THEN time_tod.hour = 0
    IF pm = %10000000 THEN time_tod.hour = time_tod.hour + 12

    time_tod.minute = 10 * SHR(ctime.minute, 4) + (ctime.minute AND $0f)
    time_tod.second = 10 * SHR(ctime.second, 4) + (ctime.second AND $0f)
    time_tod.frac = ctime.frac AND $0f
END FUNCTION

GOTO THE_END
_HOUR_24_TO_PM:
DATA AS BYTE $92, $01, $02, $03, $04, $05, $06, $07, $08, $09, $10, $11
DATA AS BYTE $12, $81, $82, $83, $84, $85, $86, $87, $88, $89, $90, $91

THE_END:
