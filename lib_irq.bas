'INCLUDE "lib_types.bas"
'INCLUDE "lib_memory.bas"
'INCLUDE "lib_spr.bas"

DIM IrqHandler AS WORD SHARED

DIM irq_routine_lo(5) AS BYTE
DIM irq_routine_hi(5) AS BYTE
FOR ZP_B0 = 0 TO 4
    irq_routine_lo(ZP_B0) = $ff
    irq_routine_hi(ZP_B0) = $ff
NEXT

'DIM irq_sid_addr AS WORD SHARED
'DIM irq_sid_return_addr AS WORD SHARED
DIM irq_spr_addr AS WORD SHARED
DIM irq_spr_return_addr AS WORD SHARED

SUB InstallIrqRoutine(Priority AS BYTE, Addr AS WORD) SHARED STATIC
    ASM
        sei

        ldx {Priority}
        lda {Addr}
        sta {irq_routine_lo},x
        lda {Addr}+1
        sta {irq_routine_hi},x

        cli
    END ASM
END SUB

SUB IrqSpr(Addr AS WORD) SHARED STATIC
    ASM
        sei
    END ASM
    IF Addr = 0 THEN
        irq_spr_addr = irq_spr_return_addr
    ELSE
        irq_spr_addr = Addr
    END IF
    ASM
        cli
    END ASM
END SUB

SUB Reset() SHARED STATIC
    ASM
        lda #0          ; disable raster interrupts
        sta $d01a
        lda #$ff
        sta $dc0d       ; enable cia interrupts
        sta $dd0d
    END ASM
END SUB

ASM
IRQ1LINE        = $fc               ;This is the place on screen where the sorting IRQ happens
    sta 1024
    lda #<irq_handler_spr_return
    sta {irq_spr_addr}
    sta {irq_spr_return_addr}
    lda #>irq_handler_spr_return
    sta {irq_spr_addr}+1
    sta {irq_spr_return_addr}+1

    sei
    lda #<irq_handler
    sta $0314
    sta {IrqHandler}
    lda #>irq_handler
    sta $0315
    sta {IrqHandler}+1

    lda #$7f                        ;CIA interrupt off
    sta $dc0d
;    sta $dd0d
    and $d011                       ;High bit of interrupt position = 0
    sta $d011
    
    lda #IRQ1LINE                   ;Line where next IRQ happens
    sta $d012

    lda $dc0d                       ;Acknowledge IRQ (to be sure)
;    lda $dd0d                       ;Acknowledge IRQ (to be sure)

    lda #$01
    sta $d019                       ;Acknowledge raster (to be sure)
    sta $d01a                       ;Raster interrupt on

    cli

    jmp irq_end

;-----------------------------------
irq_handler:
;-----------------------------------
    inc $d020
    ; BIT $D019
    ; BPL NotVICTryCIA
    ; IRQ_from_VIC:
    lda #$ff                        ; ACK any VIC IRQs
    sta $d019

    ldx #5
irq_routine_loop:
    dex
    bmi irq_handler_spr

    lda {irq_routine_hi},x
    cmp #$ff
    beq irq_routine_loop

    sta irq_routine_jsr + 2

    lda {irq_routine_lo},x
    sta irq_routine_jsr + 1

    ;inc $d020

    txa
    pha
irq_routine_jsr:
    jsr $beef
    pla
    tax

    jmp irq_routine_loop

irq_handler_spr:
    ;inc $d020
    jmp ({irq_spr_addr})
irq_handler_spr_return:
    dec $d020
    jmp $ea31
;-----------------------------------
irq_end:
;-----------------------------------
END ASM
