'INCLUDE "lib_types.bas"
'INCLUDE "lib_memory.bas"
'INCLUDE "lib_spr.bas"

DIM IrqHandler AS WORD SHARED
DIM irq_sid_addr AS WORD SHARED
DIM irq_sid_return_addr AS WORD SHARED
DIM irq_spr_addr AS WORD SHARED
DIM irq_spr_return_addr AS WORD SHARED

SUB IrqSid(Addr AS WORD) SHARED STATIC
    ASM
        sei
    END ASM
    IF Addr = 0 THEN
        irq_sid_addr = irq_sid_return_addr
    ELSE
        irq_sid_addr = Addr
    END IF
    ASM
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
    lda #<irq_handler_sid_return
    sta {irq_sid_addr}
    sta {irq_sid_return_addr}
    lda #>irq_handler_sid_return
    sta {irq_sid_addr}+1
    sta {irq_sid_return_addr}+1

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
    sta $dd0d
    
    lda #$01                        ;Raster interrupt on
    sta $d01a

    lda $d011
    and #%01111111                  ;High bit of interrupt position = 0
    sta $d011

    lda #IRQ1LINE                   ;Line where next IRQ happens
    sta $d012

    lda $dc0d                       ;Acknowledge IRQ (to be sure)
    lda $dd0d                       ;Acknowledge IRQ (to be sure)
    dec $d019                       ;Acknowledge raster (to be sure)
    cli

    jmp irq_end
;-----------------------------------
irq_handler:
;-----------------------------------
    dec $d019                   ; ACK any raster IRQs
    inc $d020

    jmp ({irq_sid_addr})
irq_handler_sid_return:

    jmp ({irq_spr_addr})
irq_handler_spr_return:
    nop
    dec $d020
    jmp $ea31
;-----------------------------------
irq_end:
;-----------------------------------
END ASM
