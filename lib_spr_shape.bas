'INCLUDE "lib_memory.bas"

REM ****************************************************************************
REM Copies sprite pattern from DATA AS BYTE statements to address specified by 
REM given FramePtr (16384 * VIC_BANK + 64 * FramePtr = dest_address)
REM ****************************************************************************
DECLARE SUB SprShapeImport(SrcAddr AS WORD, FramePtr AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Creates new pattern from existing pattern by mirroring it horizontally.
REM ****************************************************************************
DECLARE SUB SprShapeFlipX(SrcFramePtr AS BYTE, DstFramePtr AS BYTE) SHARED STATIC

REM ****************************************************************************
REM Creates new pattern from existing pattern by mirroring it vertically.
REM ****************************************************************************
DECLARE SUB SprShapeFlipY(SrcFramePtr AS BYTE, DstFramePtr AS BYTE) SHARED STATIC

REM ****************************************************************************
REM CALL SprImportShape(@PLAYER_SHIP, 255)
REM CALL SprFrame(0, 255)
REM ...
REM PLAYER_SHIP:
REM DATA AS BYTE 1,2,3,4,5...
REM ****************************************************************************
REM Copies sprite pattern from DATA AS BYTE statements to address specified by 
REM given FramePtr (16384 * VIC_BANK + 64 * FramePtr = dest_address)
REM [Developer is responsible that the area is free]
REM ****************************************************************************
SUB SprShapeImport(SrcAddr AS WORD, FramePtr AS BYTE) SHARED STATIC
    MEMCPY SrcAddr, spr_vic_bank_addr + SHL(CWORD(FramePtr), 6), 63
END SUB

REM ****************************************************************************
REM Creates new pattern from existing pattern by mirroring it horizontally.
REM ****************************************************************************
SUB SprShapeFlipX(SrcFramePtr AS BYTE, DstFramePtr AS BYTE) SHARED STATIC
    ZP_W0 = spr_vic_bank_addr + SHL(CWORD(SrcFramePtr), 6)
    ZP_W1 = spr_vic_bank_addr + SHL(CWORD(DstFramePtr), 6)

    DIM tmp AS BYTE
    ASM
        ldy #60
flip_x_row:
        lda ({ZP_W0}),y
        sta {tmp}
        lda #128
flip_x_byte0:
        asl {tmp}
        ror
        bcc flip_x_byte0
        tax

        iny
        iny

        lda ({ZP_W0}),y
        sta {tmp}
        txa
        sta ({ZP_W1}),y
        lda #128
flip_x_byte2:
        asl {tmp}
        ror
        bcc flip_x_byte2
        tax

        dey

        lda ({ZP_W0}),y
        sta {tmp}
        lda #128
flip_x_byte1:
        asl {tmp}
        ror
        bcc flip_x_byte1
        sta ({ZP_W1}),y

        dey

        txa
        sta ({ZP_W1}),y

        dey
        dey
        dey
        bpl flip_x_row
    END ASM
END SUB

REM ****************************************************************************
REM Creates new pattern from existing pattern by mirroring it vertically.
REM ****************************************************************************
SUB SprShapeFlipY(SrcFramePtr AS BYTE, DstFramePtr AS BYTE) SHARED STATIC
    ZP_W0 = spr_vic_bank_addr + SHL(CWORD(SrcFramePtr), 6)
    ZP_W1 = spr_vic_bank_addr + SHL(CWORD(DstFramePtr), 6) + 60
    FOR t AS BYTE = 0 TO 63
        POKE ZP_W1+2, PEEK(ZP_W0 + 2)
        POKE ZP_W1+1, PEEK(ZP_W0 + 1)
        POKE ZP_W1, PEEK(ZP_W0)
        ZP_W0 = ZP_W0 + 3
        ZP_W1 = ZP_W1 - 3
    NEXT t
END SUB

