INCLUDE "../lib_sid.bas"
INCLUDE "../lib_joy.bas"

DIM tune AS BYTE
    tune = 0
DIM Sid AS SidInfo

CALL Sid.Import(@SID_START, @SID_END)
CALL Sid.Debug()

PLAY:
    CALL Sid.Play(tune)
    CALL Joy1.WaitClick()
    tune = tune XOR 1
GOTO PLAY

SID_START:
INCBIN "Castle_of_Life.sid"
SID_END:

END
