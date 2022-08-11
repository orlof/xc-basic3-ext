SHARED CONST JOY_UP_MASK      = %00000001
SHARED CONST JOY_DOWN_MASK    = %00000010
SHARED CONST JOY_LEFT_MASK    = %00000100
SHARED CONST JOY_RIGHT_MASK   = %00001000
SHARED CONST JOY_FIRE_MASK    = %00010000
SHARED CONST JOY_ANY_DIR_MASK = %00001111
SHARED CONST JOY_ANY_MASK     = %00011111

TYPE Joystick
    Value AS BYTE
    Prev AS BYTE
    Addr AS WORD

    SUB Init(Addr AS WORD) STATIC
        THIS.Addr = Addr
        THIS.Prev = PEEK(THIS.Addr) AND JOY_ANY_MASK
        THIS.Value = THIS.Prev
    END SUB

    SUB Update() STATIC
        THIS.Prev = THIS.Value    
        THIS.Value = PEEK(THIS.Addr) AND JOY_ANY_MASK
    END SUB

    FUNCTION North AS BYTE() STATIC
        RETURN (THIS.Value AND JOY_UP_MASK) = 0
    END FUNCTION

    FUNCTION South AS BYTE() STATIC
        RETURN (THIS.Value AND JOY_DOWN_MASK) = 0
    END FUNCTION

    FUNCTION East AS BYTE() STATIC
        RETURN (THIS.Value AND JOY_RIGHT_MASK) = 0
    END FUNCTION

    FUNCTION West AS BYTE() STATIC
        RETURN (THIS.Value AND JOY_LEFT_MASK) = 0
    END FUNCTION

    FUNCTION Button AS BYTE() STATIC
        RETURN (THIS.Value AND JOY_FIRE_MASK) = 0
    END FUNCTION

    FUNCTION ButtonOn AS BYTE() STATIC
        RETURN ((THIS.Value XOR THIS.Prev) AND JOY_FIRE_MASK) <> 0 AND THIS.Button()
    END FUNCTION

    FUNCTION ButtonOff AS BYTE() STATIC
        RETURN ((THIS.Value XOR THIS.Prev) AND JOY_FIRE_MASK) <> 0 AND NOT THIS.Button()
    END FUNCTION

    SUB WaitClick() STATIC
        CALL THIS.Update()
        DO UNTIL THIS.ButtonOn()
            CALL THIS.Update()
        LOOP
    END SUB

    FUNCTION XAxis AS INT() STATIC
        IF THIS.West() THEN RETURN -1
        IF THIS.East() THEN RETURN 1
        RETURN 0
    END FUNCTION

    FUNCTION YAxis AS INT() STATIC
        IF THIS.North() THEN RETURN -1
        IF THIS.South() THEN RETURN 1
        RETURN 0
    END FUNCTION
END TYPE

DIM Joy1 AS Joystick SHARED
CALL Joy1.Init($dc01)

DIM Joy2 AS Joystick SHARED
CALL Joy2.Init($dc00)
