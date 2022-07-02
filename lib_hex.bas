FUNCTION hex AS STRING * 2 (n AS BYTE) SHARED STATIC OVERLOAD
    DIM h AS STRING * 16
    h = "0123456789abcdef"
    hex = "--"
    POKE @hex + 2, PEEK(@h + 1 + (n AND $0f))
    n = SHR(n, 4)
    POKE @hex + 1, PEEK(@h + 1 + (n AND $0f))
END FUNCTION

FUNCTION hex AS STRING * 4 (n AS INT) SHARED STATIC OVERLOAD
    hex = hex(peek(@n+1)) + hex(peek(@n))
END FUNCTION

FUNCTION hex AS STRING * 4 (n AS WORD) SHARED STATIC OVERLOAD
    hex = hex(peek(@n+1)) + hex(peek(@n))
END FUNCTION

FUNCTION hex AS STRING * 6 (n AS LONG) SHARED STATIC OVERLOAD
    hex = hex(peek(@n+2)) + hex(peek(@n+1)) + hex(peek(@n))
END FUNCTION

FUNCTION hex AS STRING * 9 (n AS FLOAT) SHARED STATIC OVERLOAD
    hex = hex(peek(@n+1)) + hex(peek(@n+2)) + hex(peek(@n+3)) + " " + hex(peek(@n)) 
END FUNCTION
