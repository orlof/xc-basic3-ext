FUNCTION random AS BYTE(min AS BYTE, max AS BYTE) SHARED STATIC OVERLOAD
    DIM range AS BYTE: range = max - min
    DO
        random = RNDB()
        DIM mask AS BYTE: mask = 2
        DO
            if range < mask THEN random = random AND (mask - 1): EXIT DO
            mask = SHL(mask, 1)
        LOOP UNTIL mask = 0
    LOOP UNTIL random <= range
    random = random + min
END FUNCTION
