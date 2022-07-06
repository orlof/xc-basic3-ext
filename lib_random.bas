FUNCTION random AS BYTE(min AS BYTE, max AS BYTE) SHARED STATIC
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

FUNCTION random_word AS WORD(min AS WORD, max AS WORD) SHARED STATIC
    DIM range AS WORD: range = max - min
    DO
        random_word = RNDW()
        DIM mask AS WORD: mask = 2
        DO
            if range < mask THEN random_word = random_word AND (mask - 1): EXIT DO
            mask = SHL(mask, 1)
        LOOP UNTIL mask = 0
    LOOP UNTIL random_word <= range
    random_word = random_word + min
END FUNCTION

