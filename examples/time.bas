INCLUDE "../lib_sysinfo.bas"
INCLUDE "../lib_time.bas"

CALL sysinfo_debug()
CALL time_reset(0, 0, 0, 0)

DO UNTIL 0
    DIM t AS TOD: t = time_tod()
    LOCATE 0, 0
    PRINT t.hour ; ":" ; t.minute ; ":" ; t.second ; ":" ; t.frac
LOOP
