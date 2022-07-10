# xc-basic3-ext
Extension libraries for XC-BASIC3

## lib_spr.bas

Basic functions for controlling sprites. Can work in either 8 sprite or 16 sprite (multiplexed) modes.
Multiplexed cannot set individual sprites double width/height, multicolor or priority properties.

Sprite coordinates match C64 hires screen coordinates x: 0-319 and y 0-199. Negative coordinates
are allowed.

