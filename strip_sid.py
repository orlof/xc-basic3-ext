import sys
import os

basename = os.path.basename(sys.argv[1])

with open(sys.argv[1], "rb") as f:
    data = f.read()

with open("out_%s" % basename, "wb") as f:
    f.write(data[0x7e:])

print("Init: %s" % hex(int.from_bytes(data[0x0a:0x0c], "big")))
print("Play: %s" % hex(int.from_bytes(data[0x0c:0x0e], "big")))
print("Base: %s" % hex(int.from_bytes(data[0x7c:0x7e], "little")))
#print("Play: %s" % hex(256*data[0x0c] + data[0x0d]))
#print("Base: %s" % hex(256*data[0x7d] + data[0x7c]))
