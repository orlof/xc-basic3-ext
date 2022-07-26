import sys
import os

basename = os.path.basename(sys.argv[1])

with open(sys.argv[1], "rb") as f:
    data = f.read()

with open("out_bitmap_%s" % basename, "wb") as f:
    f.write(data[:8000])

with open("out_screen_%s" % basename, "wb") as f:
    f.write(data[8000:9000])

with open("out_color_%s" % basename, "wb") as f:
    f.write(data[9000:10000])

print("Screen color: %s" % data[10000])
print("Border color: %s" % data[10001])
