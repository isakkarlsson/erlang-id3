import sys
NAME = sys.argv[1]
names = []
with open(NAME+".names") as file:
    for line in file.readlines():
        names.append("'" + line.strip() + "'")


output = []
with open(NAME+".data") as file:
    data = file.readlines()
    for line in data:
        attr = line.split(",")
        idx = 0
        line = []
        for name in names:
            line.append("{"+ name + ", '"+attr[idx].strip()+"'}")
            idx += 1
        output.append(line)

with open(NAME+"_erl.data", "w+") as file:
    str=""
    for line in output:
        str += "[" + ",".join(line) + "].\n"
    
    file.write(str)

#
# [{class, Value}, {f, Value}...N]
