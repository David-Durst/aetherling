from functools import reduce

circuits = {}
current_circuit_name = ""

in_file = open("resources.log")

for line in in_file:
    if line.__contains__("instances in current"):
        end_of_name = line.index("|") - 1
        current_circuit_name = line[:end_of_name]
        current_circuit = {}
    elif line == "\n":
        continue
    else:
        current_circuit[line.split("|")[0].strip()] = \
            int(line.split("|")[1]) + int(line.split("|")[2])
    circuits[current_circuit_name] = current_circuit

all_circuits_dicts = [c for c in circuits.values()]
all_circuits_dicts_keys = [set(d.keys()) for d in all_circuits_dicts]
all_keys = sorted(reduce(lambda x,y: x.union(y), all_circuits_dicts_keys))
csv_header = "circuit name," + ",".join(all_keys) + "\n"

out_file = open("resources.csv", "w+")
out_file.write(csv_header)
for name,elems in circuits.items():
    out_file.write(name)
    for k in all_keys:
        if k in elems:
            out_file.write("," + str(elems[k]))
        else:
            out_file.write(",0")
    out_file.write("\n")
print("hi")
