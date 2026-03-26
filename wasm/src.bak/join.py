import os

eq = open('../eq.gleam').read()

print(eq)

for f in os.listdir("."):
    if not f.endswith('.gleam'):
        continue
    print(f)
    funcs = []
    content = open(f).read()
    for token in content.replace("()", " ").split():
        if token.endswith('examples'):
            funcs.append(token)
    f = open(f, "a")
    f.write("\n")
    f.write(eq)
    f.write("pub fn main() {\n")
    for func in funcs:
        f.write("  " + func + "()\n")
    f.write("}")
