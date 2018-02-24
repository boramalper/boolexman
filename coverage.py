#!/usr/bin/env python3
import datetime
import os
import os.path
import re
import sys

def main(argc, argv):
    function_re = re.compile(r"^([a-zA-Z0-9'_]+) *::")

    functions = {}
    properties = {}
    n_files = 0
    n_lines = 0

    for root, dirs, files in os.walk("./src" if argc == 1 else argv[1]):
        for f in files:
            file_path = os.path.join(root, f)

            if not file_path.endswith(".hs"):
                print("Skipping `%s`" % (file_path,))
                continue

            with open(file_path) as fd:
                n_files += 1
                for line_no, line in enumerate(fd, 1):
                    n_lines += 1

                    m = function_re.findall(line)
                    if not m:
                        continue
                    else:
                        func = m[0]

                    if func.startswith("prop_"):
                        properties[func] = (file_path, line_no)
                    else:
                        functions[func] = (file_path, line_no)

    assert len(properties) <= len(functions)

    print("# boolexman Property Coverage")
    print()
    print("| Event | Date |")
    print("|:----- |:---- |")
    print("| Automatically generated on | %s |" % (datetime.datetime.utcnow().strftime("%Y-%m-%dT%H:%M:%SZ"),))
    print("| Tests completed on | YYYY-MM-DD |")
    print()

    print("| Function | Property | Testing Results |")
    print("|:-------- |:-------- |:--------------- |")
    for func in sorted(list(functions.keys())):
        prop = "prop_" + func

        if prop in properties:
            print("| %s | %s | _ |" % (link(func, *functions[func]), link(prop, *properties[prop])))
        else:
            print("| %s | *none* | *n/a* |" % (link(func, *functions[func])))

    print()
    print("%s functions<sup>*</sup> and %s properties in %s files consisting of %s lines in total." % (
        len(functions), len(properties), n_files, n_lines
    ))
    print()
    print("__*:__ Excluding _nested_ functions, and functions that lack an explicit type signature.")


def link(func, file_path, line_no):
    return "[`%s`](%s)" % (func, "https://github.com/boramalper/boolexman/blob/master/%s#L%s" % (file_path[2:], line_no))


if __name__ == "__main__":
    sys.exit(main(len(sys.argv), sys.argv))
