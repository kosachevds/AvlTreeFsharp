import os
import sys
from matplotlib import pyplot as pp


def main(args):
    if len(args) < 2:
        return
    values = read_values(args[1])
    plot_values(values)

    pp.show()


def read_values(filename):
    with open(filename, "rt") as fin:
        lines = fin.readlines()
    lines = [line for line in lines if line and line != "\n"]
    return list(map(int, lines))


def plot_values(values):
    pp.figure()
    pp.plot(values)


if __name__ == "__main__":
    main(sys.argv)
