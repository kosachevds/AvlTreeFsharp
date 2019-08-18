import os
import sys
from matplotlib import pyplot as pp


def main():
    if len(sys.argv) < 2:
        return
    values = read_values(sys.argv[1])
    plot_values(values)

    pp.show()


def read_values(filename):
    with open(filename, "rt") as fin:
        lines = fin.readlines()
    lines = [l for l in lines if l and l != "\n"]
    return [int(l) for l in lines]


def plot_values(values):
    pp.figure()
    pp.plot(values)

if __name__ == "__main__":
    main()
