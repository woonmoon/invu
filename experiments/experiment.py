#!/usr/bin/python3

import os, subprocess

def main():
    for config in os.listdir("configs/mega/all-survive"):
        for i in range(0, 3):
            os.system(f"lein run configs/mega/all-survive/{config} {i}")
            # breakp oint()

if __name__ == "__main__":
    main()