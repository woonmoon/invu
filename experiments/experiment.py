#!/usr/bin/python3

import os, subprocess

def main():
    for i in range(0, 30):
        breakpoint()
        os.system(f"lein run configs/half-die/half-die0.edn {i}")

if __name__ == "__main__":
    # main()