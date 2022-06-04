#!/usr/bin/python3

import os, subprocess

def main():
    # for config in os.listdir("configs"):
    for i in range(1, 151): 
        os.system(f"lein run configs/all-even.edn {i}")

if __name__ == "__main__":
    main()