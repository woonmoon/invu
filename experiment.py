#!/usr/bin/python3

import os, subprocess

def main():
    for config in os.listdir("configs"):
        for i in range(1, 6): 
            print(f"lein run configs/{config} {i}")
        # for i in range(0, 1): 
        #     os.system(f"lein run {config} {i}")

if __name__ == "__main__":
    main()