#!/usr/bin/python3

import os, sys
import numpy as np
from collections import Counter
from matplotlib import pyplot as plt

def main():
    num_survivor_labels = [0, 1, 2, 3, 4, 5]
    knowledge_mined_labels = [0, 0.2, 0.4, 0.6, 0.8, 1.0]
    config_to_num_survivors = {}
    config_to_knowledge_mined = {}
    cfg_label = { 
        "0001": "Cooperative Aggressive",
        "0010": "Cooperative Unaggressive",
        "0100": "Uncooperative Aggressive",
        "1000": "Uncooperative Unaggressive"
    }
    x = np.arange(len(num_survivor_labels))
    width = 0.5
    for log_file in os.listdir("outputs/state"):
        with open(f"outputs/state/{log_file}") as f:
            config = log_file.replace("log-state-", "")
            config_to_num_survivors[cfg_label[config]] = list(zip(*sorted(Counter([line.split()[1] for line in f.readlines()]).items())))[1]
            # config_to_knowledge_mined[cfg_label[config]] = [ line for line in f.readlines()]
    
    fig, ax = plt.subplots()
    rects1 = ax.bar(x-width/4, config_to_num_survivors["Cooperative Aggressive"], width, label="Cooperative Aggressive")
    rects2 = ax.bar(x+width/4, config_to_num_survivors["Cooperative Unaggressive"], width, label="Cooperative Unaggressive")
    rects3 = ax.bar(x+width/4, config_to_num_survivors["Uncooperative Aggressive"], width, label="Uncooperative Aggressive")
    rects4 = ax.bar(x+width/4, config_to_num_survivors["Uncooperative Unaggressive"], width, label="Uncooperative Unaggressive")

    ax.set_ylabel("Frequency of Number of Survivors")
    ax.set_xticks(x, num_survivor_labels)
    ax.legend()

    ax.bar_label(rects1, padding=3)
    ax.bar_label(rects2, padding=3)
    ax.bar_label(rects3, padding=3)
    ax.bar_label(rects4, padding=3)

    fig.tight_layout()
    plt.show()
    print(config_to_num_survivors)
    # fig, ax = plt.subplots()

if __name__ == "__main__":
    main()