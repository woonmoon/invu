#!/usr/bin/python3

import os, sys
import numpy as np
from collections import Counter
from matplotlib import pyplot as plt

def num_survivors():
    num_survivor_labels = [0, 1, 2, 3, 4, 5]
    knowledge_mined_labels = [0, 0.2, 0.4, 0.6, 0.8, 1.0]
    config_to_num_survivors = {}
    cfg_label = { 
        "0001": "Cooperative Aggressive",
        "0010": "Cooperative Unaggressive",
        "0100": "Uncooperative Aggressive",
        "1000": "Uncooperative Unaggressive"
    }
    x = np.arange(len(num_survivor_labels))
    width = 0.15
    for log_file in os.listdir("outputs/state"):
        with open(f"outputs/state/{log_file}") as f:
            config = log_file.replace("log-state-", "")
            # For num-survivors
            config_to_num_survivors[cfg_label[config]] = list(zip(*sorted(Counter([line.split()[1] for line in f.readlines()]).items())))[1]
    
    fig, ax = plt.subplots()
    rects1 = ax.bar(x-(1.5*width), config_to_num_survivors["Cooperative Aggressive"], width, label="Cooperative Aggressive")
    rects2 = ax.bar(x-(0.5*width), config_to_num_survivors["Cooperative Unaggressive"], width, label="Cooperative Unaggressive")
    rects3 = ax.bar(x+(0.5*width), config_to_num_survivors["Uncooperative Aggressive"], width, label="Uncooperative Aggressive")
    rects4 = ax.bar(x+(1.5*width), config_to_num_survivors["Uncooperative Unaggressive"], width, label="Uncooperative Unaggressive")

    ax.set_ylabel("Frequency of Number of Survivors")
    ax.set_xlabel("Number of Survivors")
    ax.set_xticks(x, num_survivor_labels)

    ax.legend(prop={'size':9})

    ax.bar_label(rects1, fontsize=8, padding=5)
    ax.bar_label(rects2, fontsize=8, padding=5)
    ax.bar_label(rects3, fontsize=8, padding=5)
    ax.bar_label(rects4, fontsize=8, padding=5)
    plt.figtext(0.4, 0.02, "Number of Players: 10, Number of Steps: 5, Number of Ticks: 10", ha="right", fontsize=5, bbox={"facecolor":"orange", "alpha":0.5, "pad":3})
    fig.suptitle("Number of Survivors With Homogenous Agent Populations", fontsize=10)
    fig.tight_layout()
    fig.savefig("homo-agents-num-survivors.jpg")

def main():
    config_to_knowledge_mined = {}
    cfg_label = { 
        "0001": "Cooperative Aggressive",
        "0010": "Cooperative Unaggressive",
        "0100": "Uncooperative Aggressive",
        "1000": "Uncooperative Unaggressive"
    }
    x = np.arange(6)
    width = 0.15
    for log_file in os.listdir("outputs/state"):
        with open(f"outputs/state/{log_file}") as f:
            config = log_file.replace("log-state-", "")
            frequencies = Counter([line.split()[2] for line in f.readlines()])
            frequencies.setdefault("0.0", 0)
            frequencies.setdefault("0.2", 0)
            frequencies.setdefault("0.4", 0)
            frequencies.setdefault("0.6", 0)
            frequencies.setdefault("0.8", 0)
            frequencies.setdefault("1.0", 0)
            config_to_knowledge_mined[cfg_label[config]] = list(zip(*sorted(frequencies.items())))[1]
    
    fig, ax = plt.subplots()
    rects1 = ax.bar(x-(1.5*width), config_to_knowledge_mined["Cooperative Aggressive"], width, label="Cooperative Aggressive")
    rects2 = ax.bar(x-(0.5*width), config_to_knowledge_mined["Cooperative Unaggressive"], width, label="Cooperative Unaggressive")
    rects3 = ax.bar(x+(0.5*width), config_to_knowledge_mined["Uncooperative Aggressive"], width, label="Uncooperative Aggressive")
    rects4 = ax.bar(x+(1.5*width), config_to_knowledge_mined["Uncooperative Unaggressive"], width, label="Uncooperative Unaggressive")

    ax.set_ylabel("Frequency of Knowledge Mined")
    ax.set_xlabel("Knowledge Mined")
    ax.set_xticks(x, ["0.0", "0.2", "0.4", "0.6", "0.8", "1.0"])

    ax.legend(prop={'size':9})

    ax.bar_label(rects1, fontsize=8, padding=5)
    ax.bar_label(rects2, fontsize=8, padding=5)
    ax.bar_label(rects3, fontsize=8, padding=5)
    ax.bar_label(rects4, fontsize=8, padding=5)
    plt.figtext(0.4, 0.02, "Number of Players: 10, Number of Steps: 5, Number of Ticks: 10", ha="right", fontsize=5, bbox={"facecolor":"orange", "alpha":0.5, "pad":3})
    fig.suptitle("Frequency of Knowledge-Mined With Homogenous Agent Populations", fontsize=10)
    fig.tight_layout()
    fig.savefig("homo-agents-knowledge-mined.jpg")
    

if __name__ == "__main__":
    main()