#!/usr/bin/python3

import os, subprocess
import pandas as pd
from enum import Enum

class PlayerType(Enum):
    # 0001
    Cooperative Aggressive = 1
    # 0010
    Cooperative Unaggressive = 2

def main():
    def is_alive(status):
        return True if status==":survived" else False

    def cast_to_correct_type(row):
        return list(map(float, row[:-2])) + [is_alive(row[-2]), int(row[-1])]

    def classify_player(cooperation, aggression):
        if cooperation > 0.5
    with open(f"outputs/players/log-player-all-even") as f:
        experiments = [line.split()[2:] for line in f.readlines()]
        for experiment in experiments:
            if len(experiment) == 7:
                experiment.append(-1)
        correct_experiments = map(cast_to_correct_type, experiments)
        column_labels = ["init_wtl", "final_wtl", "init_cooperation", "final_cooperation", "init_aggression", "final_aggression", "is_alive", "tick"]
        df = pd.DataFrame(correct_experiments, columns=column_labels)

        print(df.head().to_string())

if __name__ == "__main__":
    main()