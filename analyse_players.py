#!/usr/bin/python3

import os, subprocess
import pandas as pd
from enum import Enum

class PlayerType(Enum):
    # 0001
    CooperativeAggressive = 1
    # 0010
    CooperativeUnaggressive = 2
    # 0100
    UncooperativeAggressive = 3
    # 1000
    UncooperativeUnaggressive = 4

def main():
    def is_alive(status):
        return True if status==":survived" else False

    def cast_to_correct_type(row):
        return list(map(float, row[:-2])) + [is_alive(row[-2]), int(row[-1])]

    def classify_player(cooperation, aggression):
        if cooperation >= 0.5 and aggression >= 0.5:
            return PlayerType.CooperativeAggressive
        elif cooperation >= 0.5 and aggression < 0.5:
            return PlayerType.CooperativeUnaggressive
        elif cooperation < 0.5 and aggression >= 0.5:
            return PlayerType.UncooperativeAggressive
        else:
            return PlayerType.UncooperativeUnaggressive

    with open(f"outputs/players/log-player-all-even") as f:
        experiments = [line.split()[2:] for line in f.readlines()]
        for experiment in experiments:
            if len(experiment) == 7:
                experiment.append(-1)
        correct_experiments = map(cast_to_correct_type, experiments)
        column_labels = ["init_wtl", "final_wtl", "init_cooperation", "final_cooperation", "init_aggression", "final_aggression", "is_alive", "tick"]
        df = pd.DataFrame(correct_experiments, columns=column_labels)
        df["init_class"] = df.apply(lambda x: classify_player(x.init_cooperation, x.init_aggression), axis=1).astype("category")
        df["final_class"] = df.apply(lambda x: classify_player(x.final_cooperation, x.final_aggression), axis=1).astype("category")
        survivors_per_init_class = df.groupby(["init_class", "is_alive"]).count()
        player_class_change = df.groupby(["init_class", "final_class"]).count()
        breakpoint()

if __name__ == "__main__":
    main()