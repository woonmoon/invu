#!/usr/bin/python3

import os, sys
import re
import pandas as pd

def gen_df(file_path, file_prefix):
    with open(file_path) as f:
        def process_row(row):
            config, trial_no = re.split("[-:]", row[0].replace(file_prefix, ""))
            def classify_player(cooperation, aggression):
                if cooperation >= 0.5 and aggression >= 0.5:
                    return "c-a"
                elif cooperation >= 0.5 and aggression < 0.5:
                    return "c-ua"
                elif cooperation < 0.5 and aggression >= 0.5:
                    return "uc-a"
                else:
                    return "uc-ua"

            return [
                int(config), 
                int(trial_no), 
                row[1], # id
                classify_player(float(row[4]), float(row[6])),
                float(row[2]), # init_wtl
                float(row[4]), # init_coop
                float(row[6]), # init_aggr
                classify_player(float(row[5]), float(row[7])),
                float(row[3]), # fin_wtl
                float(row[5]), # fin_coop
                float(row[7]), # fin_aggr
                True if row[8]==":survived" else False, # is_alive
                -1 if len(row) == 9 else int(row[9])
            ]
        
        rows = map(process_row, [line.split() for line in f.readlines()])
        df = pd.DataFrame(
            rows,
            columns=[
                "config_no", 
                "trial_no", 
                "id", 
                "init_class",
                "init_wtl",
                "init_coop",
                "init_aggr",
                "fin_class",
                "fin_wtl",
                "fin_coop",
                "fin_aggr",
                "is_alive",
                "tick"
            ]
        )
        df[["id", "init_class", "fin_class"]] = df[["id", "init_class", "fin_class"]].astype("category")
        df = df.round(
            {
                "init_wtl": 3, 
                "init_coop": 3, 
                "init_aggr": 3, 
                "fin_wtl": 3,
                "fin_coop": 3,
                "fin_aggr": 3
            }
        )
        return df

def main():
    # df = gen_df("outputs/all_survive/log_players", "all-survive/all-survive")
    df = gen_df("outputs/half_die/log_players", "half-die/half-die")
    init_class_survival = df.groupby(["init_class", "is_alive"])["is_alive"].count()
    fin_class_survival = df.groupby(["fin_class", "is_alive"])["is_alive"].count()
    # init_class_survival.to_csv("outputs/all_survive/init_class_survival.csv")
    # fin_class_survival.to_csv("outputs/all_survive/fin_class_survival.csv")
    init_class_survival.to_csv("outputs/half_die/init_class_survival.csv")
    fin_class_survival.to_csv("outputs/half_die/fin_class_survival.csv")

    # 35164 non timeouted dead players total
    top10p_dead = df[(~df["is_alive"]) & (df["tick"] > -1)].sort_values(["tick"], ascending=False).head(3517)
    init_class_top_dead = top10p_dead.groupby(["init_class"])["init_class"].count()
    fin_class_top_dead = top10p_dead.groupby(["fin_class"])["fin_class"].count()
    breakpoint()

if __name__ == "__main__":
    main()