#!/usr/bin/python3

import os, sys
import numpy as np
import pandas as pd


def knowledge_mined():
    def make_label_rows(filename, label):
        with open(filename) as f: 
            df = pd.DataFrame(
                [float(line.split()[-1]) for line in f.readlines()],
                columns=["knowledge_mined"]
            )
            df["label"] = label
            return df

    df = pd.concat(
        [
            make_label_rows(
                "outputs/state/log-state-all-die-ca", 
                "Cooperative Aggressive"
            ),
            make_label_rows(
                "outputs/state/log-state-all-die-cua",
                "Cooperative Unaggressive"
            ),
            make_label_rows(
                "outputs/state/log-state-all-die-uca", 
                "Uncooperative Aggressive"
            ),
            make_label_rows(
                "outputs/state/log-state-all-die-ucua", 
                "Uncooperative Unaggressive"
            )
        ]
    )
    df["label"].astype("category")
    foo = df.groupby("label").agg(["mean", "median", "var", pd.Series.mode]).reset_index()
    breakpoint()


def num_survivorss():
    def make_label_rows(filename, label):
        with open(filename) as f: 
            df = pd.DataFrame(
                [float(line.split()[1]) for line in f.readlines()],
                columns=["num_survivors"]
            )
            df["label"] = label
            return df

    df = pd.concat(
        [
            make_label_rows(
                "outputs/state/log-state-all-survive-ca", 
                "Cooperative Aggressive"
            ),
            make_label_rows(
                "outputs/state/log-state-all-survive-cua",
                "Cooperative Unaggressive"
            ),
            make_label_rows(
                "outputs/state/log-state-all-survive-uca", 
                "Uncooperative Aggressive"
            ),
            make_label_rows(
                "outputs/state/log-state-all-survive-ucua", 
                "Uncooperative Unaggressive"
            )
        ]
    )
    foo = df.groupby("label").agg(["mean", "median", "var", pd.Series.mode]).reset_index()
    avg_num_survivors = df.groupby("label").mean()
    max_num_survivors = df.groupby("label").max()
    mode_num_survivors = df.groupby("label").agg(pd.Series.mode)
    breakpoint()

def main():
    # for player_file in os.listdir(""):
    with open("outputs/players/log-players-half-survive-ca") as f:
        df = pd.DataFrame(
            [line.split() for line in f.readlines()],
            columns= [
                "experiment",
                "id",
                "init_wtl",
                "final_wtl",
                "init_cooperation", 
                "final_cooperation", 
                "init_aggression", 
                "final_aggression", 
                "is_alive", 
                "tick"
            ]
        )
        df["unique_id"] = df["experiment"].astype(str) + df["id"].astype(str)
        df.drop(["experiment", "id"], axis=1, inplace=True)
        df.tick.fillna(-1, inplace=True)
        df = df.astype(
            {
                "init_wtl": "float", 
                "final_wtl": "float", 
                "init_cooperation": "float",
                "final_cooperation": "float", 
                "init_aggression": "float", 
                "final_aggression": "float",
                "is_alive": "category",
                "tick": "int"
            }
        )
        df["init_coop_class"] = pd.cut(
            df.init_cooperation, 
            bins=[0, 0.5, 1.0], 
            labels=["Uncooperative", "Cooperative"]
        )
        df["init_aggr_class"] = pd.cut(
            df.init_aggression,
            bins=[0, 0.5, 1.0],
            labels=["Unaggressive", "Aggressive"]
        )
        df["init_class"] = df["init_coop_class"].astype(str) + df["init_aggr_class"].astype(str)
        df.drop(["init_coop_class", "init_aggr_class"], axis=1, inplace=True)
        df["final_coop_class"] = pd.cut(
            df.final_cooperation, 
            bins=[0, 0.5, 1.0], 
            labels=["Uncooperative", "Cooperative"]
        )
        df["final_aggr_class"] = pd.cut(
            df.final_aggression,
            bins=[0, 0.5, 1.0],
            labels=["Unaggressive", "Aggressive"]
        )
        df["final_class"] = df["final_coop_class"].astype(str) + df["final_aggr_class"].astype(str)
        df.drop(["final_coop_class", "final_aggr_class"], axis=1, inplace=True)
        foo = df.groupby(["init_class", "is_alive"]).count()
        breakpoint()


if __name__ == "__main__":
    main()