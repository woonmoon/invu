#!/usr/bin/python3

import os, sys
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
    avg_knowledge_mined = df.groupby("label").mean()
    max_knowledge_mined = df.groupby("label").max()
    mode_knowledge_mined = df.groupby("label").agg(pd.Series.mode)
    breakpoint()


def main():
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
    
if __name__ == "__main__":
    main()