#!/usr/bin/python3

import os, sys
import numpy as np
import matplotlib.pyplot as plt
import itertools
import pandas as pd
import re

def config_df():
    filterchars = "{}:,"
    data = []
    for i in range(0, 282):
        with open("configs/all-survive/all-survive"+str(i)+".edn") as f: 
            str_config = "".join(c for c in f.read() if c not in filterchars)
            config = dict(list(map(lambda s: s.lstrip().split(), str_config.split("\n")))[:-1])
            data.append(
                [
                    i, 
                    float(config["cooperative-aggressive"]), 
                    float(config["cooperative-unaggressive"]), 
                    float(config["uncooperative-aggressive"]),
                    float(config["uncooperative-unaggressive"])
                ]
            )

    return pd.DataFrame(
        data, 
        columns=[
            "config_no", 
            "cooperative-aggressive",
            "cooperative-unaggressive",
            "uncooperative-aggressive",
            "uncooperative-unaggressive"
        ]
    )

def gen_state_csvs():
    config_no_to_player_ratios = config_df()

    def process_output(output_file_path, file_prefix):
        def process_row(row):
            config, trial_no = re.split("[-:]", row[0].replace(file_prefix, ""))
            return [int(config), int(trial_no), int(row[1]), float(row[2])]
        
        with open(output_file_path) as f:
            rows = map(process_row, [line.split() for line in f.readlines()])
            df = pd.DataFrame(
                rows,
                columns=["config_no", "trial_no", "num_survivors", "knowledge_mined"]
            )
            df = df.groupby("config_no")[["num_survivors", "knowledge_mined"]].agg(["mean", "std"])
            df = df.round(
                {
                    ("num_survivors", "mean"): 3, 
                    ("num_survivors", "std"): 3, 
                    ("knowledge_mined", "mean"): 3, 
                    ("knowledge_mined", "std"): 3
                }
            )
            df.columns = df.columns.to_flat_index()
            return config_no_to_player_ratios.join(df).sort_values(by=[("num_survivors", "mean")], ascending=False)

    all_live_df = process_output("outputs/all_survive/log_state", "all-survive/all-survive")
    half_die_df = process_output("outputs/half_die/log_state", "half-die/half-die")

    all_live_df.head(29).to_csv("outputs/all_survive/all_live.csv", index=False)
    half_die_df.head(29).to_csv("outputs/half_die/half_die.csv", index=False)
    return all_live_df, half_die_df


def correlation_csvs():
    all_live_df, half_die_df = gen_state_csvs()
    state_statistics = [
        ("num_survivors", "mean"), 
        ("num_survivors", "std"), 
        ("knowledge_mined", "mean"), 
        ("knowledge_mined", "std")
    ]
    all_live_corr_df = all_live_df.corr(method="pearson")[state_statistics].drop(state_statistics).round(3)
    all_live_corr_df.columns = all_live_corr_df.columns.to_flat_index()
    half_die_corr_df = half_die_df.corr(method="pearson")[state_statistics].drop(state_statistics).round(3)
    half_die_corr_df.columns = half_die_corr_df.columns.to_flat_index()

    all_live_corr_df.to_csv("outputs/all_survive/corr.csv")
    half_die_corr_df.to_csv("outputs/half_die/corr.csv")
    return all_live_corr_df, half_die_corr_df


def main():
    correlation_csvs()
    breakpoint()
    df_ca = df.groupby(["cooperative-aggressive"])[["all_live_mean", "half_die_mean"]].mean().reset_index()
    df_ca.columns = ["ratio", "all_live_mean_ca", "half_live_mean_ca"]
    df_cua = df.groupby(["cooperative-unaggressive"])[["all_live_mean", "half_die_mean"]].mean().reset_index()
    df_cua.columns = ["ratio", "all_live_mean_cua", "half_live_mean_cua"]
    df_uca = df.groupby(["uncooperative-aggressive"])[["all_live_mean", "half_die_mean"]].mean().reset_index()
    df_uca.columns = ["ratio", "all_live_mean_uca", "half_live_mean_uca"]
    df_ucua = df.groupby(["uncooperative-unaggressive"])[["all_live_mean", "half_die_mean"]].mean().reset_index()
    df_ucua.columns = ["ratio", "all_live_mean_ucua", "half_live_mean_ucua"]
    df_merged = pd.merge(pd.merge(df_ca, df_cua), pd.merge(df_uca, df_ucua))
    all_live_df = df_merged[["ratio", "all_live_mean_ca", "all_live_mean_cua", "all_live_mean_uca", "all_live_mean_ucua"]]
    half_live_df = df_merged[["ratio", "half_live_mean_ca", "half_live_mean_cua", "half_live_mean_uca", "half_live_mean_ucua"]]
    
    x = np.arange(0, 10, 1, dtype=int)
    width = 0.15

    fig, ax = plt.subplots()
    rects1 = ax.bar(x-(1.5*width), half_live_df["half_live_mean_ca"], width, label="Cooperative Aggressive")
    rects2 = ax.bar(x-(0.5*width), half_live_df["half_live_mean_cua"], width, label="Cooperative Unaggressive")
    rects3 = ax.bar(x+(0.5*width), half_live_df["half_live_mean_uca"], width, label="Uncooperative Aggressive")
    rects4 = ax.bar(x+(1.5*width), half_live_df["half_live_mean_ucua"], width, label="Uncooperative Unaggressive")

    ax.set_ylabel("Mean Number of Survivors")
    ax.set_xlabel("Proportion of Player Type")
    ax.set_xticks(x, labels=[0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9])

    ax.legend(loc='upper center', prop={'size':9})

    plt.figtext(0.4, 0.02, "Number of Players: 50, Number of Steps: 20, Number of Ticks: 45", ha="right", fontsize=5, bbox={"facecolor":"orange", "alpha":0.5, "pad":3})
    fig.suptitle("Number of Survivors With Varied Configurations", fontsize=10)
    fig.tight_layout()


if __name__ == "__main__":
    main()