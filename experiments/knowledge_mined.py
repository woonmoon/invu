#!/usr/bin/python3

import os, sys
import numpy as np
import matplotlib.pyplot as plt
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
    foo = df.groupby("label").agg(["mean", "median", "std", pd.Series.mode]).reset_index()
    breakpoint()


def num_survivors():
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
    foo = df.groupby("label").agg(["mean", "median", "std", pd.Series.mode]).reset_index()
    avg_num_survivors = df.groupby("label").mean()
    max_num_survivors = df.groupby("label").max()
    mode_num_survivors = df.groupby("label").agg(pd.Series.mode)
    breakpoint()

def players():
    # for player_file in os.listdir(""):
    with open("outputs/players/log-players-half-survive-cua") as f:
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
        bins = np.arange(0, 1.0, 0.05)
        survivors_init_wtl = pd.cut(df[df["is_alive"] == ":survived"].init_wtl, bins=bins)
        survivors_init_coop = pd.cut(df[df["is_alive"] == ":survived"].init_cooperation, bins=bins)
        survivors_init_aggr = pd.cut(df[df["is_alive"] == ":survived"].init_aggression, bins=bins)
        breakpoint()

def foo():
    ratios_ = [  
                (i,j,k,l) 
                for i in range(10) 
                for j in range(10) 
                for k in range(10) 
                for l in range(10) if i + j + k + l == 10
            ]
    ratios = map(lambda x: map(lambda y: float(y/10), x), ratios_)
    for i, ratio in enumerate(ratios):
        r = list(ratio)
        os.system(f"lein run {i} {r[0]} {r[1]} {r[2]} {r[3]}")

def main():
    filterchars = "{}:,"
    path = "configs/mega/all-survive/all-survive"
    data = []
    with open("outputs/mega/all_live/all_live") as f: 
        rows = [line.split() for line in f.readlines()]
        rows = map(lambda r: [r[0].replace("mega/all-survive/all-survive", ""), int(r[1])], rows)
        df_1 = pd.DataFrame(
            rows,
            columns=["label", "survivors"]
        )
        df_1.survivors.astype(int)
    
    all_live_df = df_1.groupby("label").agg(["mean", "std"]).reset_index()
    all_live_df.columns = ["label", "all_live_mean", "all_live_std"]

    with open("outputs/mega/half_die/half_die") as f: 
        rows = [line.split() for line in f.readlines()]
        rows = map(lambda r: [r[0].replace("mega/all-survive/all-survive", ""), int(r[1])], rows)
        df_2 = pd.DataFrame(
            rows,
            columns=["label", "survivors"]
        )
        df_2.survivors.astype(int)
    
    half_die_df = df_2.groupby("label").agg(["mean", "std"]).reset_index()
    half_die_df.columns = ["label", "half_die_mean", "half_die_std"]
    breakpoint()
    filterchars = "{}:,"
    path = "configs/mega/all-survive/all-survive"
    data = []
    for i in range(0, 282):
        with open(path+str(i)+".edn") as f: 
            str_config = "".join(c for c in f.read() if c not in filterchars)
            config = dict(list(map(lambda s: s.lstrip().split(), str_config.split("\n")))[:-1])
            data.append(
                [
                    i, 
                    config["cooperative-aggressive"], 
                    config["cooperative-unaggressive"], 
                    config["uncooperative-aggressive"],
                    config["uncooperative-unaggressive"]
                ]
            )

    df = pd.DataFrame(
        data, 
        columns=[
            "label", 
            "cooperative-aggressive",
            "cooperative-unaggressive",
            "uncooperative-aggressive",
            "uncooperative-unaggressive"
        ]
    )

    results = pd.merge(all_live_df, half_die_df).astype({"label": "int"})
    df = pd.merge(df, results)
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
    # fig.savefig("configs_half_survive.jpg")
    breakpoint()


if __name__ == "__main__":
    main()