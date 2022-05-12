#!/usr/bin/python3

import sys, getopt
import ast

from dataclasses import dataclass
from enum import Enum

class FuzzyLabel(Enum):
    VHI = ":VHI"
    HI = ":HI"
    MID = ":MID"
    LO = ":LO"
    VLO = ":VLO"

@dataclass
class PlayerInfo:
    ID: str
    Cooperation: float
    Aggression: float
    WillToLive: float
    FuzzyCooperation: FuzzyLabel
    FuzzyAggression: FuzzyLabel
    CooperationDesire: float
    WTLDesire: float

min_cooperation_lookup = {
    FuzzyLabel.VLO: 0.8,
    FuzzyLabel.LO: 0.7,
    FuzzyLabel.MID: 0.6,
    FuzzyLabel.HI: 0.5,
    FuzzyLabel.VHI: 0.4 
}

min_will_to_live_lookup = {
    FuzzyLabel.VLO: 0.4,
    FuzzyLabel.LO: 0.5,
    FuzzyLabel.MID: 0.6,
    FuzzyLabel.HI: 0.7,
    FuzzyLabel.VHI: 0.8 
}


def verifyClassification(player, _attr):
    attribute = getattr(player, _attr)
    fuzzy_attribute = getattr(player, "Fuzzy"+_attr)
    assert(0.0 <= attribute < 1.0)
    if 0 <= attribute < 0.2:
        assert(FuzzyLabel.VLO == fuzzy_attribute)
    elif 0.2 <= attribute < 0.4:
        assert(FuzzyLabel.LO == fuzzy_attribute)
    elif 0.4 <= attribute < 0.6:
        assert(FuzzyLabel.MID == fuzzy_attribute)
    elif 0.6 <= attribute < 0.8:
        assert(FuzzyLabel.HI == fuzzy_attribute)
    else:
        assert(FuzzyLabel.VHI == fuzzy_attribute)


def verifyCooperationDesire(player):
    min_cooperation = min_cooperation_lookup[player.FuzzyCooperation]
    print("Cooperation: ", player.Cooperation)
    print("Min Cooperation: ", min_cooperation)
    print("Copperation desire: ", player.CooperationDesire)
    assert(player.Cooperation - min_cooperation == player.CooperationDesire)


def verifyWTLDesire(player):
    min_will_to_live = min_will_to_live_lookup[player.FuzzyAggression]
    assert(player.WillToLive - min_will_to_live == player.WTLDesire)


def verifyJump(player, player_to_jump):
    if player.CooperationDesire > 0 and player.WTLDesire > 0:
        assert(player_to_jump[player.ID])


def main(argv):
    players = []
    player_to_jump = {}
    with open(argv, 'r') as testfile:
        for line in testfile:
            fields = line.split()
            assert(len(fields) == 9)
            _id = fields[0]
            _cooperation = float(fields[1])
            _aggression = float(fields[2])
            _wtl = float(fields[3])
            _fuzz_coop = FuzzyLabel(fields[4])
            _fuzz_aggr = FuzzyLabel(fields[5])
            _coop_desire = float(fields[6])
            _wtl_desire = float(fields[7])
            jumped = fields[8]
            player = PlayerInfo(
                _id, 
                _cooperation, 
                _aggression, 
                _wtl, 
                _fuzz_coop, 
                _fuzz_aggr, 
                _coop_desire, 
                _wtl_desire
            )
            players.append(player)
            player_to_jump[_id] = jumped

    for player in players:
        verifyClassification(player, "Cooperation")
        verifyClassification(player, "Aggression")
        verifyCooperationDesire(player)
        verifyWTLDesire(player)


if __name__ == "__main__":
    main(sys.argv[1])