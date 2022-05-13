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
    FuzzyLabel.VLO: 0.1,
    FuzzyLabel.LO: 0.2,
    FuzzyLabel.MID: 0.3,
    FuzzyLabel.HI: 0.4,
    FuzzyLabel.VHI: 0.5 
}


def verifyClassification(player, _attr):
    attribute = getattr(player, _attr)
    fuzzy_attribute = getattr(player, "Fuzzy"+_attr)
    assert(0.0 <= attribute < 1.0)
    if 0 <= attribute < 0.2:
        return FuzzyLabel.VLO == fuzzy_attribute
    elif 0.2 <= attribute < 0.4:
        return FuzzyLabel.LO == fuzzy_attribute
    elif 0.4 <= attribute < 0.6:
        return FuzzyLabel.MID == fuzzy_attribute
    elif 0.6 <= attribute < 0.8:
        return FuzzyLabel.HI == fuzzy_attribute
    else:
        return FuzzyLabel.VHI == fuzzy_attribute


def verifyCooperationDesire(player, common_cooperation):
    min_cooperation = min_cooperation_lookup[player.FuzzyCooperation]
    return (common_cooperation - min_cooperation == player.CooperationDesire)


def verifyWTLDesire(player):
    min_will_to_live = min_will_to_live_lookup[player.FuzzyAggression]
    return (player.WillToLive - min_will_to_live == player.WTLDesire)


def verifyJump(player, player_to_jump):
    should_jump = player.CooperationDesire > 0 and player.WTLDesire > 0
    return should_jump == player_to_jump[player.ID]


def main(argv):
    players = []
    common_cooperation = None
    player_to_jump = {}
    with open(argv, 'r') as testfile:
        for line in testfile:
            fields = line.split()
            assert(len(fields) == 10)
            _id = fields[0]
            _cooperation = float(fields[1])
            _aggression = float(fields[2])
            _wtl = float(fields[3])
            _fuzz_coop = FuzzyLabel(fields[4])
            _fuzz_aggr = FuzzyLabel(fields[5])
            _coop_desire = float(fields[6])
            _wtl_desire = float(fields[7])
            common_coop = float(fields[8])
            jumped = ast.literal_eval(fields[9].title())
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
            common_cooperation = common_coop

    for player in players:
        assert(verifyClassification(player, "Cooperation"))
        assert(verifyClassification(player, "Aggression"))
        assert(verifyCooperationDesire(player, common_cooperation))
        assert(verifyWTLDesire(player))
        assert(verifyJump(player, player_to_jump))
    

if __name__ == "__main__":
    main(sys.argv[1])