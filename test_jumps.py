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
class TestInfo:
    ID: str
    Cooperation: float
    Aggression: float
    WillToLive: float
    FuzzyCooperation: FuzzyLabel
    FuzzyAggression: FuzzyLabel
    CooperationDesire: float
    WTLDesire: float
    Jumped: bool
    CommonCooperation: float

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


def verifyCooperationDesire(test):
    min_cooperation = min_cooperation_lookup[test.FuzzyCooperation]
    return (test.CommonCooperation - min_cooperation == test.CooperationDesire)


def verifyWTLDesire(test):
    min_will_to_live = min_will_to_live_lookup[test.FuzzyAggression]
    return (test.WillToLive - min_will_to_live == test.WTLDesire)


def verifyJump(test):
    should_jump = test.CooperationDesire > 0 and test.WTLDesire > 0
    return should_jump == test.Jumped


def main(argv):
    tests = []
    common_cooperation = None
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
            _common_coop = float(fields[8])
            _jumped = ast.literal_eval(fields[9].title())
            test = TestInfo(
                _id, 
                _cooperation, 
                _aggression, 
                _wtl, 
                _fuzz_coop, 
                _fuzz_aggr, 
                _coop_desire, 
                _wtl_desire,
                _jumped,
                _common_coop
            )
            tests.append(test)

    for test in tests:
        assert(verifyClassification(test, "Cooperation"))
        assert(verifyClassification(test, "Aggression"))
        assert(verifyCooperationDesire(test))
        assert(verifyWTLDesire(test))
        assert(verifyJump(test))
    

if __name__ == "__main__":
    main(sys.argv[1])