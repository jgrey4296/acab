#!/usr/bin/env python3
from acab.abstract.config.config import AcabConfig
config = AcabConfig.Get()
NEGATION_S = config.value("Value.Structure", "NEGATION")

def split_clauses(sentences):
    """ Separate out the clauses of the query
        into positive and negative clauses
        """
    pos = []
    neg = []
    for c in sentences:
        if NEGATION_S in c.data and c.data[NEGATION_S]:
            neg.append(c)
        else:
            pos.append(c)

    return (pos, neg)
