"""
A Data Structure that Self-Balances weights
Weights can be specified as (a < b), (a < c), (b < c), or (a < b < c)
with a range and a distritbution (linear, exponential etc),
and a,b,c will be ranked into bins that respect the constraints
where bins are (lowerbound, upperbound),
and where for any (a < b), (a.upperbound < b.lowerbound)
"""
import numpy as np
from collections import defaultdict
# from cairo_utils import BeachLine

defaultZeros = lambda : defaultdict(lambda: 0)

class WeightBalance:
    """  """

    def __init__(self, data=None, w_range=None, dist=None):
        assert(w_range is None or isinstance(w_range, list))
        assert(dist is None or callable(dist))
        if w_range is None:
            w_range = (0.0, 1.0)
        if dist is None:
            dist = lambda l, u, n: np.linspace(l, u, n)
        if data is None:
            data = []
        assert(all([len(x) == 2 for x in data]))
        self.range = w_range
        #Input data to balance / train on
        #Possibly: [ (x < y), ...]
        self.data = [CompWB(x,y) for x,y in data]
        # a_g = [ CompNode(x, ([y,z], [a,b])) ]
        #The mapping of x to CompNode
        self.comparisons = {}
        #The determined weights, spread across the distribution, after balancing
        self.weights = {}
        #Generation function, which specifes the distribution of weights between the range
        #   could be linspace, could be exponential, etc
        self.dist = dist

    def __call__(self):
        """ Set the balancing algorithm going """
        # self.aggregate()
        # t = BeachLine(arc=False)
        # t.insert_many(*list(self.comparisons.values()))
        # chain = [x for x in t.get_chain()]
        # #give each value in chain a range based on bins from the distribution function
        # xs = [x.value.key for x in chain]
        # ys = self.dist(self.range[0], self.range[1], len(xs))
        # #TODO: turn these into ranges?
        # self.weights = { x : y for (x,y) in zip(xs, ys) }

    def __getitem__(self, key):
        return self.weights[self.weights[key]]


    def aggregate(self):
        #Convert individual comparisons into aggregated comparison nodes
        for comp in self.data:
            if comp.a not in self.comparisons:
                self.comparisons[comp.a] = CompNode(comp.a, [comp.b])
            else:
                self.comparisons[comp.a].lt_set.add(comp.b)
            if comp.b not in self.comparisons:
                self.comparisons[comp.b] = CompNode(comp.b, [])

    def verify(self):
        """ Provide a statistic tuple on the state of the balance
        (errors, accuracy, precision) where
        errors = brokenComparisons_n / totalComparisons_n
        precision = { key : errors_for_key }
        """
        errors = 0
        precision = defaultZeros()
        for comp in self.data:
            lower_weight = self.weights[comp.a]
            upper_weight = self.weights[comp.b]
            if not (lower_weight < upper_weight):
                errors += 1
                precision[comp.a] += 1
                precision[comp.b] += 1
        #TODO: normalise precision by number of comps per key

        return (errors / len(self.data), precision)


class CompWB:
    def __init__(self, a, b):
        """ Class to hold the relation a < b """
        self.a = a
        self.b = b

    def __repr__(self):
        return "({} < {})".format(self.a, self.b)


class CompNode:
    """ A comparison node for a RB-Tree """
    def __init__(self, k, is_lt_than=None):
        # is_gt_than < key < is_lt_than
        assert(is_lt_than is None or isinstance(is_lt_than, list))
        self.key = k
        self.lt_set = set(is_lt_than)

    def __lt__(self, other):
        if isinstance(other, CompNode):
            return other.key in self.lt_set
        else:
            return other in self.lt_set

    def __eq__(self, other):
        return (other.key not in self.lt_set) and (self.key not in other.lt_set)

    def __repr__(self):
        return "Comp({} < {})".format(self.key, str(self.lt_set))
