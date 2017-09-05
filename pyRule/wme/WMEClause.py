""" The WME Specific representation of a query clause """
import re
from pyRule import Clause
from pyRule.utils import Bind
from pyRule.Comparisons import COMP


class WMEClause(Clause):

    def __init__(self, components, negated=False, fallback=None):
        super().__init__(components, negated, fallback)

    def split_tests(self):
        """ Split the clause into alpha, beta, and regexs """
        alphas = []
        binds = []
        betas = []
        regexs = []
        for c in self.components:
            if len(c) == 2: #(c:$x)
                binds.append(c)
            elif len(c) == 3 and c[1] is COMP.RE:
                regexs.append(c)
            elif len(c) == 3 and isinstance(c[2], Bind): #(c < $x)
                betas.append(c)
            else:
                alphas.append(c)
        return (alphas, binds, betas, regexs)
