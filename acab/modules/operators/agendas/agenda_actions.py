"""
 Cycle, expand, map, filter, random, ranking, sandbox

 Default ProductionStructure: (::ProductionStructure)
    | $proposals |

    AgendaReturn $proposals


 Sort ProductionStructure: (::ProductionStructure)
    | $proposals |

    sort.agenda.values.selection.$amnt?

    Sort $proposals by $proposals.ctx.$x? -> $sorted
    Select $amnt from $sorted             -> $selected

    Return $selected

 Test Layer: (::Layer)
   a.rule.set.$rules?
   default.$agenda?

   run $rules -> $proposals
   run $agenda with $proposals -> $selected

   perform $selected

 Test Pipeline: (::Pipeline)
    :module standard_operators
   :module standard_structures

   analysis.$tc(~= typecheck)?
   test.$layer?

   init:
       load ~/a/file.trie
       run layer $tc


   loop:
       run layer $layer
"""
from acab.core.data.instruction import ProductionOperator, ProductionStructure

# TODO do these need a wrapping decorator?
class AgendaSelect(ProductionOperator):

    def __call__(self, amnt, group, data=None):
        return group[:amnt]


class AgendaSort(ProductionOperator):

    def __call__(self, group, sort_query, data=None):
        # map the group to the sort_query value
        pairs = []

        # sort it
        sorted_group = []

        return sorted_group


class AgendaReturn(ProductionOperator):

    def __call__(self, returns, data=None):
        return {ProductionStructure.RETURN_NAME_S : returns}
