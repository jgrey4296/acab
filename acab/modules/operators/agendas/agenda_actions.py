"""
 Cycle, expand, map, filter, random, ranking, sandbox

 Default Agenda: (::Agenda)
    | $proposals |

    AgendaReturn $proposals


 Sort Agenda: (::Agenda)
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
from acab.abstract.action import ActionOp
from acab.abstract.transform import TransformOp
from acab.abstract.agenda import Agenda

class AgendaSelect(TransformOp):

    def __call__(self, amnt, group, data=None, engine=None):
        return group[:amnt]


class AgendaSort(TransformOp):

    def __call__(self, group, sort_query, data=None, engine=None):
        # map the group to the sort_query value
        pairs = []

        # sort it
        sorted_group = []

        return sorted_group


class AgendaReturn(ActionOp):

    def __call__(self, returns, data=None, engine=None):
        return {Agenda.RETURN_NAME_S : returns}
