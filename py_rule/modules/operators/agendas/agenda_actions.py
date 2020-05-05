from py_rule.abstract.agenda import AgendaAction

class AgendaSelect(AgendaAction):
    pass

class AgendaSort(AgendaAction):
    pass

class AgendaSet(AgendaAction):
    pass

class AgendaReturn(AgendaAction):

    def __call__(self, returns, data=None, engine=None):
        return {Agenda.RETURN_NAME_S : returns}


# Cycle, expand, map, filter, random, ranking, sandbox


# Default Agenda: (::Agenda)
# 	| $proposals |
#
# 	AgendaReturn $proposals
#

# Sort Agenda: (::Agenda)
#    | $proposals |
#
#    sort.agenda.values.selection.$amnt?
#
#    Sort $proposals by $proposals.ctx.$x? -> $sorted
#    Select $amnt from $sorted             -> $selected
#
#    Return $selected

# Test Layer: (::Layer)
#   a.rule.set.$rules?
#   default.$agenda?
#
#   run $rules -> $proposals
#   run $agenda with $proposals -> $selected
#
#   perform $selected

# Test Pipeline: (::Pipeline)
# 	:module standard_operators
#   :module standard_structures
#
#   analysis.$tc(~= typecheck)?
#   test.$layer?
#
#   init:
#       load ~/a/file.trie
#       run layer $tc
#
#
#   loop:
#       run layer $layer
#
#
