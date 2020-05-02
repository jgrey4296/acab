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
