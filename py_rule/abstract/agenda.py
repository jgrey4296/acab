""""
Agendas don't need to be parsable,
they just need to be registered so
layers and pipelines can be defined
"""
from enum import Enum
from py_rule.util import NAME_S, STATEMENT_S, TYPE_DEC_S
from py_rule.abstract.rule import Rule
from py_rule.abstract.production_operator import ProductionOperator

RELATION_E = Enum('Agenda_Relation', 'ONE MANY')


class AgendaAction(ProductionOperator):

    op_list = {}

class AgendaSelect(AgendaAction):
    pass

class AgendaSort(AgendaAction):
    pass

class AgendaSet(AgendaAction):
    pass

class AgendaReturn(AgendaAction):

    def __call__(self, returns, data=None, engine=None):
        return {Agenda.RETURN_NAME_S : returns}

class Agenda(Rule):
    """ Abstract Class of Rule Layer Agendas
    Takes a set of potential rule activations
    and applys a transform, filter, or other function on them
    """

    agenda_list = {}
    RETURN_NAME_S = "__agenda_return_name__"


    @staticmethod
    def construct_subclass_tree():
        """ Populate a dictionary for auto-generation of parser keywords """
        found = []
        queue = [Agenda]
        while bool(queue):
            current = queue.pop(0)
            if current in found:
                continue
            found += found
            queue += current.__subclasses__()

        # At this point, all subclasses have been found
        for x in found:
            Agenda.agenda_list[x.__name__] = x

        # TODO: use inspect module to get kwargs of each agenda type

    def __init__(self, query=None, transform=None, name="AnonAgenda"):

        super(Agenda, self).__init__(query=query,
                                     transform=transform,
                                     name=name)
        # Whether the agenda expands or constrains proposals
        self._relation_type = (None, None)

        # Late set queries and transforms
        self._registered_variables = {}

        # TODO: verify actions to registered variables

    def __call__(self, ctxs=None, engine=None, **kwargs):
        """ Take the proposals, transform them in some way,
        then enact them on the engine """

        agenda_settings = super(Agenda, self).__call__(ctxs=ctxs, engine=engine)

        assert(len(agenda_settings), 1)
        settings = agenda_settings[0][0]

        # Enact agenda
        resulting_ctxs = self._action(settings, engine, **kwargs)
        return resulting_ctxs[0][Agenda.RETURN_NAME_S]


    def agenda_logic(self, proposals, settings, engine, **kwargs):
        """ Where specific logic of agenda subclasses is implemented """
        raise NotImplementedError()

    def register_variable(self, sentence, init):
        # TODO
        raise NotImplementedError()


# Utility construction function for parser
def make_agenda(toks):
    # TODO complete this
    agenda_name = toks[NAME_S][0]
    agenda_setup_tuple = toks[STATEMENT_S][0]
    assert(agenda_setup_tuple == "agenda_dict")

    agenda_type = agenda_name[-1]._data[TYPE_DEC_S]
    assert(agenda_type in Agenda.agenda_list)
    # Get the agenda constructor
    constructor = Agenda.agenda_list[agenda_type]

    # make the agenda
    the_agenda = constructor(the_dict)

    return  (the_agenda.type, the_agenda)
