""""
Agendas don't need to be parsable,
they just need to be registered so
layers and pipelines can be defined
"""
from enum import Enum
from py_rule.util import NAME_S, STATEMENT_S, TYPE_DEC_S, QUERY_S, TRANSFORM_S, ACTION_S
from py_rule.abstract.rule import Rule
from py_rule.abstract.production_operator import ProductionOperator, ProductionContainer

RELATION_E = Enum('Agenda_Relation', 'ONE MANY')

class Agenda(Rule):
    """ Abstract Class of Rule Layer Agendas
    Takes a set of potential rule activations
    and applys a transform, filter, or other function on them
    """

    RETURN_NAME_S = "__agenda_return_name__"

    def __init__(self,
                 query=None,
                 transform=None,
                 action=None,
                 name="AnonAgenda"):

        super(Agenda, self).__init__(query=query,
                                     transform=transform,
                                     action=action,
                                     name=name)
        # Whether the agenda expands or constrains proposals
        self._relation_type = (None, None)

        # TODO: verify actions to registered variables

    def __call__(self, ctxs=None, engine=None, **kwargs):
        """ Take the proposals, transform them in some way,
        then enact them on the engine """

        agenda_settings = super(Agenda, self).__call__(ctxs=ctxs, engine=engine)

        assert(len(agenda_settings) == 1)
        settings = agenda_settings[0][0]

        # Enact agenda
        resulting_ctxs = self._action(settings, engine, **kwargs)
        return resulting_ctxs[0][Agenda.RETURN_NAME_S]


# Utility construction function for parser
def make_agenda(toks):
    # Get Conditions
    if QUERY_S in toks:
        c = toks[QUERY_S][0][1]
        assert(isinstance(c, ProductionContainer))
    else:
        c = None

    # Get Transform
    if TRANSFORM_S in toks:
        t = toks[TRANSFORM_S][0][1]
        assert(isinstance(t, ProductionContainer))
    else:
        t = None

    # Get Action
    if ACTION_S in toks:
        a = toks[ACTION_S][0][1]
        assert(isinstance(a, ProductionContainer))
    else:
        a = None

    # make the agenda
    the_agenda = Agenda(query=c, transform=t, action=a)

    return  (the_agenda.type, the_agenda)
