""""
Agendas are special cases of rules
"""
from enum import Enum
from acab.abstract.config.config import AcabConfig

from acab.abstract.rule.rule import Rule
from acab.abstract.rule.production_operator import ProductionOperator, ProductionContainer

util = AcabConfig.Get()

NAME_S      = util.value("Parse.Structure", "NAME")
STATEMENT_S = util.value("Parse.Structure", "STATEMENT")
QUERY_S     = util.value("Parse.Structure", "QUERY")
TRANSFORM_S = util.value("Parse.Structure", "TRANSFORM")
ACTION_S    = util.value("Parse.Structure", "ACTION")

RELATION_E  = Enum('Agenda_Relation', 'ONE MANY')

class Agenda(Rule):
    """
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


    def __call__(self, ctxs=None, engine=None):
        """ Runs an agenda rule on activated rules """
        assert(isinstance(ctxs, list))
        agenda_settings = super(Agenda, self).__call__(ctxs=ctxs, engine=engine)

        # TODO extract semantics
        assert(len(agenda_settings) == 1)
        settings = agenda_settings[0][0]

        # Enact agenda
        resulting_ctxs = self._action(settings, engine)
        return resulting_ctxs[0][Agenda.RETURN_NAME_S]



