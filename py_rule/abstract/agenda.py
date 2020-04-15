""""
Agendas don't need to be parsable,
they just need to be registered so
layers and pipelines can be defined
"""
from enum import Enum
from py_rule.util import NAME_S, STATEMENT_S, TYPE_DEC_S

RELATION_E = Enum('Agenda_Relation', 'ONE2ONE ONE2MANY MANY2ONE MANY2MANY')


# TODO: make this a subclass of production component?
class Agenda:
    """ Abstract Class of Rule Layer Agendas
    Takes a set of potential rule activations
    and applys a transform, filter, or other function on them
    """

    agenda_list = {}

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

    def __init__(self, queries, transforms, var_setting):
        # A function type signature
        self._type_signature = None
        # Whether the agenda expands or constrains proposals
        self._relation_type = None
        # Whether proposals are indexed, and what by
        self._is_indexed = False
        self._index = None

        # Late set queries and transforms
        self._registered_variables = {}
        self._queries = []
        self._transforms = []

    def __call__(self, proposals, engine, **kwargs):
        """ Take the proposals, transform them in some way,
        then enact them on the engine """
        raise NotImplementedError()


    def set_queries_and_transforms(self, queries, transforms):
        self._queries += queries
        self._transforms += transforms


    def register_variable(self, sentence, init):
        # TODO
        raise NotImplementedError()


# Utility construction function for parser
def make_agenda(toks):
    agenda_name = toks[NAME_S][0]
    agenda_setup_tuple = toks[STATEMENT_S][0]
    assert(agenda_setup_tuple == "agenda_dict")

    agenda_type = agenda_name[-1]._data[TYPE_DEC_S]
    assert(agenda_type in Agenda.agenda_list)
    # Get the agenda constructor
    constructor = Agenda.agenda_list[agenda_type]

    # make the agenda
    the_agenda = constructor(the_dict)


    # TODO actually, this doesnt chain into the normal make statement
    toks[STATEMENT_S] = (the_agenda.type, the_agenda)
    return False
