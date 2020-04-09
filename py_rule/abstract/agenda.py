""""
Agendas don't need to be parsable,
they just need to be registered so
layers and pipelines can be defined
"""
from enum import Enum

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

    def __init__(self):
        # A function type signature
        self._type_signature = None
        # Whether the agenda expands or constrains proposals
        self._relation_type = None
        # Whether proposals are indexed, and what by
        self._is_indexed = False
        self._index = None

    def __call__(self, proposals, engine, **kwargs):
        """ Take the proposals, transform them in some way,
        then enact them on the engine """
        raise NotImplementedError()
