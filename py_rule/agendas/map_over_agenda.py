from pyrule.abstract.agenda import Agenda
from random import choice


# TODO add control over random choice
class MapOverAgenda(Agenda):
    """ Agenda to select an option for each provided index (eg: character) """

    def __init__(self, index):
        super().__init__()
        self._index = index

    def __call__(self, proposals, engine, **kwargs):
        selected = []
        grouped = {}
        for data, actions in proposals:
            if data[self._index] not in grouped:
                grouped[data[self._index]] = []
            grouped[data[self._index]].append((data, actions))

        for group in grouped.values():
            selected.append(choice(group))

        return selected
