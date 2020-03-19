from pyrule.abstract.agenda import Agenda

# TODO
class CycleAgenda(Agenda):
    """ Agenda for Cycling around a list of options """
    #eg: each time called, pick one from a list of characters?

    def __init__(self, size):
        super().__init__()
        self._count = 0
        self._cycle_size = size

    def __call__(self, proposals, engine, **kwargs):


        self._count += 1
        if self._count > self._cycle_size:
            self._count = 0
        return
