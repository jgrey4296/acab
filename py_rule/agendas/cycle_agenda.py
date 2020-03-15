from pyrule.abstract.agenda import Agenda

# TODO
class CycleAgenda(Agenda):
    """ Agenda for Cycling around a list of options """
    #eg: each time called, pick one from a list of characters?

    def __init__(self):
        super().__init__()

    def __call__(self, proposals, engine, **kwargs):

        return
