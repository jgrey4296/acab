from pyrule.abstract.agenda import Agenda

class CycleAgenda(Agenda):
    """ Agenda for Cycling around a list of options """
    #eg: each time called, pick one from a list of characters?

    def __init__(self):
        super().__init__()
        self.register_variable("count", 0)
        self.register_variable("cycle_size", 5)
        self.register_variable("group_by", None)

    def __call__(self, proposals, engine, **kwargs):
        selected = []
        # group proposals into cycle groups

        # select group

        self.inc_group()
        return selected

    def inc_group(self):
        self.data['count'] += 1
        if self.data['count'] > self.data['cycle_size']:
            self.data['count'] = 0
