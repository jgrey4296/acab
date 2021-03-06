from acab.abstract.working_memory import WorkingMemory


class HyperGraphWM(WorkingMemory):
    """ A HyperGraph Working Memory  """

    def __init__(self):
        super().__init__()
        self._nodes = set()
        self._hyper_edges = set()


    # Methods to implement:
    def __eq__(self):
        raise NotImplementedError()

    def add(self, data):
        # add node?
        # add hyper edge?
        raise NotImplementedError()

    def retract(self, data):
        # retract node
        raise NotImplementedError()

    def query(self, data):

        raise NotImplementedError()

