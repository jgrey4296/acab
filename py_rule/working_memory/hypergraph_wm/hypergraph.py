from py_rule.abstract.working_memory import WorkingMemory


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

    def _insert_into_values_parser(self, parser):
        """ Inserts new value types that can be parsed in a sentence
        Should look like FP.OTHER_VALS << or_d_parser
        """
        raise NotImplementedError()

    def _insert_into_statement_parser(self, parser):
        """ Inserts new statements entirely """
        raise NotImplementedError()

    def _build_operator_parser(self):
        """ This Method calls each parser component's
        'build_operators' function, that populates Forward defined
        parser combinators *after* modules are loaded.
        This ensures operators are included in the parsers """
        raise NotImplementedError()
