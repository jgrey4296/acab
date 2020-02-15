
class Pipeline:
    """ Abstract Class to describe a rule engine pipeline
    Collects together sets of rules,
    and a sequence of agendas to run on their output
    """

    def __init__(self):
        self._layers = []


    # TODO: define interface
    # call
    def __call__(self, tick):
        return
    # input constraints
    # output constraints
