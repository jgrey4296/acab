class Cycle:
    """ Represents a Repeating Cycle of nested sequences  """

    def __init__(self, sequence, name=None):
        assert(isinstance(sequence, list))
        self.name = name
        self.sequence = sequence
        self.location = 0

    def increment(self):
        self.location += 1
        if self.location == len(self.sequence):
            self.location = 0

    def current(self):
        return self.sequence[self.location]

    def __len__(self):
        return len(self.sequence)

    def __call__(self):
        value = self.current()
        self.increment()
        return value
