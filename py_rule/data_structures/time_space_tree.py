class TimeSpaceTree:
    """ A Giddens inspired TimeSpace Pyramid/Tree  """

    def __init__(self, name=None):
        self.name = name
        self.root = None
        #The current location in the subtrees
        self.location = None

    def verify(self):
        raise Exception("Not implemented yet")


class Node:
    """ A Node in the TSTree. The start is the at least the start of the parent,
    the end is at most the end of the parent """

    def __init__(self, start, end, parent):
        self.start = start
        self.parent = parent
        self.children = []
        self.end = end
        self.data = {}

    def __len__(self):
        return self.end - self.start

    def __getitem__(self, x):
        return self.children[x]

    def children(self):
        return len(self.children)

    def verify(self):
        raise Exception("Not implemented yet")

