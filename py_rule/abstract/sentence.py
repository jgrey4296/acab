import IPython

class Sentence:

    def __init__(self, words=None):
        self._words = []
        if words is not None:
            self._words += words

    def __str__(self):
        result = "".join([str(x) for x in self._words[:-1]])
        result += self._words[-1].opless_print()
        return result

    def __repr__(self):
        return "Sentence({})".format(str(self))

    def __iter__(self):
        return iter(self._words)

    def __getitem__(self, i):
        return self._words.__getitem__(i)

    def __len__(self):
        return len(self._words)
