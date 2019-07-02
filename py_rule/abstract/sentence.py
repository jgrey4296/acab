import IPython

class Sentence:

    def __init__(self, words=None, negated=False, fallback=None):
        self._words = []
        self._negated = negated
        self._fallback = []
        if words is not None:
            self._words += words
        if fallback is not None:
            self._fallback += fallback[:]

    def __str__(self):
        result = "".join([str(x) for x in self._words[:-1]])
        result += self._words[-1].opless_print()
        negated_str = ""
        fallback_str = ""
        if self._fallback is not None:
            fallback_str = " || " + ", ".join(["${}:{}".format(x[0], x[1]) for x in self._fallback])
        if self._negated:
            negated_str = "~"

        return "{}{}{}".format(negated_str, result, fallback_str)

    def __repr__(self):
        return "Sentence({})".format(str(self))

    def __iter__(self):
        return iter(self._words)

    def __getitem__(self, i):
        return self._words.__getitem__(i)

    def __len__(self):
        return len(self._words)
