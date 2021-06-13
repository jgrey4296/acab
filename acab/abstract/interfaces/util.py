"""
Utility decorators
"""
from acab.abstract.printing import consts as PC

def ForceListArgs(f):
    """ Force the first arg to be a list """
    def wrapped(self, *the_args, **the_kwargs):
        forced = []
        if isinstance(the_args[0], list):
            forced = the_args
        else:
            forced.append([the_args[0]])
            forced += the_args[1:]

        return f(self, *forced, **the_kwargs)

    wrapped.__name__ = f"FLA({f})"
    return wrapped



def JoinFinalResults(f):
    """ For printing, join the returned list together """
    def wrapped(self, *the_args, **the_kwargs):
        results = f(self, *the_args, **the_kwargs)
        return self.use(PC.PRINT_SENTINEL_JOIN_P).join(results)

    wrapped.__name__ = f"JFR({f})"
    return wrapped
