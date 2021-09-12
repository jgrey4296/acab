import logging as root_logger

logging = root_logger.getLogger(__name__)

import acab
from acab.abstract.interfaces.debugger import AcabDebugger_i

config = acab.setup()


def build_slice(s, l, toks):
    first  = None
    second = None
    if 'first' in toks:
        first = toks['first'][0]

    if 'second' in toks:
        second = toks['second'][0]

    return slice(first, second)

def print_contexts(self, params):
    ctxs_to_print     = []
    bindings_to_print = []
    if "short_context" in params:
        try:
            ctxs_to_print.append(self.state.ctxs[params['short_context']])
        except IndexError as err:
            print(f"Selected bad ctx instance. Try 0 <= x < {len(self.state.ctxs)}.")

    elif "context_slice" in params:
        ctxs_to_print += self.state.ctxs[params['context_slice']]
    elif bool(self.state.ctxs) and len(self.state.ctxs) > 0:
        ctxs_to_print.append(self.state.ctxs[0])
    else:
        print(f"No applicable contexts to print")

    if "bindings" in params:
        bindings_to_print += params.bindings[:]

    logging.info("Ctxs: {}".format(ctxs_to_print))
    logging.info("Bindings: {}".format(bindings_to_print))

    # now print them
    for ctx in ctxs_to_print:
        if bool(bindings_to_print):
            for x in bindings_to_print:
                print("{} : {}".format(x, self.state.engine.pprint([ctx[x]])))
        else:
            for x,y in ctx.data.items():
                print("{} : {}".format(x, self.state.engine.pprint([y])))

        print("--------------------")


def ConfigBasedLoad(f):
    """ A Decorator to load the config specified debugger module
    and wrap a repl command with an instruction to get and start the
    debugger if necessary """
    if "Module.Debug" in config:
        logging.debug("Loading Debugger")
        mod = config.prepare("Module.Debug", "IMPORT", actions=[config.actions_e.IMPORT])()
        name = config.prepare("Module.Debug", "DEBUGGER")()
        debugger = getattr(mod, name)
        assert(issubclass(debugger, AcabDebugger_i)), debugger


    def wrapper(self, line):
        if self.state.debugger is None:
            logging.debug(f"Starting Debugger: {debugger.__name__}")
            self.state.debugger = debugger.Get()
            self.state.debugger.set_running_trace()

        return f(self, line)

    wrapper.__name__ = f.__name__

    return wrapper
