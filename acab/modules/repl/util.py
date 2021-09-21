import logging as root_logger
import importlib
from types import FunctionType, ModuleType

logging = root_logger.getLogger(__name__)
import acab
from acab.abstract.interfaces.debugger import AcabDebugger_i
from acab.abstract.interfaces.engine import AcabEngine_i

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
        ctx_slice = self.state.ctxs[params['context_slice']].active_list()
        ctxs_to_print += ctx_slice
    elif bool(self.state.ctxs) and len(self.state.ctxs) > 0:
        ctxs_to_print += self.state.ctxs.active_list()
    else:
        print(f"No applicable contexts to print")

    if "bindings" in params:
        bindings_to_print += params.bindings[:]

    logging.info("Ctxs: {}".format(ctxs_to_print))
    logging.info("Bindings: {}".format(bindings_to_print))

    # now print them
    for i,ctx in enumerate(ctxs_to_print):
        print(f"Context: {i}")
        # if bool(ctx.continuation):
        #     print(f"Continuation: {ctx.continuation}")
        if bool(bindings_to_print):
            for x in bindings_to_print:
                print("{} : {}".format(x, self.state.engine.pprint([ctx[x]])))
        else:
            for x,y in ctx.data.items():
                print("{} : {}".format(x, self.state.engine.pprint([y])))

        print("--------------------")

    print("Named (continuation) Sets:")
    print(self.state.engine.pprint(list(self.state.ctxs._named_sets.keys())))

def init_inspect(mod_str):
    """
    Import and Inspect the passed in module for potential constructor functions
    to init with
    """
    mod = importlib.import_module(mod_str)
    try:
        not_dunders    = [getattr(mod, x) for x in dir(mod) if "__" not in x]
        not_modules    = [x for x in not_dunders if not isinstance(x, ModuleType)]
        correct_module = [x for x in not_modules if mod_str in x.__module__]
        funcs = [x for x in correct_module if isinstance(x, FunctionType)]
        engines        = [x for x in correct_module if isinstance(x, type) and issubclass(x, AcabEngine_i)]
        total = funcs + engines

        if not bool(total):
            return print(f"No Available Constructors in {mod_str}")

        print(f"Potential Constructors in {mod_str}:")
        for x in total:
            print(f"-- {x.__name__}")
    except:
        breakpoint()

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
