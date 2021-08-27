import logging as root_logger
logging = root_logger.getLogger(__name__)


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
            ctxs_to_print.append(self.state.result[params['short_context']])
        except IndexError as err:
            print(f"Selected bad ctx instance. Try 0 <= x < {len(self.state.result)}.")

    elif "context_slice" in params:
        ctxs_to_print += self.state.result[params['context_slice']]
    elif bool(self.state.result) and len(self.state.result) > 0:
        ctxs_to_print.append(self.state.result[0])
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
