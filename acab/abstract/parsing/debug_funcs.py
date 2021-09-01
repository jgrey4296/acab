import pyparsing as pp
import logging as root_logger
logging = root_logger.getLogger(__name__)

def debug_pyparsing():
    """ Set pyparsing to debug
    Only applies for parsers created *after* this,
    so has to be set at boot time.
    """
    logging.info("Enabling Debug on Named Parsers")
    pp.__diag__.enable_debug_on_named_expressions = True
    pp._defaultSuccessDebugAction                 = debug_success_action
    pp._defaultStartDebugAction                   = debug_start_action
    pp._defaultExceptionDebugAction               = debug_fail_action


def debug_start_action(instring, loc, expr):
    context = instring[loc-5:loc] + ">!<" + instring[loc:loc+5]
    context = context.replace("\n", "\\n")
    print("Match " + str(expr) + " at: " + context)

def debug_success_action(instring, startloc, endloc, expr, toks):
    context = instring[endloc-3:endloc] + ">!<" + instring[endloc:endloc+10]
    context = context.replace("\n", "\\n")
    print("\tMatched " + str(expr) + " (" + str(toks) + ") -> " + context)


def debug_fail_action(instring, loc, expr, exc):
    foundstr = exc.pstr[exc.loc:exc.loc + 1].replace(r'\\', '\\').replace("\n", "\\n")
    print("\t\tBad " + str(expr) + ": " \
          + exc.msg + ", found '" + foundstr + "' " \
          + exc.markInputline().replace("\n", "\\n"))
