"""
Provides a straightforward way to activate pyparsing debugging,
with overriding debug functions
"""
import pyparsing as pp
import pyparsing.core as ppc
import logging as root_logger
from acab.core.parsing.debug_log_formatter import AcabParseDebugFormat
from acab.core.parsing.debug_log_formatter import SimpleColour as SC
logging = root_logger.getLogger(__name__)
logging.addHandler(AcabParseDebugFormat.scaffold())
logging.propagate = False

MARK     = SC.red(">!<")
MATCHING = SC.blue("Matching" + (" " * 17))
MATCHED  = SC.green("Matched" + ("-" * 14))
FAILED   = SC.red("Failed" + ("_" * 11))

def debug_pyparsing_active_p() -> bool:
    return pp.__diag__.enable_debug_on_named_expressions

def debug_pyparsing():
    """ Set pyparsing to debug
    Only applies for parsers created *after* this,
    so has to be set at boot time.
    """
    if not debug_pyparsing_active_p():
        logging.info("Enabling Debug on %s Parsers", SC.green("Named"))
        pp.enable_all_warnings()
        pp.__diag__.enable_debug_on_named_expressions = True
        ppc._default_success_debug_action                 = debug_success_action
        ppc._default_start_debug_action                   = debug_start_action
        ppc._default_exception_debug_action               = debug_fail_action
    else:
        logging.warning("PyParsing Debug is already active")


def debug_start_action(instring, loc, expr):
    context = SC.yellow(instring[loc-5:loc]) + MARK + SC.yellow(instring[loc:loc+5])
    context = context.replace("\n", "\\n")
    logging.info("%s <%s> at %s:  \"%s\"", MATCHING, SC.blue(expr.name), loc, context)

def debug_success_action(instring, startloc, endloc, expr, toks):
    context = SC.yellow(instring[endloc-5:endloc]) + MARK + SC.yellow(instring[endloc:endloc+5])
    context = context.replace("\n", "\\n")
    logging.warning("\t%s <%s> (%s) %s -> %s", MATCHED, SC.green(expr.name), str(toks), endloc, context)

def debug_fail_action(instring, loc, expr, exc):
    found_str = exc.pstr[exc.loc:exc.loc + 1].replace(r'\\', '\\').replace("\n", "\\n")
    mark_str  = (SC.yellow(instring[min(0, exc.loc-10):exc.loc-1]) + MARK + SC.yellow(instring[exc.loc:max(len(instring), exc.loc+10)])).replace("\n", "\\n")
    logging.error("\t\t%s <%s>: %s found '%s' at %s:  \"'%s\"",
                  FAILED, SC.red(expr.name), SC.yellow(str(exc.msg)), SC.red(found_str),
                  exc.loc, mark_str)


def dfs_activate(*parsers):
    """ DFS on a parser, adding debug funcs to named sub parsers """
    queue = list(parsers)
    while bool(queue):
        current = queue.pop(0)
        if current.name is not None:
            current.setDebugActions(debug_start_action,
                                    debug_success_action,
                                    debug_fail_action)
            if hasattr(current, 'expr'):
                queue.append(current.expr)
            elif hasattr(current, 'exprs'):
                queue += current.exprs
