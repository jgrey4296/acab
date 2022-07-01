"""
Provides a straightforward way to activate pyparsing debugging,
with overriding debug functions
"""
from __future__ import annotations

import logging as logmod

import pyparsing as pp
import pyparsing.core as ppc
from acab.core.parsing.parse_debug_log_formatter import AcabParseDebugFormat
from acab.core.util.log_formatter import SimpleLogColour as SC

logging = logmod.getLogger(__name__)
logging.addHandler(AcabParseDebugFormat.scaffold())
logging.propagate = False

MARK     = SC.red(">!<")
MATCHING = SC.blue("Matching" + (" " * 17))
MATCHED  = SC.green("Matched" + ("-" * 14))
FAILED   = SC.red("Failed" + ("_" * 11))

def debug_pyparsing_active_p() -> bool:
    return pp.__diag__.enable_debug_on_named_expressions

def debug_pyparsing(*flags, all_warnings=False):
    """ Set pyparsing to debug
    Only applies for parsers created *after* this,
    so has to be set at boot time.
    """
    if not debug_pyparsing_active_p():
        logging.info("Enabling Debug on %s Parsers", SC.green("Named"))
        pp.__diag__.enabled_debug_on_named_expressions = True
        if bool(flags):
            for flag in flags:
                pp.enable_diag(flag)

        if all_warnings:
            pp.enable_all_warnings()
        ppc._default_success_debug_action                 = debug_success_action
        ppc._default_start_debug_action                   = debug_start_action
        ppc._default_exception_debug_action               = debug_fail_action
    else:
        logging.warning("PyParsing Debug is already active")


def debug_start_action(instring, loc, expr, *args):
    """
    Log Entry into parsers


    """
    context = _calc_mark_string(instring, loc)
    logging.warning("{ctx[0]:>10&yellow}{ctx[1]:3&red}{ctx[2]:<10&yellow} {} <{}> at {}:  ", MATCHING, SC.blue(expr.name), loc, extra={'ctx':context})

def debug_success_action(instring, startloc, endloc, expr, toks, *args):
    """
    Log Parser Success
    """
    context = _calc_mark_string(instring, endloc)
    logging.warning("{ctx[0]:>10&yellow}{ctx[1]:3&red}{ctx[2]:<10&yellow}\t {} <{}> ({}) {}", MATCHED, SC.green(expr.name), str(toks), endloc, extra={'ctx':context})

def debug_fail_action(instring, loc, expr, exc, *args):
    """
    Log Parser failure
    """
    if isinstance(exc, pp.ParseBaseException):
        found_str = exc.pstr[exc.loc:exc.loc + 1].replace(r'\\', '\\').replace("\n", "\\n")
        mark_str  = _calc_mark_string(instring, exc.loc)
        msg       = exc.msg
        loc       = exc.loc
    else:
        found_str = "AssertionError"
        mark_str  = ("", "", "")
        msg       = ""
        loc       = ""

    logging.error("{ctx[0]:>10&yellow}{ctx[1]:3&red}{ctx[2]:<10&yellow}\t\t {} <{}>: {} found '{}' at {}",
                  FAILED, SC.red(expr.name), SC.yellow(msg), SC.red(found_str),
                  loc, extra={'ctx':mark_str})


def dfs_activate(*parsers, remove=False):
    """ DFS on a parser, adding debug funcs to named sub parsers """
    queue = list(parsers)
    found = set()
    while bool(queue):
        current = queue.pop(0)
        if id(current) in found:
            continue
        else:
            found.add(id(current))

        if current.name is not None and not remove:
            current.set_debug_actions(debug_start_action,
                                      debug_success_action,
                                      debug_fail_action)
        if remove:
            current.set_debug_actions(None, None, None)

        if hasattr(current, 'expr'):
            queue.append(current.expr)
        elif hasattr(current, 'exprs'):
            queue += current.exprs


def _calc_mark_string(instring, loc, buffer=10):
    str_len  = len(instring)
    pre_str  = instring[max(0, loc-buffer):max(0, loc)]
    post_str = instring[max(0, loc):min(str_len, loc+buffer)]
    return pre_str.replace("\n", "\\n"), MARK, post_str.replace("\n", "\\n")
