"""
The parser for the REPL

"""
import logging as root_logger

import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.parsing import consts as PU

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

rst = pp.restOfLine()

# multi line ##################################################################
MULTI_LINE_START = config.prepare("Module.REPL", "MULTI_LINE_START")()
MULTI_LINE_END   = config.prepare("Module.REPL", "MULTI_LINE_END")()

multi_line_start = pp.Regex(MULTI_LINE_START)
multi_line_end = pp.Regex(MULTI_LINE_END)

multi_line_start.setParseAction(lambda s,l,t: "multi")
multi_line_end.setParseAction(lambda s,l,t: "multi")
# ##############################################################
# ie: sugar parser:

# TODO put sugar in config?
short_exit = pp.Keyword("q")
short_exit.setParseAction(lambda s,l,t: "exit")

short_break = pp.Keyword("b")
short_break.setParseAction(lambda s,l,t: "break")

short_echo = pp.Keyword("e")
short_echo.setParseAction(lambda s,l,t: "echo")

short_save = pp.Keyword("s")
short_save.setParseAction(lambda s,l,t: "save")

short_load = pp.Keyword("l")
short_load.setParseAction(lambda s,l,t: "load")

# "tick"
short_step = pp.Keyword("t")
short_step.setParseAction(lambda s,l,t: "step")


sugared = pp.LineStart() + pp.Suppress(pp.Literal(":")) + pp.Or([short_exit,
                                                                 short_break,
                                                                 short_echo,
                                                                 short_save,
                                                                 short_load,
                                                                 short_step
                                                                 ]) + rst

precmd_parser = pp.Suppress(pp.LineStart()) + pp.Or([multi_line_start,
                                                     multi_line_end,
                                                     sugared,
                                                     rst])

# step kws ####################################################################
back_kw     = pp.Keyword("back")
rule_kw     = pp.Keyword("rule")
layer_kw    = pp.Keyword("layer")
pipe_kw     = pp.Keyword("pipe")
pipeline_kw = pp.Keyword("pipeline")
step_parser = pp.Or([back_kw,
                     rule_kw + rst,
                     layer_kw,
                     pipe_kw,
                     pipeline_kw])

# print kws ###################################################################
wm_kw        = pp.Keyword("wm")
bootstrap_kw = pp.Keyword("bootstrap")
module_kw    = pp.Keyword("module")
bind_kw      = pp.Keyword("binding")

# TODO also handle bind without kw?
printer_parser = pp.Or([wm_kw,
                        bootstrap_kw,
                        module_kw + rst,
                        layer_kw + rst,
                        pipe_kw,
                        pipeline_kw,
                        bind_kw + rst])
# stat kws ####################################################################
agenda_kw = pp.Keyword("agenda")

stats_parser = pp.Or([wm_kw,
                      pipe_kw,
                      pipeline_kw,
                      layer_kw,
                      agenda_kw,
                      rule_kw,
                      module_kw,
                      bind_kw])

# listener ####################################################################

# add/remove/list/threshold

listen_parser = pp.Or([])


# misc ########################################################################
# number = pp.Word(pp.nums)
# slice_p = PU.s(pp.Literal('[')) + \
#     PU.op(number).setResultsName('first') + \
#     PU.op(PU.s(pp.Literal(':')) + number).setResultsName('second') + \
#     PU.s(pp.Literal(']'))

# param_p = pp.Or([number, slice_p])

# # print trie / query results / selected type
# print_alts = pp.Or([wm_kw, bootstrap_kw, layer_kw, module_kw, pipeline_kw, binding_kw])
