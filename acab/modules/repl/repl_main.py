#!/opt/anaconda3/envs/acab/bin/python3
"""
An Acab REPL
"""
import argparse
import importlib
import logging as root_logger
import sys
import traceback
from os.path import abspath, expanduser, split, splitext, join


##############################
# TODO default config file to data dir for distribution
parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog = "\n".join([""]))
parser.add_argument('--config', action="append", default=["/Volumes/documents/github/acab/acab/__configs/default"])
parser.add_argument('--engine', default="")
parser.add_argument('-v', '--verbosity', default="WARNING", help="The logging level to use, defaults to WARNING")
parser.add_argument('-d', '--debug', action="store_true", help="CLI control of parser debugging")


# Quiet hook from https://gist.github.com/jhazelwo/86124774833c6ab8f973323cb9c7e251
def main():
    """ Top level entry point for ACAB repl.
    Can be used as setuptools entry_point

    Defaults to standard config if not overriden
    """
    # Add acab into the path:
    sys.path.append(abspath(join(split(__file__)[0], "../../..")))

    args = parser.parse_args()

    LOGLEVEL = root_logger._nameToLevel[args.verbosity.upper()]
    LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
    root_logger.basicConfig(filename=LOG_FILE_NAME, level=max(0, LOGLEVEL - 10), filemode='w')

    console = root_logger.StreamHandler()
    console.setLevel(max(0, LOGLEVEL))
    console.setFormatter(root_logger.Formatter())
    root_logger.getLogger('').addHandler(console)
    logging = root_logger.getLogger(__name__)
    #====================
    from acab.abstract.config.config import AcabConfig
    from acab.abstract.config.modal import modal_config
    config = AcabConfig.Get()
    #====================
    logging.info("Reading Config: {}".format(args.config))
    args.config = [abspath(expanduser(x)) for x in args.config]
    config.Get(*args.config, hooks=[modal_config])

    # TODO change config details here

    if args.debug:
        parse_debug_spec = config.prepare("Parse", "DEBUG_PARSERS")
        config.override(parse_debug_spec, "True")

    #import then build engine or default trie engine from args
    initial_modules = config.prepare("Module.REPL", "MODULES")().replace("\n", " ")
    rebinder = config.prepare("Module.REPL", "REBIND")()

    #--------------------
    # MAIN REPL LOGIC:
    import acab.modules.repl.commands_control
    import acab.modules.repl.commands_core
    import acab.modules.repl.commands_info
    from acab.modules.repl.repl_commander import AcabREPLCommander
    from acab.modules.repl.util import build_rebind_instruction

    repl = AcabREPLCommander()
    repl.onecmd(f"init {args.engine}")
    repl.onecmd(f"module {initial_modules}")
    repl.state.engine(build_rebind_instruction(rebinder))


    print("\n--------------------------------------------------")
    repl.cmdloop()

if __name__ == "__main__":
    main_repl()
