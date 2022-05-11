#!/opt/anaconda3/envs/acab/bin/python3
"""
An Acab REPL
"""
import argparse
import importlib
import logging as logmod
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
parser.add_argument('--simple-log', action='store_true')

# Quiet hook from https://gist.github.com/jhazelwo/86124774833c6ab8f973323cb9c7e251
def main_repl():
    """ Top level entry point for ACAB repl.
    Can be used as setuptools entry_point

    Defaults to standard config if not overriden
    """
    # Add acab into the path:
    sys.path.append(abspath(join(split(__file__)[0], "../../..")))
    args = parser.parse_args()

    ## Setup initial logging
    ## (config log hook customizes later)
    LOGLEVEL      = logmod._nameToLevel[args.verbosity.upper()]
    LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

    full_file_handler = logmod.FileHandler(LOG_FILE_NAME, mode='w')
    console_handler   = logmod.StreamHandler()
    console_handler.setLevel(LOGLEVEL)

    root_logger = logmod.getLogger('')
    root_logger.setLevel(logmod.NOTSET)
    root_logger.addHandler(console_handler)
    root_logger.addHandler(full_file_handler)

    logging = logmod.getLogger(__file__)

    # Intercept print and log it to the trace file:
    from acab.modules.repl.util import capture_printing
    capture_printing()

    #====================
    from acab import setup
    config = setup(location=args.config, rich_exc=True, format_logs=not args.simple_log)
    logging.info("Loaded Config Location: {}", args.config)
    #====================

    # TODO change config details here

    if LOGLEVEL != console_handler.level:
        console_handler.setLevel(LOGLEVEL)

    if args.debug:
        parse_debug_spec = config.prepare("Parse", "DEBUG_PARSERS", _type=bool)
        config.override(parse_debug_spec, "True")

    #import then build engine or default trie engine from args
    initial_modules = config.prepare("Module.REPL", "MODULES")().replace("\n", " ")
    #--------------------
    # MAIN REPL LOGIC:
    from acab.modules.repl.repl_commander import AcabREPLCommander
    for key in config.attr.Module.Repl.CommandImports._keys:
        logging.debug("Importing: {}", key)
        importlib.import_module(key)


    repl = AcabREPLCommander()
    repl.onecmd(f"init {args.engine}")
    repl.onecmd(f"module {initial_modules}")

    print("\n--------------------------------------------------")
    repl.cmdloop()

if __name__ == "__main__":
    main_repl()
