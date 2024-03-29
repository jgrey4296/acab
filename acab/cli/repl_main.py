#!/opt/anaconda3/envs/acab/bin/python3
"""
An Acab REPL
"""
##-- imports
import argparse
import importlib
import logging as logmod
import sys
import traceback
from os.path import abspath, expanduser, split, splitext, join
import warnings

##-- end imports


##-- argparser
# TODO default config file to data dir for distribution
parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog = "\n".join([""]))
parser.add_argument('--config', action="append", default=["/Volumes/documents/github/acab/acab/__configs/default"])
parser.add_argument('--engine', default="")
parser.add_argument('-v', '--verbosity', default="WARNING", help="The logging level to use, defaults to WARNING")
parser.add_argument('-d', '--debug', action="store_true", help="CLI control of parser debugging")
parser.add_argument('--simple-log', action='store_true')

##-- end argparser

# Quiet hook from https://gist.github.com/jhazelwo/86124774833c6ab8f973323cb9c7e251
def main_repl():
    """ Top level entry point for ACAB repl.
    Can be used as setuptools entry_point

    Defaults to standard config if not overriden
    """
    ##-- sys path manipulation
    sys.path.append(abspath(join(split(__file__)[0], "../../..")))
    ##-- end sys path manipulation

    args = parser.parse_args()

    ##-- logging setup
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

    ##-- end logging setup

    # Intercept print and log it to the trace file:
    from acab_config.utils.print_logging import capture_printing
    capture_printing()

    #====================
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        import acab
        acab.setup(location=args.config, rich_exc=True, format_logs=not args.simple_log)
        logging.info("Loaded Config Location: {}", args.config)
        #====================

        # TODO change config details here

        if LOGLEVEL != console_handler.level:
            console_handler.setLevel(LOGLEVEL)

        if args.debug:
            acab.config.override(True).parse.debug_parsers()

        #import then build engine or default trie engine from args
        initial_modules = acab.config.on_fail([], list).module.repl.import.modules()
        initial_cmds    = acab.config.on_fail([], list).module.repl.import.commands()
        #--------------------
        # MAIN REPL LOGIC:
        from acab.modules.repl.repl_commander import AcabREPLCommander
        for cmd in initial_cmds:
            logging.debug("Importing: {}", cmd)
            importlib.import_module(cmd)

    repl = AcabREPLCommander()
    repl.onecmd(f"init {args.engine}")
    repl.onecmd(f"module {initial_modules}")

    print("\n--------------------------------------------------")
    repl.cmdloop()

if __name__ == "__main__":
    main_repl()
