"""
An Acab REPL, using default of TrieWM
"""
# Setup root_logger:
from pyparsing import ParseException
from os.path import expanduser, abspath
from os.path import splitext, split
import sys
import argparse
import importlib
import logging as root_logger
import traceback

##############################
parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog = "\n".join([""]))
parser.add_argument('--config', action="append")
parser.add_argument('--engine', default="")
parser.add_argument('-v', '--verbosity', default="WARNING")


# Quiet hook from https://gist.github.com/jhazelwo/86124774833c6ab8f973323cb9c7e251
if __name__ == "__main__":
    from acab.abstract.config.config import AcabConfig
    config = AcabConfig.Get()
    #====================
    args = parser.parse_args()
    args.config = [abspath(expanduser(x)) for x in args.config]

    LOGLEVEL = root_logger._nameToLevel[args.verbose]
    LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
    root_logger.basicConfig(filename=LOG_FILE_NAME, level=max(0, LOGLEVEL - 10), filemode='w')

    console = root_logger.StreamHandler()
    console.setLevel(max(0, LOGLEVEL))
    root_logger.getLogger('').addHandler(console)
    logging = root_logger.getLogger(__name__)
    #====================
    logging.info("Reading Config: {}".format(args.config))
    config.read_list(args.config)

    logging.info("Setting up engine: {}".format(args.engine))
    #import then build engine or default trie engine from args
    engine = config("REPL", "ENGINE")
    if args.engine is not None:
        engine = args.engine

    initial_modules = " ".join(config("REPL", "MODULES", actions=[AcabConfig.actions_e.LIST]))
    #--------------------
    # MAIN REPL LOGIC:
    from acab.modules.repl.repl_cmd import AcabREPL
    import acab.modules.repl.repl_commands
    repl = AcabRepl()
    repl.onecmd(f"init {args.engine}")
    repl.onecmd(f"module {initial_modules}")
    repl.cmdloop()
    # REPL loop over
    logging.info("Shutting down engine: {}".format(args.engine))
