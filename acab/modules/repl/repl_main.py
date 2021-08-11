#!/opt/anaconda3/envs/acab/bin/python3
"""
An Acab REPL, using default of TrieWM
"""
import argparse
import importlib
import logging as root_logger
import sys
import traceback
from os.path import abspath, expanduser, split, splitext

##############################
parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog = "\n".join([""]))
parser.add_argument('--config', action="append", default=["/Volumes/documents/github/acab/acab/__configs/default"])
parser.add_argument('--engine', default="")
parser.add_argument('-v', '--verbosity', default="WARNING")


# Quiet hook from https://gist.github.com/jhazelwo/86124774833c6ab8f973323cb9c7e251
if __name__ == "__main__":
    args = parser.parse_args()

    LOGLEVEL = root_logger._nameToLevel[args.verbosity]
    LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
    root_logger.basicConfig(filename=LOG_FILE_NAME, level=max(0, LOGLEVEL - 10), filemode='w')

    console = root_logger.StreamHandler()
    console.setLevel(max(0, LOGLEVEL))
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

    #import then build engine or default trie engine from args
    initial_modules = config.prepare("Module.REPL", "MODULES")().replace("\n", " ")

    #--------------------
    # MAIN REPL LOGIC:
    import acab.modules.repl.commands_control
    import acab.modules.repl.commands_core
    import acab.modules.repl.commands_info
    from acab.modules.repl.repl_cmd import AcabREPL
    repl = AcabREPL()
    repl.onecmd(f"init {args.engine}")
    # repl.onecmd(f"module {initial_modules}")
    print("\n--------------------------------------------------")
    repl.cmdloop()
    # REPL loop over
    logging.info("Shutting down engine: {}".format(args.engine))