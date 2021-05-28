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

# Quiet hook from https://gist.github.com/jhazelwo/86124774833c6ab8f973323cb9c7e251
if __name__ == "__main__":
    from acab.abstract.config.config import AcabConfig
    config = AcabConfig.Get()

    #see https://docs.python.org/3/howto/argparse.html
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('--config', action="append")
    parser.add_argument('--engine', action="append")
    parser.add_argument('-v', '--verbose', default="WARNING")

    args = parser.parse_args()
    args.config = [abspath(expanduser(x)) for x in args.config]

    LOGLEVEL = root_logger._nameToLevel[args.verbose]
    LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
    root_logger.basicConfig(filename=LOG_FILE_NAME, level=max(0, LOGLEVEL - 10), filemode='w')

    console = root_logger.StreamHandler()
    console.setLevel(max(0, LOGLEVEL))
    root_logger.getLogger('').addHandler(console)
    logging = root_logger.getLogger(__name__)

    logging.info("Reading Config: {}".format(args.config))
    config.read_list(args.config)

    # Only after having read in config, import rest of acab
    from acab.modules.repl import ReplParser as ReP
    from acab.modules.repl import repl_commands as ReC

    logging.info("Setting up engine: {}".format(args.engine))
    #import then build engine or default trie engine from args
    engine = config("REPL", "ENGINE")
    if args.engine is not None:
        engine = args.engine

    init_module = importlib.import_module(splitext(engine)[0])
    # build engine
    engine, dummy = ReC.get(ReC.ReplE.INIT)(None, {'params': [engine]})
    # TODO Load Standard modules
    load_cmd        = ReC.get(ReC.ReplE.MODULE)
    initial_modules = config("REPL", "MODULES", actions=[AcabConfig.actions_e.LIST])
    engine, dummy   = load_cmd(engine, {'params': initial_modules})

    from acab.modules.repl.repl_state import ReplState
    data = ReplState()
    while data.command != ReC.ReplE.EXIT:
        try:
            data.current_str = input(data.prompt + " ")
            # parse string in REPL parser
            parse_response = ReP.parseString(data.current_str,
                                             in_multi_line=data.in_multi_line)
            logging.debug("Parse Response: {}".format(parse_response))
            data.update(parse_response)

            # perform command:
            if data.command is not None:
                cmd_fn = ReC.get(data.command)
                # Perform the instruction
                engine, u_data = cmd_fn(engine, data)
                if u_data is not None:
                    data.update(u_data)

        except Exception as exp:
            if data.stack:
                logging.exception(str(exp))
                exc_type, exc_value, exc_tb = sys.exc_info()
                # traceback.print_tb(exc_tb, limit=4)

            print("(Trace: {}) Error: {}".format(data.stack, str(exp)))
            breakpoint()
            data.result  = ["Look at data, and engine"]

        if (not data.in_multi_line) and data.echo:
            cmd_fn = ReC.get(ReC.ReplE.PRINT)
            engine, u_data = cmd_fn(engine, {'params': ['wm']})
            print(u_data['result'])
        #print result
        if bool(data.result):
            print(data.result)
            data.result = None

    logging.info("Shutting down engine: {}".format(args.engine))
