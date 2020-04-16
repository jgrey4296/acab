"""
A PyRule REPL, using default of TrieWM
"""
# Setup root_logger:
from os.path import splitext, split
import argparse
import importlib

import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
from py_rule.engines.trie_engine import TrieEngine
from py_rule.abstract.parsing import ReplParser as ReP
from py_rule.abstract.parsing import repl_commands as ReC



if __name__ == "__main__":
    #see https://docs.python.org/3/howto/argparse.html
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('--engine', default="py_rule.engines.trie_engine.TrieEngine")

    args = parser.parse_args()

    logging.info("Setting up engine: {}".format(args.engine))
    #import engine or defauilt trie engine from args
    init_module = importlib.import_module(splitext(args.engine)[0])
    # build engine
    engine = eval('init_module{}'.format(splitext(args.engine)[1]))()

    command = None
    result = None
    current_str = input("PyRuleREPL: ")
    while command != ReC.ReplE.EXIT:
        try:
            # parse string in REPL parser
            command, params = ReP.parseString(current_str)

            # Lookup the command
            cmd_fn = ReC.get(command)
            # Perform the instruction
            engine, result = cmd_fn(engine, params)
        except Exception as e:
            print("Error: {}".format(str(e)))
            result = None
        finally:
            #print result
            print(result)
            # Repeat
            if command != ReC.ReplE.EXIT:
            current_str = input("PyRuleREPL: ")

    logging.info("Shutting down engine: {}".format(args.engine))
