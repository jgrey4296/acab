"""
A PyRule REPL, using default of TrieWM
"""
# Setup root_logger:
from os.path import splitext, split
import argparse
import importlib
import logging as root_logger

##############################
from py_rule.engines.trie_engine import TrieEngine
from py_rule.abstract.parsing import ReplParser as ReP
from py_rule.abstract.parsing import repl_commands as ReC



if __name__ == "__main__":

    #see https://docs.python.org/3/howto/argparse.html
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('--engine', default="py_rule.engines.trie_engine.TrieEngine")
    parser.add_argument('-v', '--verbose', default="WARNING")
    args = parser.parse_args()

    LOGLEVEL = root_logger._nameToLevel[args.verbose]
    LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
    root_logger.basicConfig(filename=LOG_FILE_NAME, level=max(0, LOGLEVEL - 10), filemode='w')

    console = root_logger.StreamHandler()
    console.setLevel(max(0, LOGLEVEL))
    root_logger.getLogger('').addHandler(console)
    logging = root_logger.getLogger(__name__)

    logging.info("Setting up engine: {}".format(args.engine))
    #import engine or defauilt trie engine from args
    init_module = importlib.import_module(splitext(args.engine)[0])
    # build engine
    engine = eval('init_module{}'.format(splitext(args.engine)[1]))()

    prompt = "PyRuleREPL: "
    multi_line_prompt = "... "
    command = None
    result = None
    current_str = input("PyRuleREPL: ")
    while command != ReC.ReplE.EXIT:
        try:
            # parse string in REPL parser
            parse_response = ReP.parseString(current_str)
            logging.debug("Parse Response: {}".format(parse_response))
            command, params = parse_response

            # Change prompt:
            if command == ReC.ReplE.PROMPT:
                prompt = params[0]
            else:
                # Or Lookup Command
                cmd_fn = ReC.get(command)
                # Perform the instruction
                engine, result = cmd_fn(engine, params)

        except Exception as exp:
            logging.exception("Error: {}".format(str(exp)))
            breakpoint()
            result = None
        finally:
            #print result
            if result is not None:
                print(result)
                result = None
            # Repeat
            if command != ReC.ReplE.EXIT:
                current_str = input(prompt)

    logging.info("Shutting down engine: {}".format(args.engine))
