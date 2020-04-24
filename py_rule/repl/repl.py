"""
A PyRule REPL, using default of TrieWM
"""
# Setup root_logger:
from pyparsing import ParseException
from os.path import splitext, split
import sys
import argparse
import importlib
import logging as root_logger
import traceback

##############################
from py_rule.repl import ReplParser as ReP
from py_rule.repl import repl_commands as ReC
from py_rule.abstract.printing import util as PrU
from py_rule import util

# Quiet hook from https://gist.github.com/jhazelwo/86124774833c6ab8f973323cb9c7e251
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

    #import then build engine or default trie engine from args
    init_module = importlib.import_module(splitext(args.engine)[0])
    # build engine
    engine, dummy = ReC.get(ReC.ReplE.INIT)(None, {'params': [args.engine]})

    data = { 'prompt' : 'PyRuleREPL: ',
             'prompt_ml' : '... ',
             'command': ReC.ReplE.NOP,
             'params' : [],
             'result' : None,
             'current_str' : None,
             'collect_str' : [],
             'in_multi_line': False}
    data['current_str'] = input(data['prompt'])
    while data['command'] != ReC.ReplE.EXIT:
        try:
            # parse string in REPL parser
            parse_response = ReP.parseString(data['current_str'],
                                             in_multi_line=data['in_multi_line'])
            logging.debug("Parse Response: {}".format(parse_response))
            data.update(parse_response)

            # Change prompt or perform command:
            if data['command'] is not None:
                cmd_fn = ReC.get(data['command'])
                # Perform the instruction
                engine, u_data = cmd_fn(engine, data)
                if u_data is not None:
                    data.update(u_data)

        except Exception as exp:
            logging.exception(str(exp))
            print("Error: {}".format(str(exp)))
            exc_type, exc_value, exc_tb = sys.exc_info()
            # traceback.print_tb(exc_tb, limit=4)
            breakpoint()
            data['result']  = []
        finally:
            #print result
            if bool(data['result']):
                print(data['result'])
                data['result'] = None
            # Repeat
            if data['command'] != ReC.ReplE.EXIT:
                data['current_str'] = input(data['prompt'])

    logging.info("Shutting down engine: {}".format(args.engine))
