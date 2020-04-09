"""
A PyRule REPL, using default of TrieWM
"""
# Setup root_logger:
from os.path import splitext, split
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


if __name__ == "__main__":
    logging.info("Setting up TrieEngine")
    engine = TrieEngine()

    current_str = input("PyRuleREPL: ")
    while current_str != "exit":

        # TODO: assertions / retractions
        # TODO: run rule
        # TODO: run layer
        # TODO: run pipeline
        # TODO: print state
        # TODO: query state
        # TODO: load module
        # TODO: save state
        # TODO: load state

        current_str = input("PyRuleREPL: ")

    logging.info("Shutting down TrieEngine")
