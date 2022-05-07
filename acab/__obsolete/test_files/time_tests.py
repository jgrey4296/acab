import unittest
import unittest.mock as mock

import logging
from os.path import join, isfile, exists, isdir
from os.path import split, splitext, expanduser, abspath
from os import listdir
import timeit

import acab
acab.setup()

from acab.core.value.sentence import Sentence
from acab.core.value.instruction import ProductionStructure
from acab.core.engine.engine import Engine

def S(*words):
    return Sentence.build(words)


def path(filename):
    """ Navigate from the file,
        not the cwd """
    return abspath(join("testfiles", filename))


class Timing_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

    def setUp(self):
        self.e = Engine(modules=["acab.modules.operators.standard_operators"])
        self.e.alias_module(S("acab", "modules", "operators", "standard", "operators"), S("S"))
        self.e.build()

    def tearDown(self):
        return 1

    #----------
    # use testcase snippet
    # mock.Mock / MagicMock
    # create_autospec
    # @patch(' ') / with patch.object(...)

    def test_time_load_file(self):
        load_time = timeit.timeit(lambda: self.e.load_file(path("multi_rule_timeit.trie")),
                                  number=50,
                                  setup="gc.enable()")

        # TODO Record this and check for regressions
        logging.warning("Load Time: {}".format(load_time))

    def test_time_query(self):
        self.e.load_file(path("multi_rule_timeit.trie"))
        query_time = timeit.timeit(lambda: self.e.query("rule.$x?"),
                                   number=1000)


        query_time_clauses = timeit.timeit(lambda: self.e.query("rule.first?, rule.second?"),
                                           number=1000,
                                           setup="gc.enable()")

        logging.warning("Query Time: {}".format(query_time))
        logging.warning("Query Time clauses: {}".format(query_time_clauses))

    def test_time_autorange(self):
        self.e.load_file(path("multi_rule_timeit.trie"))
        t = timeit.Timer(lambda: self.e.query("rule.$x?"))
        number, _ = t.autorange()

        logging.warning("Query Number for < 0.2seconds : {}".format(number))


    # TODO Test times

    # abstract classes
    # value, sentence, type_base

    # trie, node, contexts

    # rule, query,action,transform, prod_op

    # agenda, layer, pipeline

    # engine, working_memory

    # parsing

    # printing




if __name__ == "__main__":
    #run python $filename to use this logging setup
    #using python -m unittest $filename won't
    LOGLEVEL = logging.INFO
    logFileName = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
