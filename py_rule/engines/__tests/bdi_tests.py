import unittest
import logging
import py_rule.util as util
from py_rule.engines import bdi_engine as bdi
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os.path import abspath, split
from os import listdir


@unittest.skip("Broken")
class BDI_TESTS(unittest.TestCase):

    def path(self, filename):
        return abspath(join(split(__file__)[0],
                            '..',
                            'testfiles',
                            'bdi',
                            filename))

    def setUpAgent(self, files):
        self.e = bdi.Agent("testAgent",
                           [self.path(x) for x in files])

    #----------
    def test_init(self):
        self.setUpAgent(["initial_load_test.trie"])
        self.assertIsNotNone(self.e)

    @unittest.skip("Broken")
    def test_load(self):
        self.setUpAgent(["initial_load_test.trie"])
        self.assertEqual(self.e.num_rules(), 3)
        self.e.run()
        self.assertTrue(self.e._engine.query("count!$x(> 9)?"))

    @unittest.skip("Broken")
    def test_responsive(self):
        self.setUpAgent(["responsive_test.trie"])
        self.e.run()



    #BDI architecture to test:
    #1) addition / retraction of beliefs
    #2) addition / retraction of rules
    #3) addition / retraction of desires
    #4) generation / retraction of intentions
    #5) addition of actions
    #6) selection of actions by intention
    #7) firing of actions
    #8) updating of beliefs from actions
    #9) Logic Cycle


if __name__ == "__main__":
      #use python $filename to use this logging setup
# Setup root_logger:
    LOGLEVEL = root_logger.DEBUG
    LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
    root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

    console = root_logger.StreamHandler()
    console.setLevel(root_logger.INFO)
    root_logger.getLogger('').addHandler(console)
    logging = root_logger.getLogger(__name__)

    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
