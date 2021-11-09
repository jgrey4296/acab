#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html
import logging
import logging as root_logger
import unittest
import unittest.mock as mock
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
import timeit
logging = root_logger.getLogger(__name__)

import acab

config = acab.setup()

from acab.modules.engines.configured import exlo

initial_modules = config.prepare("Module.REPL", "MODULES")().split("\n")

class FileParseTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        root_logger.getLogger('').setLevel(root_logger.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = root_logger.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(root_logger.DEBUG)

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.WARNING)

        logging = root_logger.getLogger(__name__)
        logging.setLevel(root_logger.DEBUG)
        logging.addHandler(console)
        logging.addHandler(file_h)

    #----------
    def test_file_parsing_times(self):
        test_loc = join(split(__file__)[0], "test_files")
        test_files = [join(test_loc, x) for x in listdir(test_loc)
                      if isfile(join(test_loc, x)) and splitext(x)[1] == ".rule"]

        for file_name in test_files:
            print(f"\nTrying: {file_name}")
            t = timeit.timeit(f"engine.load_file('{file_name}')",
                              setup="""engine = exlo()\nengine.load_modules(*initial_modules)""",
                              number=1,
                              globals=globals())
            with open(file_name) as f:
                lines = len(f.readlines())

            print("Length: {}".format(lines))
            print("Time  : {}".format(t))