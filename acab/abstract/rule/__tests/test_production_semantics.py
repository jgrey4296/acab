#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.abstract.config.config import AcabConfig
AcabConfig.Get("acab")

#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split
import logging as root_logger

import unittest
import unittest.mock as mock

import logging


class ProductionSemanticTests(unittest.TestCase):

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


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    # use testcase snippet
    # mock.Mock / MagicMock
    # create_autospec
    # @patch(' ') / with patch.object(...)


    @unittest.skip("TODO")
    def test_component_call(self):
        engine_mock = mock.Mock()
        engine_mock.get_operator = mock.Mock(return_value=lambda x, y, data=None, engine=None: x + y)

        test_sen = Sentence.build(["testop"])
        val = PO.ProductionComponent(test_sen, [AcabValue("a"), AcabValue("b")])
        result = val({}, engine_mock)

        engine_mock.get_operator.assert_called_with(test_sen)
        self.assertEqual(result, "ab")


    @unittest.skip("TODO")
    def test_get_params_empty(self):
        """
        Component should take a set of bindings, and return its params with those bindings accounted for.
        So {x:2} on a Component(param="$x") -> [2]
        """
        val = PO.ProductionComponent(Sentence.build(["testop"]), [])
        params = val.get_params({})
        self.assertIsInstance(params, list)
        self.assertFalse(bool(params))


    @unittest.skip("TODO")
    def test_get_params_basic(self):
        a_var = AcabValue("test")
        a_var.set_data({BIND_S : True})

        val = PO.ProductionComponent(Sentence.build(["testop"]), [a_var])
        params = val.get_params({'test' : AcabValue("blah")})
        self.assertTrue(bool(params))
        self.assertEqual(params[0], "blah")

    @unittest.skip("TODO")
    def test_get_params_missing_binding(self):
        a_var = AcabValue("test")
        a_var.set_data({BIND_S : True})

        val = PO.ProductionComponent(Sentence.build(["testop"]), [a_var])
        with self.assertRaises(AssertionError):
            val.get_params({'blah' : AcabValue("blah")})


    @unittest.skip("TODO")
    def test_get_params_sentence(self):
        pass

    @unittest.skip("TODO")
    def test_get_params_list(self):
        pass

    @unittest.skip("TODO")
    def test_get_params_at_bind(self):
        pass

    @unittest.skip("TODO")
    def test_container_call(self):
        pass

    @unittest.skip("TODO")
    def test_get_vars(self):
        pass

    @unittest.skip("TODO")
    def test_container_to_sentences(self):
        pass


    # TODO container var set
    # TODO container verify
