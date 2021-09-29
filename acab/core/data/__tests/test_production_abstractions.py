#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import unittest.mock as mock
import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab import setup
config = setup()

from acab.core.data.values import Sentence
from acab.core.data.values import AcabValue, AcabStatement

from acab.core.data import production_abstractions as PO

BIND_S               = config.prepare("Value.Structure", "BIND")()
OPERATOR_TYPE_PRIM_S = config.prepare("Type.Primitive", "OPERATOR")()

class ProductionAbstractionTests(unittest.TestCase):
    """ Test the construction of production abstractions """

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    #----------
    def test_init_operator(self):
        """ Check an operator can be created, and is of the correct type """
        op = PO.ProductionOperator()
        self.assertIsInstance(op, PO.ProductionOperator)
        # TODO OPERATOR, COMPONENT
        self.assertEqual(op.type, Sentence.build([OPERATOR_TYPE_PRIM_S]))

    def test_component_init(self):
        """ Check a component can be created """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]))
        self.assertIsInstance(val, PO.ProductionComponent)
        self.assertIsInstance(val, AcabValue)

    def test_component_with_params(self):
        """ Check a component can be created with parameters """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]), params=[AcabValue("a"), AcabValue("b")])
        self.assertIsInstance(val, PO.ProductionComponent)
        self.assertEqual(len(val.params), 2)

    def test_component_with_str_params(self):
        """ Check a component can be created with string parameters """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]), params=["a", "b"])
        self.assertIsInstance(val, PO.ProductionComponent)
        self.assertEqual(len(val.params), 2)

    @unittest.skip
    def test_container_init(self):
        pass

    @unittest.skip
    def test_structure_init(self):
        pass

    def test_component_op(self):
        """ Test a components operator can be recovered """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]))
        self.assertEqual(val.op, Sentence.build(["testop"]))


    def test_apply_parameters(self):
        """ Test a component can have parameters applied to it, creating a new component """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]))
        self.assertEqual(len(val.params), 0)
        copied = val.apply_params(["a","test"])
        self.assertNotEqual(val, copied)
        self.assertEqual(len(val.params), 0)
        self.assertEqual(len(copied.params), 2)
        self.assertNotEqual(val.uuid, copied.uuid)
