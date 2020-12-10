import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)

import pyparsing as pp


from acab.abstract.config.config import AcabConfig
config = AcabConfig.Get("acab")

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.modules.structures.time.time_core import BaseTime, TimeEvent, TimeContainer
from acab.modules.structures.time.pattern import PatternSeq, PatternPar
from acab.modules.structures.time.parsing import parser as tp
from acab.modules.structures.time.config import Time as t
from acab.abstract.parsing import parsers as PU
from acab.abstract.printing.print_semantics import AcabPrintSemantics
from acab.abstract.printing import default_handlers as DH

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR}

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'OPERATOR',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})

BIND_S = config.value("Parse.Structure", "BIND")

class TestTime(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    @classmethod
    def setUpClass(cls):
        #Hotload value and bind
        tp.HOTLOAD_VALUE << PU.BASIC_VALUE
        tp.HOTLOAD_BIND << PU.BIND

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    # ARC TESTS
    def test_arc_creation(self):
        an_arc = BaseTime((t(0, 1), t(1, 1)))
        self.assertIsNotNone(an_arc)

    def test_arc_contains_true(self):
        an_arc = BaseTime((t(0, 1), t(1, 1)))
        self.assertTrue(t(1, 4) in an_arc)
        self.assertTrue(t(3, 4) in an_arc)

    def test_arc_contains_false(self):
        an_arc = BaseTime((t(0, 1), t(1, 2)))
        self.assertFalse(t(3, 4) in an_arc)
        self.assertFalse(t(-1, 4) in an_arc)

    def test_arc_size(self):
        an_arc = BaseTime((t(1, 4), t(1, 2)))
        self.assertEqual(an_arc.size(), t(1, 4))

    def test_arc_equality(self):
        an_arc = BaseTime((t(1, 4), t(2, 3)))
        an_arc_2 = BaseTime((t(1, 4), t(2, 3)))
        self.assertEqual(an_arc, an_arc_2)

    def test_arc_equality_failure(self):
        an_arc = BaseTime((t(1, 4), t(2, 3)))
        an_arc_2 = BaseTime((t(2, 4), t(2, 3)))
        self.assertNotEqual(an_arc, an_arc_2)

    def test_arc_bound(self):
        an_arc = BaseTime((t(1, 4), t(1, 2)))
        another_arc = BaseTime((t(1, 3), t(2, 3)))
        combined = an_arc.bound(another_arc)
        combined_other = another_arc.bound(an_arc)
        self.assertEqual(combined, combined_other)
        self.assertEqual(combined.start, t(1, 4))
        self.assertEqual(combined.end, t(2, 3))

    def test_arc_copy(self):
        an_arc = BaseTime((t(1, 4), t(3, 5)))
        copied = an_arc.copy()
        self.assertEqual(an_arc, copied)

    def test_arc_bound_invariant(self):
        with self.assertRaises(AssertionError):
            BaseTime((t(3, 4), t(1, 4)))

    def test_arc_pair(self):
        an_arc = BaseTime((t(1, 4), t(2, 4)))
        pair = an_arc.arc
        self.assertEqual(pair[0], t(1, 4))
        self.assertEqual(pair[1], t(2, 4))


    #--------------------
    # EVENT TESTS
    def test_event_creation(self):
        an_event = TimeEvent((t(0, 1), t(1, 1)), "a")
        self.assertIsNotNone(an_event)
        self.assertEqual(an_event.arc, (t(0, 1), t(1, 1)))
        self.assertEqual(Printer.print(an_event), "a")

    def test_event_creation_with_params(self):
        an_event = TimeEvent((t(0, 1), t(2, 4)), "b", data={"test": 5})
        self.assertEqual(an_event._data["test"], 5)

    def test_event_call(self):
        an_event = TimeEvent((t(0, 1), t(1, 1)), "a")
        call_result = an_event(t(1, 2))
        self.assertEqual(len(call_result), 1)

    def test_event_call_outside_range(self):
        an_event = TimeEvent((t(0, 1), t(1, 1)), "a")
        call_result = an_event(t(2, 1))
        self.assertEqual(len(call_result), 0)

    def test_event_call_pattern(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 1)),
                                  [TimeEvent((t(0, 1), t(1, 2)), "a"),
                                   TimeEvent((t(1, 2), t(1, 1)), "b")])
        an_event = TimeEvent((t(0, 1), t(1, 1)), a_pattern)
        call_result = an_event(t(1, 2))
        self.assertEqual(len(call_result), 1)
        self.assertEqual(Printer.print(call_result[0]), "b")

    def test_event_base(self):
        an_event = TimeEvent((t(0, 1), t(1, 1)), "a")
        base = an_event.base
        self.assertEqual(len(base), 2)

    def test_event_base_pattern(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 1)),
                                  [TimeEvent((t(1, 4), t(1, 2)), "a"),
                                   TimeEvent((t(1, 6), t(3, 8)), "b")])
        an_event = TimeEvent((t(0, 1), t(1, 1)), a_pattern)
        base = an_event.base
        self.assertEqual(len(base), 6)

    def test_event_get_key(self):
        an_event = TimeEvent((t(0, 1), t(1, 1)), "a")
        key = an_event.key
        self.assertEqual(key, t(0, 1))

    def test_event_sort_by_key(self):
        events = [TimeEvent((t(1, 2), t(1, 1)), "a"),
                  TimeEvent((t(0, 1), t(1, 2)), "b"),
                  TimeEvent((t(2, 1), t(3, 1)), "c")]
        sorted_events = sorted(events, key=lambda x: x.key)
        values = [x.event for x in sorted_events]
        self.assertEqual(values, ["b", "a", "c"])

    def test_event_contains(self):
        an_event = TimeEvent((t(0, 1), t(1, 2)), "a")
        self.assertTrue(t(1, 4) in an_event)
        self.assertFalse(t(3, 4) in an_event)

    def test_event_set_arc(self):
        an_event = TimeEvent((t(1, 2), t(3, 4)), "a")
        self.assertEqual(an_event.arc, (t(1, 2), t(3, 4)))
        an_event.set_arc((t(1, 1), t(5, 4)))
        self.assertEqual(an_event.arc, (t(1, 1), t(5, 4)))

    def test_event_is_pure(self):
        an_event = TimeEvent((t(1, 2), t(3, 4)), "a")
        self.assertTrue(an_event.is_pure())

    def test_event_is_pure_fail(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 1)),
                                  [TimeEvent((t(1, 4), t(1, 2)), "a"),
                                   TimeEvent((t(1, 6), t(3, 8)), "b")])
        an_event = TimeEvent((t(0, 1), t(1, 1)), a_pattern)
        self.assertFalse(an_event.is_pure())

    def test_event_binding(self):
        an_event = TimeEvent((t(1, 2), t(3, 4)),
                             "a", {BIND_S : True})

        self.assertEqual(Printer.print(an_event), "$a")
        bound = an_event.bind({"a" : "b"})
        self.assertIsInstance(bound, TimeEvent)
        self.assertEqual(Printer.print(bound), "b")

    def test_event_binding_fail(self):
        an_event = TimeEvent((t(1, 2), t(3, 4)),
                             "a", {BIND_S : True})

        self.assertEqual(Printer.print(an_event), "$a")
        bound = an_event.bind({"c" : "b"})
        self.assertEqual(Printer.print(bound), "$a")

    def test_event_binding_fail_non_var(self):
        an_event = TimeEvent((t(1, 2), t(3, 4)),
                             "a", {BIND_S : False})

        self.assertEqual(Printer.print(an_event), "a")
        bound = an_event.bind({"a" : "b"})
        self.assertEqual(Printer.print(bound), "a")

    def test_event_fail_var_pattern(self):
        with self.assertRaises(AssertionError):
            an_event = TimeEvent((t(1, 2), t(3, 4)),
                                 TimeContainer((t(0, 1), t(1, 1)),
                                               [TimeEvent((t(0, 1), t(1, 2)), "a"),
                                                TimeEvent((t(1, 2), t(1, 1)), "b")]),
                                 {BIND_S : True})

    #--------------------
    # PATTERN TESTS
    def test_pattern_creation(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 1)), [])
        self.assertIsNotNone(a_pattern)

    def test_pattern_call_empty(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 1)), [])
        self.assertEqual(len(a_pattern(t(0, 1))), 0)

    def test_pattern_call(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 1)),
                                  [TimeEvent((t(0, 1), t(1, 2)), "a"),
                                   TimeEvent((t(1, 2), t(1, 1)), "b")])
        res = a_pattern(t(1, 2))
        self.assertEqual(len(res), 1)
        self.assertEqual(Printer.print(res[0]), "b")

    def test_pattern_call_with_internal_pattern_start(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 2)),
                                  [TimeEvent((t(0, 1), t(1, 2)), "a"),
                                   TimeEvent((t(1, 2), t(1, 1)), "b")])

        main_pattern = TimeContainer((t(0, 1), t(1, 1)),
                                     [TimeEvent((t(0, 1), t(1, 1)), "c"),
                                      a_pattern])

        result = main_pattern(t(0, 1), True)
        self.assertEqual(len(result), 2)
        self.assertTrue("a" in result)
        self.assertTrue("c" in result)
        self.assertFalse("b" in result)

    def test_pattern_call_with_internal_pattern_scale(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 2)),
                                  [TimeEvent((t(0, 1), t(1, 2)), "a"),
                                   TimeEvent((t(1, 2), t(1, 1)), "b")])

        main_pattern = TimeContainer((t(0, 1), t(1, 1)),
                                     [TimeEvent((t(0, 1), t(1, 1)), "c"),
                                      a_pattern])

        #checks the internal pattern is scaled appropriately:
        result = main_pattern(t(1, 4), True)
        self.assertEqual(len(result), 2)
        self.assertFalse("a" in result)
        self.assertTrue("b" in result)
        self.assertTrue("c" in result)

    def test_pattern_call_with_internal_pattern_end(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 2)),
                                  [TimeEvent((t(0, 1), t(1, 2)), "a"),
                                   TimeEvent((t(1, 2), t(1, 1)), "b")])

        main_pattern = TimeContainer((t(0, 1), t(1, 1)),
                                     [TimeEvent((t(0, 1), t(1, 1)), "c"),
                                      a_pattern])

        #checks the internal pattern ends appropriately:
        result = main_pattern(t(1, 2), True)
        self.assertEqual(len(result), 1)
        self.assertTrue("c" in result)
        self.assertFalse("a" in result)
        self.assertFalse("b" in result)

    def test_pattern_call_with_patterns_in_events(self):
        a_pattern = TimeContainer((t(0, 1), t(1, 1)),
                                  [TimeEvent((t(0, 1), t(1, 2)), "a"),
                                   TimeEvent((t(1, 2), t(1, 1)), "b")])

        main_pattern = TimeContainer((t(0, 1), t(1, 1)),
                                     [TimeEvent((t(0, 1), t(1, 1)), "c"),
                                      TimeEvent((t(1, 4), t(3, 4)), a_pattern)])

        #checks the internal pattern ends appropriately:
        result = main_pattern(t(0, 2), True)
        self.assertTrue("c" in result)
        self.assertFalse("a" in result)
        result2 = main_pattern(t(1, 4), True)
        self.assertTrue("c" in result2)
        self.assertTrue("a" in result2)
        result3 = main_pattern(t(1, 2), True)
        self.assertTrue("c" in result3)
        self.assertFalse("a" in result3)
        self.assertTrue("b" in result3)
        result4 = main_pattern(t(3, 4), True)
        self.assertTrue("c" in result4)
        self.assertFalse("a" in result4)
        self.assertFalse("b" in result4)

    def test_pattern_get_key(self):
        a_pattern = TimeContainer((t(3, 8), t(6, 8)),
                                  [TimeEvent((t(0, 1), t(1, 1)), "a")])

        self.assertEqual(a_pattern.key, t(3, 8))

    def test_pattern_contains(self):
        a_pattern = TimeContainer((t(3, 8), t(6, 8)),
                                  [TimeEvent((t(0, 1), t(1, 1)), "a")])

        self.assertTrue(t(4, 8) in a_pattern)
        self.assertFalse(t(7, 8) in a_pattern)

    def test_pattern_denominator_simple(self):
        a_pattern = tp.parseString("[[a b c ]]")
        denom = a_pattern.denominator
        self.assertEqual(denom, 3)

    def test_pattern_denominator_simple_2(self):
        a_pattern = tp.parseString("[[a b c d]]")
        denom = a_pattern.denominator
        self.assertEqual(denom, 4)

    def test_pattern_denominator_nested(self):
        a_pattern = tp.parseString("[[a b [c d]]]")
        denom = a_pattern.denominator
        self.assertEqual(denom, 6)

    def test_pattern_denominator_parallel(self):
        a_pattern = tp.parseString("[[a b, c d]]")
        denom = a_pattern.denominator
        self.assertEqual(denom, 2)

    def test_pattern_seq__double_cycle(self):
        p1 = tp.parseString("[[a b c d]]")
        p2 = tp.parseString("[[e f g h]]")
        p3 = PatternSeq((t(0, 1), t(2, 1)),
                        [p1, p2])

        self.assertEqual(p3(t(0, 1), True)[0], "a")
        # self.assertEqual(p3(t(1, 1), True)[0], "e")
        self.assertEqual(p3(t(7, 4), True)[0], "h")

    def test_pattern_iterator(self):
        a_pattern = tp.parseString("[[a b [c d] e]]")
        for i, x in zip([0, 1, 2, 3, 4], a_pattern.iter()):
            self.assertEqual(x[0], ["a", "a",
                                    "b", "b",
                                    "c", "d",
                                    "e", "e"][i])

    def test_pattern_iterator_loop(self):
        a_pattern = tp.parseString("[[a b ]]")
        end = False
        for i, x in zip(range(8), a_pattern.iter(cycles=None)):
            if i == 7:
                end = True
                self.assertEqual(x[0], (["a", "b"] * 4)[i])
                self.assertTrue(end)

    def test_pattern_iterator_events(self):
        a_pattern = tp.parseString("[[a b c ]]")
        for i, x in zip(range(4), a_pattern.iter(False)):
            self.assertIsInstance(x[0], TimeEvent)

    @unittest.skip("Not implemented yet")
    def test_pattern_add(self):
        first_pattern = tp.parseString("[[a b]]")
        second_pattern = tp.parseString("[[c d]]")
        combined = first_pattern + second_pattern
        for i, x in zip([0, 1, 2, 3, 4, 5],
                        combined.iter()):
            self.assertEqual(x[0],
                             "a b c d a b".split(" ")[i])

    @unittest.skip("not implemented yet")
    def test_pattern_add_twice(self):
        first_pattern = tp.parseString("[[a b]]")
        second_pattern = tp.parseString("[[c d]]")
        third_pattern = tp.parseString("[[e f]]")
        combined = first_pattern + second_pattern
        combined_2 = combined + third_pattern
        for i, x in zip([0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                        combined_2.iter()):
            self.assertEqual(x[0],
                             "a b c d e f a b c d e f".split(" ")[i])

    @unittest.skip("not implemented yet")
    def test_pattern_stack(self):
        first_pattern = tp.parseString("[[a b]]")
        second_pattern = tp.parseString("[[c d]]")
        combined = first_pattern * second_pattern
        for i, x in zip([0, 1, 2, 3],
                        combined.iter()):
            self.assertEqual(x[0],
                             "a b a b".split(" ")[i])
            self.assertEqual(x[1],
                             "c d c d".split(" ")[i])


    def test_pattern_var_set_basic(self):
        result = tp.parseString("[[a $b c $d]]")
        var_set = result.var_set
        self.assertEqual(var_set['in'], set(["b", "d"]))

    def test_pattern_var_set_nested(self):
        pattern = tp.parseString("[[a $b <$c d>, $e f [g $h]]]")
        var_set = pattern.var_set
        self.assertEqual(var_set['in'], set(["b", "c", "e", "h"]))

    def test_pattern_empty_var_set(self):
        pattern = tp.parseString("[[a b <c d>, e f [g h]]]")
        var_set = pattern.var_set
        self.assertFalse(var_set['in'])
        self.assertFalse(var_set['out'])

    def test_pattern_bind(self):
        pattern = tp.parseString("[[a $b c $b]]")
        bound = pattern.bind({"a": "e", "b": "g"})

        for x, y in zip(bound.iter(cycles=2), ["a", "g", "c", "g"]*2):
            self.assertEqual(x[0], y)

    def test_pattern_iterator_count(self):
        pattern = tp.parseString("[[a b c d]]")
        count = 0
        for x, y in zip(pattern.iter(count=3), ["a", "b", "c", "d", "a"]):
            count += 1
            self.assertEqual(x[0], y)

        self.assertEqual(count, 3)

    def test_pattern_iterator_cycle(self):
        pattern = tp.parseString("[[a b c d]]")
        count = 0
        for x, y in zip(pattern.iter(cycles=2), ["a", "b", "c", "d"]*3):
            count += 1
            self.assertEqual(x[0], y)

        self.assertEqual(count, 8)

    def test_pattern_iterator_implicit(self):
        pattern = tp.parseString("[[a b c d]]")
        count = 0
        for x, y in zip(pattern, ["a", "b", "c", "d"]*3):
            count += 1
            self.assertEqual(x[0], y)

        self.assertEqual(count, 4)

    def test_pattern_choice(self):
        pattern = tp.parseString("[<a b c d>]")
        count = 0
        for x in pattern.iter(cycles=4):
            count += 1
            self.assertTrue(x[0] in ["a", "b", "c", "d"])

        self.assertEqual(count, 4)

    def test_pattern_choice_bind(self):
        pattern = tp.parseString("[<$a $a $a c>]")
        bound = pattern.bind({"a": "q"})
        count = 0
        for x in bound.iter(count=20):
            count += 1
            self.assertTrue(x[0] in ["q", "c"])

        self.assertEqual(count, 20)

    #TODO: test event optional
    #TODO: test silence

    #--------------------
    # PARSER TESTS
    #Parse a pattern
    def test_parse_simple(self):
        a_pattern = tp.parseString("[[a b c ]]")
        self.assertIsInstance(a_pattern, TimeContainer)
        self.assertEqual(len(a_pattern.events), 3)

    def test_parse_balance_failure(self):
        with self.assertRaises(Exception):
            tp.parseString("[a b c")

    def test_parse_balance_failure_nested(self):
        with self.assertRaises(Exception):
            tp.parseString("[[a b [c d]]")

    def test_parse_nested_simple(self):
        a_pattern = tp.parseString("[[a b [c d]]]")
        self.assertEqual(len(a_pattern.events), 3)
        self.assertIsInstance(a_pattern.events[2].event, TimeContainer)

    def test_parse_parallel_nested(self):
        a_pattern = tp.parseString("[[a b , [c d] e]]")
        self.assertIsInstance(a_pattern, PatternPar)
        self.assertEqual(len(a_pattern.events), 2)
        self.assertEqual(len(a_pattern.events[1].events), 2)


