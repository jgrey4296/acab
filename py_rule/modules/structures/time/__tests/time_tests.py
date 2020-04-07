import unittest
import logging
import pyparsing as pp
from py_rule.modules.structures.time.arc import Arc
from py_rule.modules.structures.time.event import Event
from py_rule.modules.structures.time.pattern import Pattern, PatternSeq, PatternPar
from py_rule.modules.structures.time.parsing import parser as tp
from py_rule.modules.structures.time.util import Time as t
from py_rule.abstract.parsing import util as PU
from py_rule.util import BIND_S


class TestTime(unittest.TestCase):

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
        an_arc = Arc(t(0,1), t(1,1))
        self.assertIsNotNone(an_arc)

    def test_arc_contains_true(self):
        an_arc = Arc(t(0,1), t(1,1))
        self.assertTrue(t(1,4) in an_arc)
        self.assertTrue(t(3,4) in an_arc)

    def test_arc_contains_false(self):
        an_arc = Arc(t(0,1), t(1,2))
        self.assertFalse(t(3,4) in an_arc)
        self.assertFalse(t(-1, 4) in an_arc)

    def test_arc_size(self):
        an_arc = Arc(t(1,4), t(1,2))
        self.assertEqual(an_arc.size(), t(1,4))

    def test_arc_equality(self):
        an_arc = Arc(t(1,4), t(2,3))
        an_arc_2 = Arc(t(1,4), t(2,3))
        self.assertEqual(an_arc, an_arc_2)

    def test_arc_equality_failure(self):
        an_arc = Arc(t(1,4), t(2,3))
        an_arc_2 = Arc(t(2,4), t(2,3))
        self.assertNotEqual(an_arc, an_arc_2)

    def test_arc_bound(self):
        an_arc = Arc(t(1,4), t(1,2))
        another_arc = Arc(t(1,3), t(2,3))
        combined = an_arc.bound(another_arc)
        combined_other = another_arc.bound(an_arc)
        self.assertEqual(combined, combined_other)
        self.assertEqual(combined._start, t(1,4))
        self.assertEqual(combined._end, t(2,3))

    def test_arc_copy(self):
        an_arc = Arc(t(1,4), t(3,5))
        copied = an_arc.copy()
        self.assertEqual(an_arc, copied)

    def test_arc_bound_invariant(self):
        with self.assertRaises(AssertionError):
            Arc(t(3,4), t(1,4))

    def test_arc_pair(self):
        an_arc = Arc(t(1,4), t(2,4))
        pair = an_arc.pair()
        self.assertEqual(pair[0], t(1,4))
        self.assertEqual(pair[1], t(2,4))


    #--------------------
    # EVENT TESTS
    def test_event_creation(self):
        anEvent = Event(Arc(t(0,1), t(1,1)), "a")
        self.assertIsNotNone(anEvent)
        self.assertEqual(anEvent._arc, Arc(t(0,1), t(1,1)))
        self.assertEqual(anEvent._event, "a")

    def test_event_creation_with_params(self):
        anEvent = Event(Arc(t(0,1), t(2,4)), "b", params={"test": 5})
        self.assertEqual(anEvent["test"], 5)

    def test_event_call(self):
        anEvent = Event(Arc(t(0,1), t(1,1)), "a")
        callResult = anEvent(t(1,2))
        self.assertEqual(len(callResult), 1)

    def test_event_call_outside_range(self):
        anEvent = Event(Arc(t(0,1), t(1,1)), "a")
        callResult = anEvent(t(2,1))
        self.assertEqual(len(callResult), 0)

    def test_event_call_pattern(self):
        aPattern = Pattern(Arc(t(0,1),t(1,1)),
                           [ Event(Arc(t(0,1),t(1,2)), "a"),
                             Event(Arc(t(1,2),t(1,1)), "b"),
                           ])
        anEvent = Event(Arc(t(0,1), t(1,1)), aPattern, True)
        callResult = anEvent(t(1,2))
        self.assertEqual(len(callResult), 1)
        self.assertEqual(callResult[0]._event, "b")

    def test_event_base(self):
        anEvent = Event(Arc(t(0,1),t(1,1)), "a")
        base = anEvent.base()
        self.assertEqual(len(base), 2)

    def test_event_base_pattern(self):
        aPattern = Pattern(Arc(t(0,1),t(1,1)),
                                [ Event(Arc(t(1,4),t(1,2)), "a"),
                                  Event(Arc(t(1,6),t(3,8)), "b") ])
        anEvent = Event(Arc(t(0,1),t(1,1)), aPattern, True)
        base = anEvent.base()
        self.assertEqual(len(base), 6)

    def test_event_get_key(self):
        anEvent = Event(Arc(t(0,1),t(1,1)), "a")
        key = anEvent.key()
        self.assertEqual(key, t(0,1))

    def test_event_sort_by_key(self):
        events = [ Event(Arc(t(1,2),t(1,1)), "a"),
                   Event(Arc(t(0,1),t(1,2)), "b"),
                   Event(Arc(t(2,1),t(3,1)), "c")]
        sorted_events = sorted(events, key=lambda x: x.key())
        values = [x._event for x in sorted_events]
        self.assertEqual(values, ["b","a","c"])

    def test_event_contains(self):
        anEvent = Event(Arc(t(0,1),t(1,2)), "a")
        self.assertTrue(t(1,4) in anEvent)
        self.assertFalse(t(3,4) in anEvent)

    def test_event_set_arc(self):
        an_event = Event(Arc(t(1,2), t(3,4)), "a")
        self.assertEqual(an_event._arc, Arc(t(1,2), t(3,4)))
        an_event.set_arc(Arc(t(1,1), t(5,4)))
        self.assertEqual(an_event._arc, Arc(t(1,1), t(5,4)))

    def test_event_is_pure(self):
        an_event = Event(Arc(t(1,2), t(3,4)), "a")
        self.assertTrue(an_event.is_pure())

    def test_event_is_pure_fail(self):
        aPattern = Pattern(Arc(t(0,1),t(1,1)),
                           [ Event(Arc(t(1,4),t(1,2)), "a"),
                             Event(Arc(t(1,6),t(3,8)), "b") ])
        anEvent = Event(Arc(t(0,1),t(1,1)), aPattern, True)
        self.assertFalse(anEvent.is_pure())

    def test_event_binding(self):
        an_event = Event(Arc(t(1,2), t(3,4)),
                         "a", False, {BIND_S : True})

        self.assertEqual(an_event._event, "a")
        bound = an_event.bind({"a" : "b"})
        self.assertEqual(bound._event, "b")

    def test_event_binding_fail(self):
        an_event = Event(Arc(t(1,2), t(3,4)),
                         "a", False, {BIND_S : True})

        self.assertEqual(an_event._event, "a")
        bound = an_event.bind({"c" : "b"})
        self.assertEqual(bound._event, "a")

    def test_event_binding_fail_non_var(self):
        an_event = Event(Arc(t(1,2), t(3,4)),
                         "a", False, {BIND_S : False})

        self.assertEqual(an_event._event, "a")
        bound = an_event.bind({"a" : "b"})
        self.assertEqual(bound._event, "a")

    def test_event_fail_var_pattern(self):
        with self.assertRaises(AssertionError):
            an_event = Event(Arc(t(1,2), t(3,4)),
                             Pattern(Arc(t(0,1),t(1,1)),
                                     [ Event(Arc(t(0,1),t(1,2)), "a"),
                                       Event(Arc(t(1,2),t(1,1)), "b"),
                                     ]),
                             True, {BIND_S : True})

    #--------------------
    # PATTERN TESTS
    def test_pattern_creation(self):
        aPattern = Pattern(Arc(t(0,1), t(1,1)), [])
        self.assertIsNotNone(aPattern)

    def test_pattern_call_empty(self):
        aPattern = Pattern(Arc(t(0,1),t(1,1)), [])
        self.assertEqual(len(aPattern(t(0,1))), 0)

    def test_pattern_call(self):
        aPattern = Pattern(Arc(t(0,1), t(1,1)),
                                [ Event(Arc(t(0,1),t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)), "b")])
        res = aPattern(t(1,2))
        self.assertEqual(len(res), 1)
        self.assertEqual(res[0]._event, "b")

    def test_pattern_call_with_internal_pattern_start(self):
        aPattern = Pattern(Arc(t(0,1), t(1,2)),
                                [ Event(Arc(t(0,1), t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)), "b") ])

        mainPattern = Pattern(Arc(t(0,1), t(1,1)),
                                   [ Event(Arc(t(0,1), t(1,1)), "c"),
                                     aPattern ])

        result = mainPattern(t(0,1), True)
        self.assertEqual(len(result), 2)
        self.assertTrue("a" in result)
        self.assertTrue("c" in result)
        self.assertFalse("b" in result)

    def test_pattern_call_with_internal_pattern_scale(self):
        aPattern = Pattern(Arc(t(0,1), t(1,2)),
                                [ Event(Arc(t(0,1), t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)), "b") ])

        mainPattern = Pattern(Arc(t(0,1), t(1,1)),
                                   [ Event(Arc(t(0,1), t(1,1)), "c"),
                                     aPattern ])

        #checks the internal pattern is scaled appropriately:
        result = mainPattern(t(1,4), True)
        self.assertEqual(len(result), 2)
        self.assertFalse("a" in result)
        self.assertTrue("b" in result)
        self.assertTrue("c" in result)

    def test_pattern_call_with_internal_pattern_end(self):
        aPattern = Pattern(Arc(t(0,1), t(1,2)),
                                [ Event(Arc(t(0,1), t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)), "b") ])

        mainPattern = Pattern(Arc(t(0,1), t(1,1)),
                                   [ Event(Arc(t(0,1), t(1,1)), "c"),
                                     aPattern ])

        #checks the internal pattern ends appropriately:
        result = mainPattern(t(1,2), True)
        self.assertEqual(len(result), 1)
        self.assertTrue("c" in result)
        self.assertFalse("a" in result)
        self.assertFalse("b" in result)

    def test_pattern_call_with_patterns_in_events(self):
        aPattern = Pattern(Arc(t(0,1), t(1,1)),
                                [ Event(Arc(t(0,1), t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)),  "b") ])

        mainPattern = Pattern(Arc(t(0,1), t(1,1)),
                                   [ Event(Arc(t(0,1), t(1,1)), "c"),
                                     Event(Arc(t(1,4), t(3,4)),
                                           aPattern, True) ])

        #checks the internal pattern ends appropriately:
        result = mainPattern(t(0,2), True)
        self.assertTrue("c" in result)
        self.assertFalse("a" in result)
        result2 = mainPattern(t(1,4), True)
        self.assertTrue("c" in result2)
        self.assertTrue("a" in result2)
        result3 = mainPattern(t(1,2), True)
        self.assertTrue("c" in result3)
        self.assertFalse("a" in result3)
        self.assertTrue("b" in result3)
        result4 = mainPattern(t(3,4), True)
        self.assertTrue("c" in result4)
        self.assertFalse("a" in result4)
        self.assertFalse("b" in result4)

    def test_pattern_get_key(self):
        aPattern = Pattern(Arc(t(3,8),t(6,8)),
                           [ Event(Arc(t(0,1),t(1,1)), "a")])

        self.assertEqual(aPattern.key(), t(3,8))

    def test_pattern_contains(self):
        aPattern = Pattern(Arc(t(3,8),t(6,8)),
                                [ Event(Arc(t(0,1),t(1,1)), "a")])

        self.assertTrue(t(4,8) in aPattern)
        self.assertFalse(t(7,8) in aPattern)

    def test_pattern_denominator_simple(self):
        aPattern = tp.parse_string("[[ a b c ]]")
        denom = aPattern.denominator()
        self.assertEqual(denom, 3)

    def test_pattern_denominator_simple_2(self):
        aPattern = tp.parse_string("[[a b c d]]")
        denom = aPattern.denominator()
        self.assertEqual(denom, 4)

    def test_pattern_denominator_nested(self):
        aPattern = tp.parse_string("[[a b [c d]]]")
        denom = aPattern.denominator()
        self.assertEqual(denom, 6)

    def test_pattern_denominator_parallel(self):
        aPattern = tp.parse_string("[[a b, c d]]")
        denom = aPattern.denominator()
        self.assertEqual(denom, 2)

    def test_pattern_seq__double_cycle(self):
        p1 = tp.parse_string("[[a b c d]]")
        p2 = tp.parse_string("[[e f g h]]")
        p3 = PatternSeq(Arc(t(0,1),t(2,1)),
                             [ p1,p2 ])

        self.assertEqual(p3(t(0,1), True)[0], "a")
        # self.assertEqual(p3(t(1,1), True)[0], "e")
        self.assertEqual(p3(t(7,4), True)[0], "h")

    def test_pattern_iterator(self):
        aPattern = tp.parse_string("[[a b [c d] e]]")
        for i,x in zip([0,1,2,3,4], aPattern.iter()):
            self.assertEqual(x[0], ["a","a",
                                    "b", "b",
                                    "c", "d",
                                    "e", "e"][i])

    def test_pattern_iterator_loop(self):
        aPattern = tp.parse_string("[[ a b ]]")
        end = False
        for i,x in zip(range(8), aPattern.iter(cycles=None)):
            if i == 7:
                end = True
            self.assertEqual(x[0], (["a", "b"] * 4)[i])
        self.assertTrue(end)

    def test_pattern_iterator_events(self):
        aPattern = tp.parse_string("[[ a b c ]]")
        for i,x in zip(range(4), aPattern.iter(False)):
            self.assertIsInstance(x[0], Event)

    def test_pattern_add(self):
        first_pattern = tp.parse_string("[[a b]]")
        second_pattern = tp.parse_string("[[c d]]")
        combined = first_pattern + second_pattern
        for i,x in zip([0, 1, 2, 3, 4, 5],
                       combined.iter()):
            self.assertEqual(x[0],
                            "a b c d a b".split(" ")[i])

    def test_pattern_add_twice(self):
        first_pattern = tp.parse_string("[[a b]]")
        second_pattern = tp.parse_string("[[c d]]")
        third_pattern = tp.parse_string("[[e f]]")
        combined = first_pattern + second_pattern
        combined_2 = combined + third_pattern
        for i,x in zip([0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                       combined_2.iter()):
            self.assertEqual(x[0],
                            "a b c d e f a b c d e f".split(" ")[i])

    def test_pattern_stack(self):
        first_pattern = tp.parse_string("[[a b]]")
        second_pattern = tp.parse_string("[[c d]]")
        combined = first_pattern * second_pattern
        for i,x in zip([0, 1, 2, 3],
                       combined.iter()):
            self.assertEqual(x[0],
                             "a b a b".split(" ")[i])
            self.assertEqual(x[1],
                             "c d c d".split(" ")[i])


    def test_pattern_var_set_basic(self):
        result = tp.parse_string("[[a $b c $d]]")
        self.assertEqual(result.var_set(), set(["b", "d"]))

    def test_pattern_var_set_nested(self):
        pattern = tp.parse_string("[[a $b <$c d>, $e f [g $h]]]")
        self.assertEqual(pattern.var_set(), set(["b", "c", "e", "h"]))

    def test_pattern_empty_var_set(self):
        pattern = tp.parse_string("[[a b <c d>, e f [g h]]]")
        self.assertEqual(pattern.var_set(), set())

    def test_pattern_bind(self):
        pattern = tp.parse_string("[[a $b c $b]]")
        bound = pattern.bind({"a": "e", "b": "g"})

        for x,y in zip(bound.iter(cycles=2), ["a","g","c","g"]*2):
            self.assertEqual(x[0],y)

    def test_pattern_iterator_count(self):
        pattern = tp.parse_string("[[a b c d]]")
        count = 0
        for x,y in zip(pattern.iter(count=3), ["a","b","c","d","a"]):
            count += 1
            self.assertEqual(x[0], y)

        self.assertEqual(count, 3)

    def test_pattern_iterator_cycle(self):
        pattern = tp.parse_string("[[a b c d]]")
        count = 0
        for x,y in zip(pattern.iter(cycles=2), ["a","b","c","d"]*3):
            count += 1
            self.assertEqual(x[0], y)

        self.assertEqual(count, 8)

    def test_pattern_iterator_implicit(self):
        pattern = tp.parse_string("[[a b c d]]")
        count = 0
        for x,y in zip(pattern, ["a","b","c","d"]*3):
            count += 1
            self.assertEqual(x[0], y)

        self.assertEqual(count, 4)

    def test_pattern_choice(self):
        pattern = tp.parse_string("[<a b c d>]")
        count = 0
        for x in pattern.iter(cycles=4):
            count += 1
            self.assertTrue(x[0] in ["a","b","c","d"])
        self.assertEqual(count, 4)

    def test_pattern_choice_bind(self):
        pattern = tp.parse_string("[<$a $a $a c>]")
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
        aPattern = tp.parse_string("[[ a b c ]]")
        self.assertIsInstance(aPattern, Pattern)
        self.assertEqual(len(aPattern._components), 3)

    def test_parse_balance_failure(self):
        with self.assertRaises(Exception):
            tp.parse_string("[a b c")

    def test_parse_balance_failure_nested(self):
        with self.assertRaises(Exception):
            tp.parse_string("[[a b [c d]]")

    def test_parse_nested_simple(self):
        aPattern = tp.parse_string("[[ a b [c d]]]")
        self.assertEqual(len(aPattern._components), 3)
        self.assertIsInstance(aPattern._components[2]._event, Pattern)

    def test_parse_parallel_nested(self):
        aPattern = tp.parse_string("[[ a b , [c d] e]]")
        self.assertIsInstance(aPattern, PatternPar)
        self.assertEqual(len(aPattern._components), 2)
        self.assertEqual(len(aPattern._components[1]._components), 2)





if __name__ == "__main__":
      #use python $filename to use this logging setup
      LOGLEVEL = logging.INFO
      logFileName = "log.test_time"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.INFO)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
