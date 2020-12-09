#!/usr/bin/env python3
"""
Topologically sorted testing of acab

End to End doesn't work, because everything is swappable

So: topologically sorted.

Configuration[BootStrapParser, Interfaces, Trie]
-> ClosedSet[Values, Node]
-> Abstractions[Structures, Rule, WorkingMemory]
-> Semantics[ClosedSet, Abstractions]
-> Engine
-> Reduction
-> Input/Output[Actions, Printing, Parsing]
-> Modules

"""
