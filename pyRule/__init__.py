"""
PyRule: A Trie based Rule Engine
Comprised of:
1) Contexts. Collections of possible bindings
2) Query. Strings of data and variables to extract information from the fact base.
3) Clause. Individual strings that combine to form queries.
4) Actions. Instructions to manipulate the fact base.
5) Transforms. Instructions to manipulate intermediate bindings.
6) Rule. The Combination queries, transforms, and actions.

"""
from .Contexts import Contexts
from .Query import Query
from .Clause import Clause
from .Rule import Rule
