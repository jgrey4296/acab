"""
The Primary Implementation of PyRule. Uses a Trie for the backend of
the factbase. 
Has associated parsers for an Exclusion Logic-like DSL.
"""
from .Node import Node
from .Trie import Trie
from .TrieEngine import TrieEngine
