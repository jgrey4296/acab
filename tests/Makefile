all: trie parser engine

trie:
	python -m unittest trie_fb_tests.py -v

parser:
	python -m unittest *_parser_tests.py -v

engine:
	python -m unittest engine*.py -v

clean:
	-rm *.log
