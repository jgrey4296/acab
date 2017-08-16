all: wme trie parser engine


wme:
	python -m unittest wme_tests.py -v

trie:
	python -m unittest trie_fb_tests.py -v

parser:
	python -m unittest *_parser_tests.py -v

engine:
	python -m unittest Engine_Tests.py -v

clean:
	-rm *.log
