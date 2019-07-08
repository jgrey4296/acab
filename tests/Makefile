all: trie facttrie engine datastructures

datastructures:
	@echo "Testing Data Structures"
	python -m unittest data_structures/*_tests.py

trie:
	@echo "Testing Trie"
	python -m unittest trie/*_tests.py

facttrie:
	@echo "Testing Fact Trie"
	python -m unittest fact_trie/*_tests.py

engine:
	@echo "Testing Engine"
	python -m unittest engine/*_tests.py

typing:
	@echo "Testing Typing"
	python -m unittest typing/*_tests.py

clean:
	-rm *.log
