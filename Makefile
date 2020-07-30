SHELL=/usr/local/bin/bash
PYS := $(shell find ./acab -name '*.py' -not -name '*context.py' -not -name '__init__.py')
LOGS := $(shell find ./acab -name '*log.*')
CACHES := $(shell find ./acab -name '*__pycache__')

.PHONY: all
all: verbose

test:
	python -m unittest discover -s ./acab -p "test_*.py"

verbose:
	python -m unittest discover -s ./acab -p "test_*.py" -v

faily:
	python -m unittest discover -s ./acab -p "test_*.py" -v -f

# use as: make pattern PAT="X"
pattern:
	python -m unittest discover -s ./acab -p "test_*.py" -v -f -k ${PAT}

# make init py's as necessary
init:
	find ./acab -type d -print0 | xargs -0 -I {} touch "{}/__init__.py"

repl:
	python acab/repl/repl.py

vrepl:
	python acab/repl/repl.py --verbose DEBUG

count:
	find . -name "*.py" -not -path "./.git/*" -not -name "test_*.py" -not -name "*__init__.py" -print0 | xargs -0 wc -l | sort > linecounts.stats

re: repl
vr: vrepl

clean:
ifeq (${LOGS}, )
	@echo "No Logs to delete"
else
	-rm ${LOGS}
endif
ifeq (${CACHES}, )
	@echo "No Caches to delete"
else
	-rm -r ${CACHES}
endif
