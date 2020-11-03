SHELL=/usr/local/bin/bash
PYS := $(shell find ./acab -name '*.py' -not -name '*context.py' -not -name '__init__.py')
LOGS := $(shell find ./acab -name '*log.*')
CACHES := $(shell find ./acab/ -regextype posix-egrep -regex .*\(.mypy_cache\|__pycache__\)$ -prune)

.PHONY: all pylint

all: verbose long

long:
	python -m unittest discover -s ./acab -p "*_tests.py"

verbose:
	python -m unittest discover -s ./acab -p "test_*.py" -v

faily:
ifeq (${only}, )
	python -m unittest discover  -v -f -s ./acab -p "test_*.py"
else
	python -m unittest discover  -v -f -k ${only} -s ./acab -p "test_*.py"
endif

# use as: make pattern PAT="X"
pattern:
	python -m unittest discover  -v -f -k ${PAT} -s ./acab -p "test_*.py"

# make init py's as necessary
init:
	find ./acab -type d -print0 | xargs -0 -I {} touch "{}/__init__.py"

repl:
	python acab/repl/repl.py --config ./acab

vrepl:
	python acab/repl/repl.py --verbose DEBUG

count:
	find . -name "*.py" -not -path "./.git/*" -not -name "test_*.py" -not -name "*__init__.py" -print0 | xargs -0 wc -l | sort > linecounts.stats

re: repl
vr: vrepl

pylint:
	pylint --rcfile=./.pylintrc ./acab --ignore=${ig} --ignore-patterns=${igpat}

elint:
	pylint --rcfile=./.pylintrc ./acab --ignore=${ig} --ignore-patterns=${igpat} -E


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
