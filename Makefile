SHELL=/usr/local/bin/bash
PYS := $(shell find ./py_rule -name '*.py' -not -name '*context.py' -not -name '__init__.py')
LOGS := $(shell find ./py_rule -name '*log.*')
CACHES := $(shell find ./py_rule -name '*__pycache__')

.PHONY: all
all: verbose

test:
	python -m unittest discover -s ./py_rule -p "*_tests.py"

verbose:
	python -m unittest discover -s ./py_rule -p "*_tests.py" -v

faily:
	python -m unittest discover -s ./py_rule -p "*_tests.py" -v -f

# use as: make pattern PAT="X"
pattern:
	python -m unittest discover -s ./py_rule -p "*_tests.py" -v -f -k ${PAT}

# make init py's as necessary
init:
	find ./py_rule -type d -print0 | xargs -0 -I {} touch "{}/__init__.py"

repl:
	python py_rule/repl/repl.py

vrepl:
	python py_rule/repl/repl.py --verbose DEBUG

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
