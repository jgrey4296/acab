SHELL   := /usr/local/bin/bash
PYS		:= $(shell find ./acab -name '*.py' -not -name '*context.py' -not -name '__init__.py')
LOGS	:= $(shell find ./acab -name '*log.*')
CACHES	:= $(shell find ./acab/ -regextype posix-egrep -regex .*\(.mypy_cache\|__pycache__\)$)
TOP     := ./acab
START   := ./acab
PAT     :=
FILE_PAT := "test_*.py"

# If defined, use these overrides
ifneq (${dir}, )
	START := ${dir}
endif

ifneq (${pat}, )
	PAT = -k ${pat}
endif

ifneq (${fpat}, )
	FILE_PAT := ${fpat}
endif

.PHONY: all pylint clean

## Temp targets:
sem:
	python -m unittest /Volumes/documents/github/acab/acab/__tests/semantics/*.py -v


## Topological Sort test
top:
	python -m unittest discover -v -f -s ./acab/__tests/topological_sort/ -k Topological -t ./acab -p "test_*.py"

## --

all: verbose long

check:
	@echo "Shell	= " ${SHELL}
	@echo "Top		= " ${TOP}
	@echo "Search	= " ${START}
	@echo "Pattern	= " ${PAT}

long:
	python -m unittest discover -s ${START} -p "*_tests.py"

test:
	python -m unittest discover -v -s ${START} -p ${FILE_PAT} -t ${TOP} ${PAT}

faily:
	@echo "Testing with early fail"
	python -m unittest discover -v -f -s ${START} ${PAT} -t ${TOP} -p ${FILE_PAT}


init:
	@echo "Auto-creating empty __init__.py's"
	find ${TOP} -type d -print0 | xargs -0 -I {} touch "{}/__init__.py"

repl:
	python acab/modules/repl/repl.py --config ./acab

vrepl:
	python acab/modules/repl/repl.py --verbose DEBUG

re: repl
vr: vrepl

line_report:
	@echo "Counting Lines into linecounts.stats"
	find . -name "*.py" -not -path "./.git/*" -not -name "test_*.py" -not -name "*__init__.py" -print0 | xargs -0 wc -l | sort > linecounts.report

class_report:
	@echo "Getting Class Relations"
	find ./acab -name "*.py" -not -name "flycheck*" | xargs awk '/^class/ {print $0}' > class.report

pylint:
	@echo "Linting"
	pylint --rcfile=./.pylintrc ${TOP} --ignore=${ig} --ignore-patterns=${igpat}

elint:
	@echo "Linting -E"
	pylint --rcfile=./.pylintrc ${TOP} --ignore=${ig} --ignore-patterns=${igpat} -E


clean:
	@echo "Cleaning"
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
