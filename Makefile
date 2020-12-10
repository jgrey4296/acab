SHELL   := /usr/local/bin/bash
PYS		:= $(shell find ./acab -name '*.py' -not -name '*context.py' -not -name '__init__.py')
LOGS	:= $(shell find ./acab -name '*log.*')
CACHES	:= $(shell find ./acab/ -regextype posix-egrep -regex .*\(.mypy_cache\|__pycache__\)$ -prune)
TOP     := ./acab
START   := ./acab
PAT     :=
FILE_PAT := "test_*.py"

ifneq (${dir}, )
	START := ${dir}
endif

ifneq (${pat}, )
	PAT = -k ${pat}
endif

ifneq (${fpat}, )
	FILE_PAT := ${fpat}
endif


.PHONY: all pylint

top:
	# Topological Sort test
	python -m unittest discover -v -f -s ./acab -k Topological -t ./acab -p "test_*.py"

all: verbose long

check:
	@echo "Shell	= " ${SHELL}
	@echo "Top		= " ${TOP}
	@echo "Search	= " ${START}
	@echo "Pattern	= " ${PAT}

long:
	python -m unittest discover -s ${START} -p "*_tests.py"

test:
	python -m unittest discover -v -s ${START} -p ${FILE_PAT} -t ${TOP} ${PAT} -v

faily:
	python -m unittest discover -v -f -s ${START} ${PAT} -t ${TOP} -p ${FILE_PAT}


# make init py's as necessary
init:
	find ${TOP} -type d -print0 | xargs -0 -I {} touch "{}/__init__.py"

repl:
	python acab/modules/repl/repl.py --config ./acab

vrepl:
	python acab/modules/repl/repl.py --verbose DEBUG

count:
	find . -name "*.py" -not -path "./.git/*" -not -name "test_*.py" -not -name "*__init__.py" -print0 | xargs -0 wc -l | sort > linecounts.stats

re: repl
vr: vrepl

pylint:
	pylint --rcfile=./.pylintrc ${TOP} --ignore=${ig} --ignore-patterns=${igpat}

elint:
	pylint --rcfile=./.pylintrc ${TOP} --ignore=${ig} --ignore-patterns=${igpat} -E


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
