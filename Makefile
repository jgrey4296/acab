SHELL			:= /usr/local/bin/bash
TOP				:= ./acab
LOGLEVEL		:= WARNING

# Testing variables:
TEST_TARGET		?= ${TOP}
TEST_PAT		:=
TESTDIRS        := core modules/parsing modules/context modules/structures/trie modules/semantics modules/printing modules/repl modules/analysis/typing/unify modules/operators/dfs
TEST_FILE_PAT	:= "test_*.py"

# Clean variables:
PYS				!= find ${TOP} -name '*.py' -not -name '*context.py' -not -name '__init__.py'
LOGS			!= find ${TOP} -name '*log.*'
CACHES			!= find ${TOP} -regextype posix-egrep -regex .*\(.mypy_cache\|__pycache__\|flycheck_.+\)$)

# Documentation variables:
doc_target		?= "html"
SPHINXOPTS		?=
SPHINXBUILD		?= sphinx-build
DOCSOURCEDIR    = docs
DOCBUILDDIR     = dist/docs


# If defined, use these overrides
ifneq (${pat}, )
	TEST_PAT = -k ${pat}
endif

ifneq (${fpat}, )
	TEST_FILE_PAT := ${fpat}
endif


.PHONY: help Makefile all pylint clean browse long test dtest faily repl vrepl repld check

# Documentation ###############################################################
# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(DOCSOURCEDIR)" "$(DOCBUILDDIR)" $(SPHINXOPTS) $(O)
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
# run with `make sphinx doc_target=html` =clean etc

sphinx: Makefile
	@$(SPHINXBUILD) -M ${doc_target} "$(DOCSOURCEDIR)" "$(DOCBUILDDIR)" $(SPHINXOPTS) $(O)

browse:
	open "$(DOCBUILDDIR)/html/index.html"

docs: sphinx browse


# Rest ########################################################################
all: verbose long

# Building ####################################################################
build:
	python -m build
	pip install -e .

freeze:
	bash -ic "conda list --export > ./conda_env.txt"
	pip list --format=freeze > ./requirements.txt

# Testing #####################################################################
long:
	python -m unittest discover -s ${TEST_TARGET} -p "*_tests.py"

test:
	python -m unittest discover -v -s ${TEST_TARGET} -p ${TEST_FILE_PAT} -t ${TOP} ${TEST_PAT}

dtest: ${TESTDIRS}
	@echo "Tested: "
	@for entry in ${TESTDIRS}; do echo $$entry ; done



$(TESTDIRS):
	@echo "Target: ${TOP}/$@"
	python -m unittest discover -v -s "${TOP}/$@" -p ${TEST_FILE_PAT} -t ${TOP}


faily:
	@echo "Testing with early fail"
	python -m unittest discover -v -f -s ${TEST_TARGET} ${TEST_PAT} -t ${TOP} -p ${TEST_FILE_PAT}



# Repls #######################################################################

repl:
    # Standard REPL
	python acab/modules/repl/repl_main.py --config ./acab/__configs/default -v ${LOGLEVEL}

vrepl:
    # Verbose REPL
	python acab/modules/repl/repl_main.py --config ./acab/__configs/default -v DEBUG

repld:
    # python dev mode REPL
	python -X dev acab/modules/repl/repl_main.py --config ./acab/__configs/default

re: repl
vr: vrepl

# Reports #####################################################################
check:
	@echo "Shell	= " ${SHELL}
	@echo "Top		= " ${TOP}
	@echo "Search	= " ${TEST_TARGET}
	@echo "Pattern	= " ${TEST_PAT}


line_report:
	@echo "Counting Lines into linecounts.stats"
	find ${TOP} -name "*.py" -not -name "test_*.py" -not -name "*__init__.py" -print0 | xargs -0 wc -l | sort > linecounts.report

class_report:
	@echo "Getting Class Relations"
	find ${TOP} -name "*.py" -not -name "flycheck*" | xargs awk '/^class/ {print $0}' > class.report


export_env:
	conda env export --from-history > acab.yaml

# Linting #####################################################################
pylint:
	@echo "Linting"
	pylint --rcfile=./.pylintrc ${TOP} --ignore=${ig} --ignore-patterns=${igpat}

elint:
	@echo "Linting -E"
	pylint --rcfile=./.pylintrc ${TOP} --ignore=${ig} --ignore-patterns=${igpat} -E

# Cleaning ####################################################################
init:
	@echo "Auto-creating empty __init__.py's"
	find ${TOP} -type d -print0 | xargs -0 -I {} touch "{}/__init__.py"

clean:
	@echo "Cleaning"
	@$(SPHINXBUILD) -M clean "$(DOCSOURCEDIR)" "$(DOCBUILDDIR)" $(SPHINXOPTS) $(O)
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
	-rm -rf dist
