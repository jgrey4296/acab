SHELL			:= /usr/local/bin/bash
PYS				:= $(shell find ./acab -name '*.py' -not -name '*context.py' -not -name '__init__.py')
LOGS			:= $(shell find ./acab -name '*log.*')
CACHES			:= $(shell find ./acab/ -regextype posix-egrep -regex .*\(.mypy_cache\|__pycache__\|flycheck_.+\)$)
TOP				:= ./acab
PAT				:=
FILE_PAT		:= "test_*.py"
LOGLEVEL		:= WARNING

dir				?= ./acab
doc_target		?= "html"

SPHINXOPTS		?=
SPHINXBUILD		?= sphinx-build
DOCSOURCEDIR    = docs
DOCBUILDDIR     = dist/docs

# If defined, use these overrides
ifneq (${pat}, )
	PAT = -k ${pat}
endif

ifneq (${fpat}, )
	FILE_PAT := ${fpat}
endif


.PHONY: help Makefile all pylint clean

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

# Testing #####################################################################
long:
	python -m unittest discover -s ${dir} -p "*_tests.py"

test:
	python -m unittest discover -v -s ${dir} -p ${FILE_PAT} -t ${TOP} ${PAT}

faily:
	@echo "Testing with early fail"
	python -m unittest discover -v -f -s ${dir} ${PAT} -t ${TOP} -p ${FILE_PAT}


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
	@echo "Search	= " ${dir}
	@echo "Pattern	= " ${PAT}


line_report:
	@echo "Counting Lines into linecounts.stats"
	find . -name "*.py" -not -path "./.git/*" -not -name "test_*.py" -not -name "*__init__.py" -print0 | xargs -0 wc -l | sort > linecounts.report

class_report:
	@echo "Getting Class Relations"
	find ./acab -name "*.py" -not -name "flycheck*" | xargs awk '/^class/ {print $0}' > class.report


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
