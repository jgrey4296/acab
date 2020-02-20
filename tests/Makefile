SHELL=/usr/local/bin/bash
PYS := $(shell find . -name '*.py' -not -name '*context.py' -not -name '__init__.py')
LOGS := $(shell find . -name '*log.*')
CACHES := $(shell find . -name '*__pycache__')

.PHONY: all
all: verbose

verbose:
	python -m unittest discover -p "*tests.py" -v

faily:
	python -m unittest discover -p "*tests.py" -v -f

# use as: make pattern PAT="X"
pattern:
	python -m unittest discover -p "*tests.py" -v -f -k ${PAT}

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
