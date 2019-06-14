PY_FILES = annotation.py assumptions.py benchmark.py config.py run utils.py testing.py

all: check

check:
	flake8 $(PY_FILES)

freeze:
	pip freeze > requirements.txt

.PHONY: check mips
