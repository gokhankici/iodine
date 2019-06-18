PY_FILES = annotation.py assumptions.py benchmark.py config.py run.py utils.py testing.py minimize.py
JQ_TYPES = always_eq initial_eq

lint:
	flake8 $(PY_FILES)

$(JQ_TYPES): %:
	cat annot-last.json | jq '.annotations | map(select(.type == "$@") | .variables) | add | sort'

freeze:
	pip freeze > requirements.txt

.PHONY: lint freeze $(JQ_TYPES)
.SILENT:
