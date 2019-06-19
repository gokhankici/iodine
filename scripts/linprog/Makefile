PY_FILES = $(wildcard *.py)
JQ_TYPES = always_eq initial_eq

lint:
	flake8 $(PY_FILES)

$(JQ_TYPES): %:
	cat annot-last.json | jq '.annotations | map(select(.type == "$@") | .variables) | add | sort'

pairs:
	cat annot-last.json | jq '.qualifiers | map(select(.type == "pairs") | .variables) | .[]'

freeze:
	pip freeze > requirements.txt

.PHONY: lint freeze $(JQ_TYPES)
.SILENT:
