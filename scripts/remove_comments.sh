#!/bin/zsh

zparseopts -D -E -- \
	-trailing=TRAILING

if [[ $# -ne 1 ]]; then
	echo "usage $0 <filename>" >&2
	exit 1
fi

FILENAME="$1"

THIS_DIR="${0:A:h}"

which ivlpp &>/dev/null || { echo 'ivlpp not found on PATH'; exit 1 }

ivlpp $FILENAME | \
	sed -e '/^[[:space:]]*\/\//d' $FILENAME | \
	sed -r ':a; s%(.*)/\*.*\*/%\1%; ta; /\/\*/ !b; N; ba' | \
	sed '/^[[:space:]]*$/d' | \
	{ [[ -n "$TRAILING" ]] && sed -e 's/[[:space:]]*\/\/.*$//' || cat }

