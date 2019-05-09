#!/bin/zsh

zparseopts -D -E -- \
	-trailing=TRAILING

if [[ $# -ne 1 ]]; then
	echo "usage $0 <filename>" >&2
	exit 1
fi

FILENAME="$1"

THIS_DIR="${0:A:h}"

IVLPP="$THIS_DIR/../../iverilog-parser/ivlpp/ivlpp"

preproc() {
	# if [[ which ivlpp &>/dev/null ]]; then
	# 	ivlpp $1
	# else
	# 	cat $1
	# fi
	cat $1
}

preproc $FILENAME | \
	sed -e '/^[[:space:]]*\/\//d' $FILENAME | \
	sed -r ':a; s%(.*)/\*.*\*/%\1%; ta; /\/\*/ !b; N; ba' | \
	sed '/^[[:space:]]*$/d' | \
	{ [[ -n "$TRAILING" ]] && sed -e 's/[[:space:]]*\/\/.*$//' || cat }

