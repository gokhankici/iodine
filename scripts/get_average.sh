#!/bin/zsh

THIS_DIR=${0:A:h}

if [[ $# -ne 2 ]]; then
	echo "usage: $0 <test name> <iter count>"
	exit 1
fi

$THIS_DIR/run-tests \
	--no-save --no-output \
	--single $1 --time --times $2 2>&1 | \
	\
	sed --unbuffered --silent 's/^Time taken: \([0-9]\+\) .*/\1/p' | \
	tee /dev/tty | \
	\
	awk '{sum += $1; n += 1;} END {if(n>0){ printf("%d runs, ave: %f.1\n", n, sum/n); }}'


