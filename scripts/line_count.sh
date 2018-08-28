#!/bin/zsh

if [[ $# -ne 1 ]]; then
	echo "usage $0 <filename>" >&2
	exit 1
fi

FILENAME="$1"

THIS_DIR="${0:A:h}"

# ------------------------------------------------------------------------------

echo "number of lines:"

pushd ${FILENAME:h}
${THIS_DIR}/remove_comments.sh ${FILENAME:t} | wc -l
popd

# ------------------------------------------------------------------------------

echo "number of annots:"

grep '@annot' $FILENAME | wc -l

# ------------------------------------------------------------------------------

