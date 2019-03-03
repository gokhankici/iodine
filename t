#!/bin/zsh

THIS_DIR="${0:A:h}"
. "${THIS_DIR}/configuration.sh"

stack test verylog-hs:test:vcgen-test \
	--test-arguments="\"${THIS_DIR}\" \"${IVL_DIR:A}\" $*"
