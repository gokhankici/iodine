#!/bin/sh

PACKAGE_NAME='verylog-hs'
EXE_NAME='vcgen-fp'
TEST_NAME='vcgen-test'

EXE_COMP="${PACKAGE_NAME}:${EXE_NAME}"
TEST_COMP="${PACKAGE_NAME}:test:${TEST_NAME}"

stack build $EXE_COMP $TEST_COMP --test-arguments "$*"
