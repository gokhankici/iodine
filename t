#!/bin/sh

PACKAGE_NAME='verylog-hs'
TEST_NAME='iodine-test'

TEST_COMP="${PACKAGE_NAME}:test:${TEST_NAME}"

stack build $TEST_COMP --test-arguments "$*"
