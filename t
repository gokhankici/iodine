#!/bin/sh

TEST_NAME='iodine-test'

build() {
    stack build
}

test() {
    stack exec $TEST_NAME -- $@
}

build && test $@
