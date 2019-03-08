#!/usr/bin/env python2

import sys
import subprocess

def run_tests(cmdargs):
    PACKAGE_NAME = "verylog-hs"
    EXE_NAME     = "iodine"
    TEST_NAME    = "iodine-test"

    exe_comp  = "%s:%s"      % (PACKAGE_NAME, EXE_NAME)
    test_comp = "%s:test:%s" % (PACKAGE_NAME, TEST_NAME)

    def add_quotes(s):
        i = s.find(" ")
        if i < 0:
            return s
        else:
            return '"' + s + '"'

    args = " ".join([add_quotes(a) for a in cmdargs])

    cmd = [ "stack"
          , "build", exe_comp, test_comp
          , "--test-arguments", args
          ]

    try:
        subprocess.call(cmd)
    except KeyboardInterrupt:
        pass

run_tests(sys.argv[1:])
