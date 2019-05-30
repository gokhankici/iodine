#!/usr/bin/env python3.6

import sys
import os
import subprocess

def run_tests(test_args, stack_args=[], build_args=[]):
    PACKAGE_NAME = "iodine"
    EXE_NAME     = "iodine"
    TEST_NAME    = "iodine-test"

    exe_comp  = "{}:{}".format(PACKAGE_NAME, EXE_NAME)
    test_comp = "{}:test:{}".format(PACKAGE_NAME, TEST_NAME)

    def add_quotes(s):
        i = s.find(" ")
        if i < 0:
            return s
        else:
            return '"{}"'.format(s)

    args = " ".join([add_quotes(a) for a in test_args])

    cmd = [ "stack" ] + stack_args + \
          [ "build" ] + build_args + \
          [ exe_comp, test_comp, "--test-arguments", args ]

    try:
        return subprocess.run(cmd).returncode
    except KeyboardInterrupt:
        return 1

if __name__ == "__main__":
    if os.getenv("PROFILE"):
        rc = run_tests(sys.argv[1:], stack_args=["--profile", "--work-dir", ".stack-work-profile"])
    else:
        rc = run_tests(sys.argv[1:])
    sys.exit(rc)
