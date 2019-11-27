#!/usr/bin/env python3

import sys
import os
import subprocess

PACKAGE_NAME = "iodine"

def add_quotes(s):
    i = s.find(" ")
    if i < 0:
        return s
    else:
        return '"{}"'.format(s)

def run_tests(test_args, stack_args=[]):
    cmd = ["stack"] + stack_args + ["test", PACKAGE_NAME]

    if test_args:
        args = " ".join([add_quotes(a) for a in test_args])
        cmd += ["--test-arguments", args]

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
