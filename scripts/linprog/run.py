#!/usr/bin/env python3

import argparse
from os.path import join
import subprocess
import sys
from config import ABDUCTION_OUTPUT, LINPROG_DIR, DEBUG, TMP_ANNOTFILE
from benchmark import Benchmark, BENCHMARKS
from minimize import minimize


def err(msg):
    print(msg, file=sys.stderr)
    sys.exit(1)


def create_tmp_annotfile(b):
    rc = b.run_abduction()
    if rc != 0:
        sys.exit(1)

    # run assumption.py and create the annot-last.json file
    with open(TMP_ANNOTFILE, "w") as f:
        args = [join(LINPROG_DIR, "assumptions.py"),
                ABDUCTION_OUTPUT,
                b.annotfile]
        r = subprocess.run(args, stdout=f)
    if r.returncode != 0:
        sys.exit(r.returncode)

    if DEBUG:
        subprocess.run(["cat", TMP_ANNOTFILE])


def run(b, args):
    if not args.skip:
        create_tmp_annotfile(b)

    # re-run iodine with the new annot file
    b2 = b.with_annotfile(TMP_ANNOTFILE)
    rc = b2.run_iodine(stdout=subprocess.DEVNULL)
    if rc != 0:
        print("ERROR: Iodine rejected {} !".format(TMP_ANNOTFILE),
              file=sys.stderr)
        sys.exit(1)

    if args.minimize:
        af = minimize(b, TMP_ANNOTFILE)
        with open(TMP_ANNOTFILE, "w") as f:
            f.write(af.dump())


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="""
    Extract the CFG of the verilog program and run the abduction script.
    """)
    ap.add_argument("-b", "--benchmark", choices=BENCHMARKS.keys(),
                    help="Benchmark name")
    ap.add_argument("-f", "--filename", help="Verilog file")
    ap.add_argument("-m", "--module", help="Module name")
    ap.add_argument("-a", "--annotation", help="Annotation JSON file")
    ap.add_argument("--skip", action="store_true",
                    help="Just run the benchmark with temp annotation file")
    ap.add_argument("--minimize", action="store_true",
                    help="Run the minimization step")
    args = ap.parse_args()

    if args.benchmark:
        run(BENCHMARKS[args.benchmark], args)
    elif args.filename and args.module and args.annotation:
        b = Benchmark(filename=args.filename,
                      module=args.module,
                      annotfile=args.annotation)
        run(b, args)
    else:
        ap.print_help()
        sys.exit(1)
