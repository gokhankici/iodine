import collections
import os.path as p
import subprocess
import sys


class Benchmark(collections.namedtuple("Benchmark",
                                       ["filename", "module", "annotfile"])):
    """
    Contains the information required to run a benchmark.
    """
    IVERILOG_DIR = p.realpath(p.join(p.dirname(__file__),
                                     "../../",
                                     "iverilog-parser"))
    DEBUG = False

    def run_iodine(self, extra_args=[]):
        args = ["stack", "exec", "iodine", "--",
                "--iverilog-dir", Benchmark.IVERILOG_DIR]

        args.extend(extra_args)
        args.extend([p.realpath(self.filename),
                     self.module,
                     p.realpath(self.annotfile)])

        if Benchmark.DEBUG:
            print("running:\n{}".format(" ".join(args)),
                  file=sys.stderr)

        rc = subprocess.run(args).returncode
        if rc != 0:
            print("iodine failed", file=sys.stderr)
            sys.exit(rc)

    def run_abduction(self):
        """ Run Iodine but with the abduction feature """
        self.run_iodine(["--abduction"])

    def with_annot(self, annotfile):
        """ Returns a new benchmark with the given annotation file """
        return Benchmark(filename=self.filename,
                         module=self.module,
                         annotfile=annotfile)
