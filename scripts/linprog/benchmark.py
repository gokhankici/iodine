import collections
import os.path as p
import subprocess
import sys
from config import IODINE_SCRIPT, ABDUCTION_OUTPUT, DEBUG
import shutil


class Benchmark(collections.namedtuple("Benchmark",
                                       ["filename", "module", "annotfile"])):
    """
    Contains the information required to run a benchmark.
    """
    def run_iodine(self, extra_args=[], **kwargs):
        args = [IODINE_SCRIPT] + extra_args
        args.extend([p.realpath(self.filename),
                     self.module,
                     p.realpath(self.annotfile)])

        if DEBUG:
            print("running:\n{}".format(" ".join(args)),
                  file=sys.stderr)

        return subprocess.run(args, **kwargs).returncode

    def run_abduction(self):
        """ Run Iodine but with the abduction feature """
        rc = self.run_iodine(["--abduction"])
        if rc == 0:
            shutil.move(ABDUCTION_OUTPUT, p.basename(ABDUCTION_OUTPUT))
        return rc

    def with_annot(self, annotfile):
        """ Returns a new benchmark with the given annotation file """
        return Benchmark(filename=self.filename,
                         module=self.module,
                         annotfile=annotfile)
