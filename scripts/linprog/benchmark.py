import collections
import os.path as p
import subprocess
import sys
from config import BENCHMARK_DIR, IODINE_SCRIPT, DEBUG


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
        return self.run_iodine(["--abduction"])

    def with_annotfile(self, annotfile):
        """ Returns a new benchmark with the given annotation file """
        return Benchmark(filename=self.filename,
                         module=self.module,
                         annotfile=annotfile)


# name, file and module of the benchmarks
CONFIG = [("ctalu",
           "xcrypto-ref/rtl/coprocessor/scarv_cop_palu.v",
           "scarv_cop_palu"),
          ("mips",
           "472-mips-pipelined/mips_pipeline.v",
           "mips_pipeline"),
          ("yarvi",
           "yarvi/shared/yarvi.v",
           "yarvi"),
          ("sha",
           "crypto_cores/sha_core/trunk/rtl/sha256.v",
           "sha256"),
          ("fpu",
           "fpu/verilog/fpu.v",
           "fpu"),
          ("fpu-div",
           "fpu2/divider/divider.v",
           "divider"),
          ("modexp",
           "crypto_cores/RSA4096/ModExp2/ModExp.v",
           "ModExp")]


def mk_bmk(filename, module):
    full_filename = p.join(BENCHMARK_DIR, filename)
    h, t = p.split(full_filename)
    n, _ = p.splitext(t)
    a = p.join(h, "annot-{}.json".format(n))
    return Benchmark(filename=full_filename,
                     module=module,
                     annotfile=a)


BENCHMARKS = {n: mk_bmk(f, m) for n, f, m in CONFIG}
