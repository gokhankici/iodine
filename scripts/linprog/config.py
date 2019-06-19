import os.path as p

DEBUG = False

LINPROG_DIR = p.realpath(p.dirname(__file__))
PROJECT_ROOT = p.realpath(p.join(LINPROG_DIR, "../../"))
BENCHMARK_DIR = p.join(PROJECT_ROOT, "benchmarks")
SCHEMA_FILE = p.join(PROJECT_ROOT, "annotation-schema.json")
IODINE_SCRIPT = p.join(PROJECT_ROOT, "iodine")
ABDUCTION_OUTPUT = p.join(PROJECT_ROOT, "cplex.json")
TMP_ANNOTFILE = "annot-last.json"
