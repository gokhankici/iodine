import os.path as p

DEBUG = False

LINPROG_DIR = p.realpath(p.dirname(__file__))
PROJECT_ROOT = p.realpath(p.join(LINPROG_DIR, "../../"))
IVERILOG_DIR = p.join(PROJECT_ROOT, "iverilog-parser")
BENCHMARK_DIR = p.join(PROJECT_ROOT, "benchmarks")
SCHEMA_FILE = p.join(PROJECT_ROOT, "annotation-schema.json")
