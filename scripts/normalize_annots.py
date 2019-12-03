#!/usr/bin/env python3

from collections import defaultdict
from pathlib import Path
import json
import subprocess
import sys

# this file is under ROOT_DIR/scripts/
THIS_DIR = Path(__file__).resolve().parent
ROOT_DIR = THIS_DIR.parent
RESULTS_FILE = THIS_DIR / "passed.txt"

def print_json(j, output=None):
  print(
    json.dumps(j, indent=2),
    file=(output if output else sys.stdout))

def parse_test_results():
  passedTests = []
  with open(RESULTS_FILE, "r") as f:
    for line in f:
      cols = line.strip().split(" ")
      if len(cols) < 3:
        continue
      [t, m, af] = cols
      if t == "passed":
        passedTests.append((m, af))
  return passedTests

def normalize_annotation_file(module_name, annotation_file):
  with open(ROOT_DIR/annotation_file, "r") as f:
    annot = json.load(f)

  new_file = defaultdict(dict)
  new_file["top_module"] = module_name
  new_file["modules"][module_name] = defaultdict(dict)

  new_annot = defaultdict(set)
  for a in annot["annotations"]:
    t  = a["type"]
    vs = a["variables"]
    new_annot[t].update(vs)
    assert(a.keys() == set(["type", "variables"]))

  new_file["modules"][module_name]["annotations"] = {k: list(sorted(vs, key=str.lower)) for k, vs in new_annot.items()}

  if "clock" in annot:
    new_file["modules"][module_name]["clock"] = annot["clock"]

  if "qualifiers" in annot:
    new_file["modules"][module_name]["qualifiers"] = annot["qualifiers"]

  return new_file

if __name__ == "__main__":
  passed_tests = parse_test_results()
  for module_name, annotation_file in passed_tests:
    normalized_annot = normalize_annotation_file(module_name, annotation_file)
    print_json(normalized_annot)
    with open(ROOT_DIR/annotation_file, "w") as f:
      print_json(normalized_annot, output=f)

