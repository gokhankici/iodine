from annotation import AnnotationFile
from itertools import combinations
import json
import subprocess


TMP_FILE = "tmp.json"


def go(b, af, getter, setter):
    new_b = b.with_annot(TMP_FILE)
    orig_vars = getter(af)
    needed = orig_vars.copy()

    for v in sorted(orig_vars):
        setter(af, needed - {v})
        with open(TMP_FILE, "w") as f:
            json.dump(af.to_json(), f)
        rc = new_b.run_iodine(stdout=subprocess.DEVNULL,
                              stderr=subprocess.DEVNULL)
        if rc == 0:
            print("Skipping {}".format(v))
            needed.remove(v)
        else:
            print("Keeping  {}".format(v))

    setter(af, orig_vars)
    return needed


def minimize(b, annotfile):
    af = AnnotationFile(filename=annotfile)
    ae = go(b, af,
            (lambda af: af.annotations.always_eq),
            (lambda af, v: af.annotations.set_always_eq(v)))
    # ie = go(b, af,
    #         (lambda af: af.annotations.initial_eq),
    #         (lambda af, v: af.annotations.set_initial_eq(v)))
    af.annotations.set_always_eq(ae)
    # af.annotations.set_initial_eq(ie)
    return af
