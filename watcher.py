#!/usr/bin/env python
from __future__ import print_function
import os
import time
from subprocess import call
import re

os.chdir(os.path.realpath(os.path.dirname(__file__)))

VALID_REG = re.compile(r"^\w+\.(purs|css|js)$")

def get_changed(mtimes):
    changed_files = set()
    for toplevel in ("src",):
        for root, _, files in os.walk(toplevel):
            for f in files:
                path = os.path.join(root, f)
                if not VALID_REG.match(f):
                    if f.startswith('.#'):
                        print("Removing emacs lockfile", path)
                        os.unlink(path)
                    continue

                path = os.path.join(root, f)
                mtime = os.stat(path).st_mtime
                if path not in mtimes or mtime != mtimes[path]:
                    mtimes[path] = mtime
                    changed_files.add(path)
    return mtimes, changed_files

mtimes = {}
while True:
    mtimes, changed_files = get_changed(mtimes)
    if len(changed_files) > 0:
        print("\033[32mFiles changed:\033[0m")
        for f in sorted(changed_files):
            print("\033[32m    {}\033[0m".format(f))
        start = time.time()
        code = call('pulp browserify > bundle.js', shell=True)
        if code == 0:
            print("\033[32mbuilt successfully, {} seconds\033[0m"
                  .format(time.time() - start))
        else:
            print("\033[31mbuild failed, {} seconds\033[0m"
                  .format(time.time() - start))
    else:
        time.sleep(2)
