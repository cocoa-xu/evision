#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Post-process `lib/generated/evision.ex` to inject `@doc group:` annotations.

In a from-source build the codegen in `py_src/pipeline.py` already emits these
annotations directly. This script exists for the precompiled flow: when
`mix compile.evision_precompiled` copies a cached `evision.ex` that pre-dates
the codegen change, this script reads OpenCV's headers locally and patches
the file in place so `mix docs` can group functions today instead of waiting
for the next precompiled release.

Usage:

    python3 scripts/inject_doc_groups.py [OPENCV_SRC_DIR] [OPENCV_CONTRIB_DIR]

Both args default to ../opencv and ../opencv_contrib relative to the repo
root. The OPENCV_SRC environment variable overrides the first arg; the
OPENCV_CONTRIB env var overrides the second.
"""

from __future__ import print_function

import glob
import os
import re
import sys

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(REPO, "py_src"))

import doxygen_groups  # noqa: E402


def _collect_headers(*roots):
    headers = []
    for root in roots:
        if not root or not os.path.isdir(root):
            continue
        for ext in ("hpp", "h"):
            headers.extend(
                glob.glob(
                    os.path.join(root, "modules", "**", "include", "opencv2", "**", "*." + ext),
                    recursive=True,
                )
            )
            headers.extend(
                glob.glob(
                    os.path.join(root, "modules", "**", "include", "opencv2", "*." + ext),
                    recursive=True,
                )
            )
    return headers


def _inject(source, func_to_group, group_titles):
    # Insert `@doc group: "..."` before each `@doc """ ... """` block.
    # Function names live a few lines below the closing triple-quote; sniff
    # that line for `def someName(...)` and skip the keyword-arg overload
    # since it shares the same group annotation as the canonical clause.
    def_re = re.compile(r'^\s*def\s+([a-zA-Z_]\w*)(?:\(|\s+(?:do\b|when\b))')
    out = []
    i = 0
    lines = source.splitlines(keepends=True)
    while i < len(lines):
        line = lines[i]
        if line.strip() == '@doc """':
            j = i + 1
            while j < len(lines) and lines[j].strip() != '"""':
                j += 1
            func_name = None
            k = j + 1
            while k < min(len(lines), j + 8) and func_name is None:
                m = def_re.match(lines[k])
                if m:
                    func_name = m.group(1)
                k += 1
            indent = line[: len(line) - len(line.lstrip())]
            if func_name:
                gid = doxygen_groups.lookup(func_to_group, func_name)
                if gid:
                    title = doxygen_groups.title_for(group_titles, gid)
                    out.append(f'{indent}@doc group: {_elixir_string(title)}\n')
            out.append(line)
            i += 1
            continue
        out.append(line)
        i += 1
    return "".join(out)


def _elixir_string(value):
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def main():
    opencv = os.environ.get("OPENCV_SRC") or (sys.argv[1] if len(sys.argv) > 1 else None)
    contrib = os.environ.get("OPENCV_CONTRIB") or (sys.argv[2] if len(sys.argv) > 2 else None)
    if not opencv:
        opencv = os.path.realpath(os.path.join(REPO, "..", "opencv"))
    if not contrib:
        contrib = os.path.realpath(os.path.join(REPO, "..", "opencv_contrib"))

    headers = _collect_headers(opencv, contrib)
    if not headers:
        print(f"No OpenCV headers found under {opencv!r} or {contrib!r}", file=sys.stderr)
        sys.exit(1)

    print(f"Scanning {len(headers)} OpenCV headers...")
    titles, funcs = doxygen_groups.scan(headers)
    print(f"  {len(titles)} groups, {len(funcs)} tagged function symbols.")

    target = os.path.join(REPO, "lib", "generated", "evision.ex")
    with open(target, "r", encoding="utf-8") as f:
        src = f.read()
    patched = _inject(src, funcs, titles)
    added = patched.count("@doc group:") - src.count("@doc group:")
    with open(target, "w", encoding="utf-8") as f:
        f.write(patched)
    print(f"Wrote {target} (+{added} @doc group: annotations)")


if __name__ == "__main__":
    main()
