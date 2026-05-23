#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Lightweight scanner for OpenCV's Doxygen group tags.

OpenCV organizes its public API into hierarchical groups via Doxygen, e.g.

    @defgroup imgproc Image Processing
        @defgroup imgproc_filter Image Filtering
        @defgroup imgproc_transform Geometric Image Transformations
        ...

Function declarations inside a `@addtogroup ... @{ ... @}` block (or carrying
an explicit `@ingroup`) belong to that group. The Elixir bindings can carry
this taxonomy forward to ExDoc via `@doc group: "<title>"`, which is what the
`Evision` root module needs to make its 1400+ functions navigable.

`scan(srcfiles)` returns two dicts:

  * group_titles  - group_id (e.g. "imgproc_filter") -> human-readable title
                    (e.g. "Image Filtering")
  * func_to_group - unqualified function name (e.g. "GaussianBlur") -> group_id

Names are matched case-insensitively to absorb the lower/upperCamelCase mismatch
between C++ declarations and the binding's emitted function name.
"""

import re

DEFGROUP_RE = re.compile(r"@defgroup\s+(\w+)\s+([^\n*]+)")
ADDTOGROUP_RE = re.compile(r"@addtogroup\s+(\w+)")
INGROUP_RE = re.compile(r"@ingroup\s+(\w+)")
# CV_EXPORTS_W [return type] name(...
FUNC_DECL_RE = re.compile(
    r"CV_EXPORTS_W(?:_SIMPLE|_AS\([^)]*\))?\s+"
    r"(?:[\w:&<>,\*\s]+?\s+)?"
    r"(\w+)\s*\("
)


def _is_open(line):
    return "@{" in line


def _is_close(line):
    return "@}" in line


def scan(srcfiles):
    titles = {}
    func_to_group = {}

    for path in srcfiles:
        try:
            with open(path, "r", encoding="utf-8", errors="replace") as f:
                text = f.read()
        except OSError:
            continue

        for m in DEFGROUP_RE.finditer(text):
            titles.setdefault(m.group(1), m.group(2).strip().rstrip("*/").strip())

        _scan_funcs(text, func_to_group)

    return titles, func_to_group


# Suffix/prefix variants that OpenCV (and our binding) emits without
# repeating the @ingroup tag — e.g. `calibrateCameraExtended` inherits from
# `calibrateCamera`, `createBackgroundSubtractorKNN` from
# `BackgroundSubtractorKNN`. Try a few of these so coverage doesn't crater
# on aliases.
_VARIANT_SUFFIXES = (
    "extended", "roextended", "withmask", "withmeta", "withstats",
    "withalgorithm", "withaccumulator", "withlabels", "buffer",
)


def lookup(func_to_group, name):
    key = name.lower()
    g = func_to_group.get(key)
    if g:
        return g
    for suf in _VARIANT_SUFFIXES:
        if key.endswith(suf):
            g = func_to_group.get(key[: -len(suf)])
            if g:
                return g
    if key.startswith("create") and len(key) > 6:
        g = func_to_group.get(key[6:])
        if g:
            return g
    return None


def _scan_funcs(text, func_to_group):
    scope_stack = []
    pending_group = None
    pending_ingroup = None

    in_block_comment = False
    block_comment_buf = []

    for raw in text.splitlines():
        line = raw.strip()

        if in_block_comment:
            block_comment_buf.append(line)
            if "*/" in line:
                joined = "\n".join(block_comment_buf)
                am = ADDTOGROUP_RE.search(joined)
                if am:
                    pending_group = am.group(1)
                if "@{" in joined and pending_group is not None:
                    scope_stack.append(pending_group)
                    pending_group = None
                if "@}" in joined and scope_stack:
                    scope_stack.pop()
                ig = INGROUP_RE.search(joined)
                pending_ingroup = ig.group(1) if ig else None
                in_block_comment = False
                block_comment_buf = []
            continue

        if line.startswith("/*") and "*/" not in line:
            pending_ingroup = None
            in_block_comment = True
            block_comment_buf = [line]
            continue

        am = ADDTOGROUP_RE.search(line)
        if am:
            pending_group = am.group(1)
            if _is_open(line):
                scope_stack.append(pending_group)
                pending_group = None
            continue

        if _is_open(line) and pending_group is not None:
            scope_stack.append(pending_group)
            pending_group = None
            continue

        if _is_close(line) and scope_stack:
            scope_stack.pop()
            continue

        ig = INGROUP_RE.search(line)
        if ig:
            pending_ingroup = ig.group(1)
            continue

        if not line:
            continue

        fm = FUNC_DECL_RE.search(line)
        if fm:
            fname = fm.group(1)
            gid = pending_ingroup or (scope_stack[-1] if scope_stack else None)
            if gid:
                func_to_group.setdefault(fname.lower(), gid)

        # Any non-blank, non-comment, non-marker line consumes the pending
        # @ingroup: it was meant for the next declaration, even if that
        # declaration was an enum/class our function regex can't see.
        pending_ingroup = None


def title_for(group_titles, group_id):
    title = group_titles.get(group_id)
    if title:
        return title
    parent = group_id.rsplit("_", 1)[0] if "_" in group_id else None
    if parent and parent in group_titles:
        return group_titles[parent]
    return group_id
