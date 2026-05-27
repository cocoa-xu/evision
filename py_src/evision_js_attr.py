#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Resolve evision ↔ opencv.js correspondences for the codegen ``@js`` tag.

The whitelist source of truth is OpenCV's own
``platforms/js/opencv_js.config.py`` (parsed by ``opencv_js_whitelist``).
This module turns that flat data into an ``(classname, fn_name) → entry``
index keyed by the C++ shape evision's ``FuncInfo`` exposes, and renders
Elixir/Erlang persistent-attribute syntax for the codegen to splice in.

If the OpenCV source tree is not on disk (e.g. when building against a
precompiled binary), the lookup degrades to "no matches" — the codegen
still produces output, just without ``@js`` tags.

See ``projects/evision/context/js-correspondence-tag.md`` in the
``cocoa/ai-native`` notebook for the full design.
"""

from __future__ import annotations

from typing import Dict, Optional, Tuple

from opencv_js_whitelist import (
    ConfigNotFoundError,
    entries,
    namespace_prefix_override,
)


JsEntry = Dict[str, object]
LookupKey = Tuple[Optional[str], str]


_INDEX: Optional[Dict[LookupKey, JsEntry]] = None


def _build_index() -> Dict[LookupKey, JsEntry]:
    overrides = namespace_prefix_override()
    index: Dict[LookupKey, JsEntry] = {}
    for _module, classname, fn_name, kind in entries():
        key: LookupKey = (classname, fn_name)
        if classname is None:
            index[key] = {"js_kind": "function", "js_name": f"cv.{fn_name}"}
        elif kind == "constructor":
            index[key] = {
                "js_kind": "constructor",
                "js_class": f"cv.{_js_class_for(classname, overrides)}",
            }
        else:
            index[key] = {
                "js_kind": "method",
                "js_class": f"cv.{_js_class_for(classname, overrides)}",
                "js_method": fn_name,
            }
    return index


def _ensure_loaded() -> Dict[LookupKey, JsEntry]:
    global _INDEX
    if _INDEX is None:
        try:
            _INDEX = _build_index()
        except ConfigNotFoundError:
            _INDEX = {}
    return _INDEX


def lookup_for_func(func) -> Optional[JsEntry]:
    """Return the opencv.js entry matching an evision ``FuncInfo``, or ``None``.

    Lookup uses C++ shapes (``func.name``, ``func.classname``) per CCD-18's
    closing refinements: the post-folded Elixir name lowercases the first
    letter so a match against ``Canny`` would miss.
    """
    index = _ensure_loaded()
    if not index:
        return None
    if func.isconstructor:
        # The whitelist names the constructor as the bare class segment
        # (``aruco_Dictionary`` → ``Dictionary``), not the full path-joined
        # name ``func.name`` carries.
        bare = func.classname.rsplit("_", 1)[-1] if func.classname else ""
        key: LookupKey = (func.classname, bare)
    elif func.classname:
        key = (func.classname, func.name)
    else:
        ns = func.namespace
        if ns == "cv":
            key = (None, func.name)
        elif ns.startswith("cv."):
            # ``cv::fisheye::undistortImage`` lives in ``calib3d['']`` as
            # ``fisheye_undistortImage`` — the C++ namespace below ``cv`` is
            # flattened into the fn_name with an underscore separator.
            ns_below = ns[3:].replace(".", "_")
            key = (None, f"{ns_below}_{func.name}")
        else:
            return None
    return index.get(key)


def _js_class_for(class_key: str, overrides: Dict[str, str]) -> str:
    """Project a whitelist class key onto its opencv.js class name.

    embindgen's ``namespace_prefix_override`` strips ``dnn_``/``aruco_``
    prefixes when the whitelist is a Python file (defaults live in
    ``opencv_js_whitelist.DEFAULT_NAMESPACE_PREFIX_OVERRIDE``).
    """
    if "_" in class_key:
        prefix, rest = class_key.split("_", 1)
        if prefix in overrides:
            override = overrides[prefix]
            return f"{override}.{rest}" if override else rest
    return class_key


# ----- Source emitters ----------------------------------------------------


def elixir_register_attribute() -> str:
    """Single line that opts a module into accumulating ``@js`` attributes."""
    return (
        "  Module.register_attribute(__MODULE__, :js, persist: true, accumulate: true)\n"
    )


def elixir_attribute_line(entry: JsEntry, fun: str, arity: int) -> str:
    """``  @js %{...}\\n`` line ready to splice before a ``def`` clause."""
    fields = [
        f"fun: {_elixir_atom(fun)}",
        f"arity: {arity}",
        f"js_kind: :{entry['js_kind']}",
    ]
    if "js_name" in entry:
        fields.append(f'js_name: "{entry["js_name"]}"')
    if "js_class" in entry:
        fields.append(f'js_class: "{entry["js_class"]}"')
    if "js_method" in entry:
        fields.append(f'js_method: "{entry["js_method"]}"')
    return f"  @js %{{{', '.join(fields)}}}\n"


def erlang_attribute_line(entry: JsEntry, fun: str, arity: int) -> str:
    """``-js(#{...}).\\n`` line ready to splice before a function clause."""
    fields = [
        f"fun => {_erlang_atom(fun)}",
        f"arity => {arity}",
        f"js_kind => {entry['js_kind']}",
    ]
    if "js_name" in entry:
        fields.append(f'js_name => <<"{entry["js_name"]}">>')
    if "js_class" in entry:
        fields.append(f'js_class => <<"{entry["js_class"]}">>')
    if "js_method" in entry:
        fields.append(f'js_method => <<"{entry["js_method"]}">>')
    return f"-js(#{{{', '.join(fields)}}}).\n"


def _elixir_atom(name: str) -> str:
    # All evision-emitted def names are valid bare Elixir atoms (letter,
    # then letters/digits/underscore). The quoted form would still work but
    # would clutter generated source.
    return f":{name}"


def _erlang_atom(name: str) -> str:
    # Erlang atoms must start with lowercase. The evision codegen lowercases
    # the first character of every emitted name (`map_argname_elixir`,
    # `get_module_func_name`), so this is always safe without quoting.
    return name
