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

import sys
from typing import Dict, Iterable, List, Optional, Tuple

from opencv_js_whitelist import (
    ConfigNotFoundError,
    entries,
    namespace_prefix_override,
)

if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO


JsEntry = Dict[str, object]
LookupKey = Tuple[Optional[str], str]
RuntimeEntry = Dict[str, object]


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
    if "arg_plan" in entry:
        fields.append(f"arg_plan: {_elixir_arg_plan(entry['arg_plan'])}")
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
    if "arg_plan" in entry:
        fields.append(f"arg_plan => {_erlang_arg_plan(entry['arg_plan'])}")
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


def _elixir_arg_plan(plan) -> str:
    # opencv.js positional arg-plan as an Elixir atom list: [:in, :out, :in].
    return "[" + ", ".join(f":{slot}" for slot in plan) + "]"


def _erlang_arg_plan(plan) -> str:
    # opencv.js positional arg-plan as an Erlang atom list: [in, out, in].
    return "[" + ", ".join(plan) + "]"


# ----- Runtime module emission (Evision.JS / evision_js) ------------------


def collect_runtime_entries(module_generators: Iterable, kind: str) -> List[RuntimeEntry]:
    """Flatten ``js_attrs[kind]`` across every emitted ``ModuleGenerator``.

    Each ``ModuleGenerator`` carries its own
    ``js_attrs[kind] = {fun_name: {arity: js_entry}}``. The runtime module
    needs a single flat list of entries keyed by
    ``(module, fun, arity)`` — the Elixir module name is reconstructed
    from the generator's ``module_name`` (the root ``Evision`` keeps that
    bare name; everything else is prefixed with ``Evision.``).

    Output is sorted by ``(module, fun, arity)`` so the generated source
    is reproducible across runs.
    """
    out: List[RuntimeEntry] = []
    for mg in module_generators:
        mod_name = mg.module_name
        full_module = mod_name if mod_name == "Evision" else f"Evision.{mod_name}"
        kind_attrs = mg.js_attrs.get(kind) or {}
        for fun_name, by_arity in kind_attrs.items():
            for arity, js_entry in by_arity.items():
                entry: RuntimeEntry = {
                    "module": full_module,
                    "fun": fun_name,
                    "arity": arity,
                    "js_kind": js_entry["js_kind"],
                }
                for optional_key in ("js_name", "js_class", "js_method", "arg_plan"):
                    if optional_key in js_entry:
                        entry[optional_key] = js_entry[optional_key]
                out.append(entry)
    out.sort(key=lambda e: (e["module"], e["fun"], e["arity"]))
    return out


def elixir_runtime_module(entries_list: List[RuntimeEntry]) -> str:
    """Render ``lib/generated/evision_js.ex`` from a flat entry list."""
    out = StringIO()
    out.write("defmodule Evision.JS do\n")
    out.write('  @moduledoc """\n')
    out.write("  Generated map of evision bindings to their opencv.js counterparts.\n")
    out.write("\n")
    out.write("  Each entry carries the Elixir `(module, fun, arity)` plus the\n")
    out.write("  opencv.js call shape: `:js_kind` discriminates between\n")
    out.write("  `:function`, `:method` and `:constructor`; `:js_name` is set\n")
    out.write("  for free functions, `:js_class` for class methods and\n")
    out.write("  constructors, and `:js_method` for methods only.\n")
    out.write('  """\n\n')
    out.write('  @typedoc "An opencv.js correspondence entry."\n')
    out.write("  @type entry :: %{\n")
    out.write("          required(:module) => module(),\n")
    out.write("          required(:fun) => atom(),\n")
    out.write("          required(:arity) => arity(),\n")
    out.write("          required(:js_kind) => :function | :method | :constructor,\n")
    out.write("          optional(:arg_plan) => [:in | :out],\n")
    out.write("          optional(:js_name) => String.t(),\n")
    out.write("          optional(:js_class) => String.t(),\n")
    out.write("          optional(:js_method) => String.t()\n")
    out.write("        }\n\n")
    if entries_list:
        out.write("  @whitelist [\n")
        for entry in entries_list:
            out.write("    " + _elixir_entry_literal(entry) + ",\n")
        out.write("  ]\n\n")
    else:
        out.write("  @whitelist []\n\n")
    out.write("  @index Map.new(@whitelist, fn entry -> {{entry.module, entry.fun, entry.arity}, entry} end)\n\n")
    out.write('  @doc "Every opencv.js correspondence entry emitted by evision\'s codegen."\n')
    out.write("  @spec whitelist() :: [entry()]\n")
    out.write("  def whitelist, do: @whitelist\n\n")
    out.write('  @doc "Return `{:ok, entry}` for a known `(module, fun, arity)`; `:error` otherwise."\n')
    out.write("  @spec lookup(module(), atom(), arity()) :: {:ok, entry()} | :error\n")
    out.write("  def lookup(module, fun, arity), do: Map.fetch(@index, {module, fun, arity})\n\n")
    out.write('  @doc "Whether the given `(module, fun, arity)` has an opencv.js counterpart."\n')
    out.write("  @spec runnable?(module(), atom(), arity()) :: boolean()\n")
    out.write("  def runnable?(module, fun, arity), do: Map.has_key?(@index, {module, fun, arity})\n")
    out.write("end\n")
    return out.getvalue()


def erlang_runtime_module(entries_list: List[RuntimeEntry]) -> str:
    """Render ``src/generated/evision_js.erl`` from a flat entry list.

    ``fun`` is a reserved word in Erlang's expression / type-spec grammar,
    so every map key referring to that field is quoted as ``'fun'``.
    Inside ``-attribute(#{...}).`` declarations (CCD-19's ``-js(...)``) the
    attribute parser is more permissive and bare ``fun`` works there — that
    permissiveness does NOT extend to function bodies or ``-spec``s.
    """
    out = StringIO()
    out.write("-module(evision_js).\n")
    out.write("-compile(nowarn_export_all).\n")
    out.write("-compile([export_all]).\n\n")
    out.write("-type entry() :: #{\n")
    out.write("    module := module(),\n")
    out.write("    'fun' := atom(),\n")
    out.write("    arity := arity(),\n")
    out.write("    js_kind := function | method | constructor,\n")
    out.write("    arg_plan => [in | out],\n")
    out.write("    js_name => binary(),\n")
    out.write("    js_class => binary(),\n")
    out.write("    js_method => binary()\n")
    out.write("}.\n\n")
    out.write("-spec whitelist() -> [entry()].\n")
    out.write("whitelist() ->\n")
    if entries_list:
        out.write("    [\n")
        for i, entry in enumerate(entries_list):
            sep = "," if i < len(entries_list) - 1 else ""
            out.write(f"        {_erlang_entry_literal(entry)}{sep}\n")
        out.write("    ].\n\n")
    else:
        out.write("    [].\n\n")
    out.write("-spec lookup(module(), atom(), arity()) -> {ok, entry()} | error.\n")
    out.write("lookup(Module, Fun, Arity) ->\n")
    out.write("    maps:find({Module, Fun, Arity}, index()).\n\n")
    out.write("-spec runnable(module(), atom(), arity()) -> boolean().\n")
    out.write("runnable(Module, Fun, Arity) ->\n")
    out.write("    maps:is_key({Module, Fun, Arity}, index()).\n\n")
    out.write("index() ->\n")
    if entries_list:
        out.write("    #{\n")
        for i, entry in enumerate(entries_list):
            sep = "," if i < len(entries_list) - 1 else ""
            out.write(f"        {_erlang_key(entry)} => {_erlang_entry_literal(entry)}{sep}\n")
        out.write("    }.\n")
    else:
        out.write("    #{}.\n")
    return out.getvalue()


def _elixir_entry_literal(entry: RuntimeEntry) -> str:
    fields = [
        f"module: {entry['module']}",
        f"fun: :{entry['fun']}",
        f"arity: {entry['arity']}",
        f"js_kind: :{entry['js_kind']}",
    ]
    for optional_key in ("js_name", "js_class", "js_method"):
        if optional_key in entry:
            fields.append(f'{optional_key}: "{entry[optional_key]}"')
    if "arg_plan" in entry:
        fields.append(f"arg_plan: {_elixir_arg_plan(entry['arg_plan'])}")
    return "%{" + ", ".join(fields) + "}"


def _erlang_module_atom(elixir_module_name: str) -> str:
    return f"'Elixir.{elixir_module_name}'"


def _erlang_key(entry: RuntimeEntry) -> str:
    return f"{{{_erlang_module_atom(str(entry['module']))}, {entry['fun']}, {entry['arity']}}}"


def _erlang_entry_literal(entry: RuntimeEntry) -> str:
    fields = [
        f"module => {_erlang_module_atom(str(entry['module']))}",
        f"'fun' => {entry['fun']}",
        f"arity => {entry['arity']}",
        f"js_kind => {entry['js_kind']}",
    ]
    for optional_key in ("js_name", "js_class", "js_method"):
        if optional_key in entry:
            fields.append(f'{optional_key} => <<"{entry[optional_key]}">>')
    if "arg_plan" in entry:
        fields.append(f"arg_plan => {_erlang_arg_plan(entry['arg_plan'])}")
    return "#{" + ", ".join(fields) + "}"
