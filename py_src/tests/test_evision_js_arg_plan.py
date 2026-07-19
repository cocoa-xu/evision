#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Tests for the opencv.js ``arg_plan`` emitted into the ``@js`` tag (CCD-54).

The arg-plan is the positional opencv.js argument layout — ``in`` for each
required input, ``out`` for each pre-allocated ``OutputArray`` (dst) — that the
CCD-35 workflow→opencv.js compiler consumes to place the dst Mat. evision/Elixir
returns the output (it is not a positional arg), so the plan is the only place
the C++ positional order survives into ``Evision.JS``.

These exercise the pure derivation (``ModuleGenerator._compute_arg_plans`` /
``_variant_arg_plan``) and the rendering emitters (``evision_js_attr``) against
synthetic ``FuncInfo`` variants, so they need no OpenCV build.
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import evision_js_attr as JA  # noqa: E402
import module_generator as MG  # noqa: E402
from ir.functions import FuncInfo  # noqa: E402


def _decl(name, args, rettype="void"):
    # args: list of (type, name, defval, modifiers). "" defval => required input;
    # "/O" modifier => OutputArray. Mirrors hdr_parser's decl shape.
    arg_decls = [[t, n, d, list(m)] for (t, n, d, m) in args]
    return [name, rettype, [], arg_decls, None, ""]


def _func(classname, name, variants, is_static=False, namespace="cv"):
    f = FuncInfo(classname, name, name, False, namespace, is_static)
    for decl in variants:
        f.add_variant(decl)
    return f


@pytest.fixture
def gen():
    # Bypass __init__ (it wants many codegen args); the arg-plan helpers only
    # touch their FuncInfo argument, not instance state.
    return MG.ModuleGenerator.__new__(MG.ModuleGenerator)


# ---- derivation: the curated D25 palette -------------------------------------


def test_cvt_color_dst_after_src(gen):
    # cv.cvtColor(src, dst, code) — dst is OutputArray at position 1.
    f = _func("", "cvtColor", [
        _decl("cvtColor", [("Mat", "src", "", ()), ("Mat", "dst", "", ("/O",)),
                           ("int", "code", "", ())]),
        _decl("cvtColor", [("Mat", "src", "", ()), ("Mat", "dst", "", ("/O",)),
                           ("int", "code", "", ()), ("int", "dstCn", "0", ())]),
    ])
    assert gen._compute_arg_plans(f, False) == {2: ["in", "out", "in"]}


def test_add_dst_last(gen):
    # cv.add(src1, src2, dst, mask=, dtype=) — dst trails the two inputs.
    f = _func("", "add", [
        _decl("add", [("Mat", "src1", "", ()), ("Mat", "src2", "", ()),
                      ("Mat", "dst", "", ("/O",)), ("Mat", "mask", "Mat()", ()),
                      ("int", "dtype", "-1", ())]),
    ])
    assert gen._compute_arg_plans(f, False) == {2: ["in", "in", "out"]}


def test_gaussian_blur(gen):
    f = _func("", "GaussianBlur", [
        _decl("GaussianBlur", [("Mat", "src", "", ()), ("Mat", "dst", "", ("/O",)),
                               ("Size", "ksize", "", ()), ("double", "sigmaX", "", ()),
                               ("double", "sigmaY", "0", ()),
                               ("int", "borderType", "BORDER_DEFAULT", ())]),
    ])
    assert gen._compute_arg_plans(f, False) == {3: ["in", "out", "in", "in"]}


def test_threshold_with_retval(gen):
    # threshold returns a double (retval) and still has a dst OutputArray.
    f = _func("", "threshold", [
        _decl("threshold", [("Mat", "src", "", ()), ("Mat", "dst", "", ("/O",)),
                            ("double", "thresh", "", ()), ("double", "maxval", "", ()),
                            ("int", "type", "", ())], rettype="double"),
    ])
    assert gen._compute_arg_plans(f, False) == {4: ["in", "out", "in", "in", "in"]}


def test_canny_3_kept_but_4_is_ambiguous(gen):
    # Two variants collide at arity 4 with different layouts, so 4 is omitted;
    # the unambiguous arity-3 plan is still emitted.
    #   image, edges/O, t1, t2, apertureSize=3, L2gradient=false  -> required 3 (+opts 4)
    #   dx, dy, edges/O, t1, t2, L2gradient=false                 -> required 4
    f = _func("", "Canny", [
        _decl("Canny", [("Mat", "image", "", ()), ("Mat", "edges", "", ("/O",)),
                        ("double", "threshold1", "", ()), ("double", "threshold2", "", ()),
                        ("int", "apertureSize", "3", ()), ("bool", "L2gradient", "false", ())]),
        _decl("Canny", [("Mat", "dx", "", ()), ("Mat", "dy", "", ()),
                        ("Mat", "edges", "", ("/O",)), ("double", "threshold1", "", ()),
                        ("double", "threshold2", "", ()), ("bool", "L2gradient", "false", ())]),
    ])
    plans = gen._compute_arg_plans(f, False)
    assert plans.get(3) == ["in", "out", "in", "in"]
    assert 4 not in plans


# ---- derivation: method receiver + constructor ------------------------------


def test_method_excludes_receiver(gen):
    # cv.Tonemap::process(src, dst) — the receiver is the implicit `this`, not in
    # v.args, so the plan covers only the post-receiver args. The Elixir wrapper
    # is process(self, src) => arity 2.
    f = _func("Tonemap", "process", [
        _decl("process", [("Mat", "src", "", ()), ("Mat", "dst", "", ("/O",))]),
    ])
    assert gen._compute_arg_plans(f, False) == {2: ["in", "out"]}


def test_constructor_without_io_args_has_no_plan(gen):
    f = _func("UsacParams", "UsacParams", [_decl("UsacParams", [])])
    assert gen._compute_arg_plans(f, True) == {}


def test_optional_inputs_excluded_from_required_plan(gen):
    # Only required inputs occupy a positional `in` slot; an input with a
    # default value (mask, dtype, dstCn, …) rides the `opts` keyword bag and so
    # must not appear in the required-arity plan. Two variants that agree on the
    # required layout (differing only in an optional trailing arg) are NOT
    # ambiguous — they keep the shared plan.
    f = _func("", "fake", [
        _decl("fake", [("Mat", "src", "", ()), ("Mat", "dst", "", ("/O",)),
                       ("int", "code", "", ())]),
        _decl("fake", [("Mat", "src", "", ()), ("Mat", "dst", "", ("/O",)),
                       ("int", "code", "", ()), ("int", "extra", "0", ())]),
    ])
    assert gen._compute_arg_plans(f, False) == {2: ["in", "out", "in"]}


# ---- emitters: Elixir + Erlang rendering ------------------------------------


_FN = {"module": "Evision", "fun": "cvtColor", "arity": 2, "js_kind": "function",
       "js_name": "cv.cvtColor", "arg_plan": ["in", "out", "in"]}
_METHOD = {"module": "Evision.Tonemap", "fun": "process", "arity": 2,
           "js_kind": "method", "js_class": "cv.Tonemap", "js_method": "process",
           "arg_plan": ["in", "out"]}


def test_elixir_attribute_line_renders_arg_plan():
    line = JA.elixir_attribute_line(_FN, "cvtColor", 2)
    assert "arg_plan: [:in, :out, :in]" in line


def test_erlang_attribute_line_renders_arg_plan():
    line = JA.erlang_attribute_line(_FN, "cvtColor", 2)
    assert "arg_plan => [in, out, in]" in line


def test_elixir_entry_literal_renders_arg_plan():
    assert JA._elixir_entry_literal(_FN) == (
        '%{module: Evision, fun: :cvtColor, arity: 2, js_kind: :function, '
        'js_name: "cv.cvtColor", arg_plan: [:in, :out, :in]}'
    )


def test_erlang_entry_literal_renders_arg_plan():
    assert JA._erlang_entry_literal(_METHOD) == (
        "#{module => 'Elixir.Evision.Tonemap', 'fun' => process, arity => 2, "
        'js_kind => method, js_class => <<"cv.Tonemap">>, '
        'js_method => <<"process">>, arg_plan => [in, out]}'
    )


def test_collect_runtime_entries_carries_arg_plan():
    class _MG:
        module_name = "Evision"
        js_attrs = {"elixir": {"cvtColor": {2: dict(_FN)}}}

    [entry] = JA.collect_runtime_entries([_MG()], "elixir")
    assert entry["arg_plan"] == ["in", "out", "in"]


# ---- the additive guarantee: no arg_plan => byte-identical to pre-change -----


_NO_PLAN = {"module": "Evision", "fun": "canny", "arity": 4, "js_kind": "function",
            "js_name": "cv.Canny"}


def test_elixir_literal_without_plan_is_unchanged():
    assert JA._elixir_entry_literal(_NO_PLAN) == (
        '%{module: Evision, fun: :canny, arity: 4, js_kind: :function, '
        'js_name: "cv.Canny"}'
    )


def test_erlang_literal_without_plan_is_unchanged():
    assert JA._erlang_entry_literal(_NO_PLAN) == (
        "#{module => 'Elixir.Evision', 'fun' => canny, arity => 4, "
        'js_kind => function, js_name => <<"cv.Canny">>}'
    )


def test_attribute_lines_without_plan_are_unchanged():
    assert JA.elixir_attribute_line(_NO_PLAN, "canny", 4) == (
        '  @js %{fun: :canny, arity: 4, js_kind: :function, js_name: "cv.Canny"}\n'
    )
    assert JA.erlang_attribute_line(_NO_PLAN, "canny", 4) == (
        '-js(#{fun => canny, arity => 4, js_kind => function, '
        'js_name => <<"cv.Canny">>}).\n'
    )


# ---- @type contracts grow the optional field --------------------------------


def test_runtime_module_type_blocks_document_arg_plan():
    assert "optional(:arg_plan) => [:in | :out]" in JA.elixir_runtime_module([_NO_PLAN])
    assert "arg_plan => [in | out]" in JA.erlang_runtime_module([_NO_PLAN])


# ---- opencv.js asset URL helper ---------------------------------------------


def test_runtime_modules_emit_opencv_js_url():
    ex = JA.elixir_runtime_module([_NO_PLAN])
    assert '@github_url "https://github.com/cocoa-xu/evision"' in ex
    assert "def opencv_js_url(version \\\\ nil) do" in ex
    assert 'to_string(Application.spec(:evision, :vsn))' in ex
    assert '"#{@github_url}/releases/download/v#{version}/opencv.js"' in ex

    erl = JA.erlang_runtime_module([_NO_PLAN])
    assert "opencv_js_url() ->" in erl
    assert "application:get_key(evision, vsn)" in erl
    assert (
        'iolist_to_binary([<<"https://github.com/cocoa-xu/evision/releases/download/v">>,'
        " Version, <<\"/opencv.js\">>])"
    ) in erl


def test_runtime_modules_emit_opencv_js_path():
    ex = JA.elixir_runtime_module([_NO_PLAN])
    assert "def opencv_js_path do" in ex
    assert "case :code.priv_dir(:evision) do" in ex
    assert 'Path.join(to_string(dir), "opencv.js")' in ex

    erl = JA.erlang_runtime_module([_NO_PLAN])
    assert "opencv_js_path() ->" in erl
    assert "code:priv_dir(evision)" in erl
    assert 'filename:join(Dir, "opencv.js")' in erl
