"""The Elixir/Erlang property setters and the C++ setter NIFs must agree on
how the new value is passed: the BEAM side sends `[member: value]`, so every
C++ setter template has to unwrap it with `evision_get_kw` rather than
converting `argv[1]` directly.
"""

import os
import re
import sys
from string import Template

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import evision_templates as ET


SUBSTITUTIONS = dict(
    name="Foo",
    cname="cv::Foo",
    member="bar",
    membertype="int",
    access="->",
    storage_name="cv::Foo",
    elixir_module_name="Foo",
    property_name="bar",
    nif_name="foo_set_bar",
    self_spec_in="Evision.Foo.t()",
    self_spec_out="Evision.Foo.t()",
    prop_spec="integer()",
)

CPP_SETTER_TEMPLATES = sorted(
    name for name, value in vars(ET).items()
    if name.startswith("gen_template_set_prop")
    and name != "gen_template_set_prop_from_map"
    and isinstance(value, Template)
)


def render(template_name):
    return getattr(ET, template_name).substitute(SUBSTITUTIONS)


def test_all_cpp_setter_templates_are_discovered():
    assert {"gen_template_set_prop", "gen_template_set_prop_algo",
            "gen_template_set_prop_cv_ptr"} <= set(CPP_SETTER_TEMPLATES)


@pytest.mark.parametrize("template_name", CPP_SETTER_TEMPLATES)
def test_cpp_setter_unwraps_keyword_arg(template_name):
    code = render(template_name)
    assert "evision::nif::parse_arg(env, 1, argv, erl_terms);" in code
    assert 'evision_to_safe(env, evision_get_kw(env, erl_terms, "bar"),' in code
    assert not re.search(r"evision_to_safe\(env,\s*argv\[1\]", code)


def test_elixir_setter_sends_keyword_arg():
    code = ET.elixir_property_setter.substitute(SUBSTITUTIONS)
    assert "[bar: Evision.Internal.Structurise.from_struct(prop)]" in code


def test_erlang_setter_sends_proplist():
    code = ET.erlang_property_setter.substitute(SUBSTITUTIONS)
    assert "evision_nif:foo_set_bar(SelfRef, [{bar, PropRef}])" in code
