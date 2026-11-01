import os
import shutil
import subprocess
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from module_generator import _dnn_net_layer_shapes_spec  # noqa: E402

FUNCS = ["getLayerShapes", "getLayersShapes"]


@pytest.mark.parametrize("func_name", FUNCS)
def test_spec_names_its_own_function(func_name):
    assert _dnn_net_layer_shapes_spec(func_name).startswith(f"@spec {func_name}(")


@pytest.mark.parametrize("func_name", FUNCS)
def test_spec_takes_a_keyword_list(func_name):
    assert "(Evision.DNN.Net.t(), [{atom(), term()},...] | nil)" in _dnn_net_layer_shapes_spec(func_name)


@pytest.mark.skipif(shutil.which("elixir") is None, reason="elixir not installed")
def test_specs_compile(tmp_path):
    src = tmp_path / "spec.exs"
    src.write_text(
        "defmodule Evision.DNN.Net, do: (@type t :: %__MODULE__{}; defstruct [])\n"
        + "".join(
            f"defmodule Stub{i} do\n"
            f"  {_dnn_net_layer_shapes_spec(func_name)}\n"
            f"  def {func_name}(_self, _opts \\\\ nil), do: {{:error, \"\"}}\n"
            "end\n"
            for i, func_name in enumerate(FUNCS)
        )
    )
    result = subprocess.run(["elixir", str(src)], capture_output=True, text=True)
    assert result.returncode == 0, result.stdout + result.stderr
