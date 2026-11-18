defmodule Evision.ErlangStructurise.Test do
  use ExUnit.Case

  @compile {:no_warn_undefined, [:evision_internal_structurise, :evision_mat]}

  @generated_erl_dir Path.join([File.cwd!(), "src", "generated"])
  @skip_without_erlang not File.exists?(Path.join(@generated_erl_dir, "evision_dnn_net.erl")) &&
                         "Erlang sources not generated (EVISION_GENERATE_LANG is elixir-only)"

  setup_all do
    unless @skip_without_erlang do
      for source <- [
            "src/evision_internal_structurise.erl",
            "src/evision_mat.erl",
            "src/generated/evision_dnn_net.erl"
          ] do
        {:ok, module, beam} =
          :compile.file(String.to_charlist(source), [:binary, i: ~c"src/generated"])

        {:module, ^module} = :code.load_binary(module, String.to_charlist(source), beam)
      end
    end

    :ok
  end

  @tag skip: @skip_without_erlang
  test "namespaced classes become their Erlang records" do
    ref = make_ref()

    assert {:evision_dnn_net, ref} ==
             :evision_internal_structurise.to_struct(%{class: Evision.DNN.Net, ref: ref})

    assert ref == :evision_internal_structurise.from_struct({:evision_dnn_net, ref})
  end

  @tag skip: @skip_without_erlang
  test "option tuples pass through from_struct" do
    ref = make_ref()
    opts = [{:outBlobNames, ["boxes", "scores"]}, {:img, {:evision_dnn_net, ref}}]

    assert [{:outBlobNames, ["boxes", "scores"]}, {:img, ref}] ==
             :evision_internal_structurise.from_struct(opts)
  end

  @tag skip: @skip_without_erlang
  test "evision_mat:from_binary_by_shape builds a Mat" do
    mat = :evision_mat.from_binary_by_shape(<<1, 2, 3, 4>>, {:u, 8}, {2, 2})
    assert <<1, 2, 3, 4>> == :evision_mat.to_binary(mat)
  end
end
