defmodule Evision.Internal.Structurise do
  def to_struct(any)

  def to_struct({:ok, ret}), do: to_struct_ok(ret)

  def to_struct(mat = %{:class => :Mat}) do
    Evision.Mat.__make_struct__(mat)
  end

  def to_struct(pass_through), do: pass_through

  def to_struct_ok(mat = %{:class => :Mat}) do
    {:ok, Evision.Mat.__make_struct__(mat)}
  end

  def to_struct_ok(pass_through), do: {:ok, pass_through}

  def from_struct(%Evision.Mat{ref: ref}) do
    ref
  end

  def from_struct(pass_through), do: pass_through
end
