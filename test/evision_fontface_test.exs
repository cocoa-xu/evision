defmodule Evision.FontFace.Test do
  use ExUnit.Case, async: true

  test "the built-in sans, uni and italic fonts are available" do
    for name <- ["sans", "uni", "italic"] do
      assert name == name |> Evision.FontFace.fontFace() |> Evision.FontFace.getName()
    end
  end
end
