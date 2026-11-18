defmodule Evision.Mat.Image.Test do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Evision.Mat

  defp shape_of(%Mat{dims: dims, channels: channels, shape: shape}), do: {dims, channels, shape}

  describe "to_2d_image/1" do
    test "returns a 2D Mat with 1, 3 or 4 channels as is" do
      mat = Mat.zeros({2, 3, 3}, :u8) |> Mat.last_dim_as_channel()
      assert Mat.to_2d_image(mat) == mat
    end

    test "turns a 3-D single-channel Mat into a multi-channel 2D Mat" do
      for c <- [1, 3, 4] do
        mat = Mat.zeros({2, 3, c}, :u8)
        assert {2, ^c, _} = shape_of(Mat.to_2d_image(mat))
      end
    end

    test "accepts tensors" do
      assert {2, 1, {2, 3}} = shape_of(Mat.to_2d_image(Nx.iota({2, 3}, type: :u8)))
      assert {2, 4, {2, 3, 4}} = shape_of(Mat.to_2d_image(Nx.iota({2, 3, 4}, type: :u8)))
    end

    test "rejects anything that is not a 1, 3 or 4 channel image" do
      assert {:error, _} = Mat.to_2d_image(Mat.from_binary(<<1, 2>>, {:u, 8}, 1, 1, 2))
      assert {:error, _} = Mat.to_2d_image(Mat.zeros({2, 3, 2}, :u8))
      assert {:error, _} = Mat.to_2d_image(Mat.zeros({2, 3, 4, 1}, :u8))
      assert {:error, _} = Mat.to_2d_image(Nx.iota({2, 3, 5}, type: :u8))
      assert {:error, _} = Mat.to_2d_image(Nx.iota({6}, type: :u8))
    end
  end

  describe "to_rgb/1" do
    test "converts GRAY, BGR and BGRA to RGB" do
      gray = Mat.from_binary(<<7, 9>>, {:u, 8}, 1, 2, 1)
      assert <<7, 7, 7, 9, 9, 9>> = Mat.to_binary(Mat.to_rgb(gray))

      bgr = Mat.from_binary(<<1, 2, 3>>, {:u, 8}, 1, 1, 3)
      assert <<3, 2, 1>> = Mat.to_binary(Mat.to_rgb(bgr))

      bgra = Mat.from_binary(<<1, 2, 3, 255>>, {:u, 8}, 1, 1, 4)
      assert <<3, 2, 1>> = Mat.to_binary(Mat.to_rgb(bgra))
    end

    test "accepts what to_2d_image/1 accepts" do
      assert <<3, 2, 1>> = Mat.to_binary(Mat.to_rgb(Nx.tensor([[[1, 2, 3]]], type: :u8)))
      assert {:error, _} = Mat.to_rgb(Mat.zeros({2, 3, 2}, :u8))
    end
  end

  describe "quicklook/1" do
    setup do
      env = Application.fetch_env(:evision, :display_inline_image_iterm2)
      lc_terminal = System.get_env("LC_TERMINAL")
      term = System.get_env("TERM")

      Application.put_env(:evision, :display_inline_image_iterm2, true)
      System.put_env("LC_TERMINAL", "iTerm2")
      System.put_env("TERM", "xterm-256color")

      on_exit(fn ->
        case env do
          {:ok, value} -> Application.put_env(:evision, :display_inline_image_iterm2, value)
          :error -> Application.delete_env(:evision, :display_inline_image_iterm2)
        end

        if lc_terminal,
          do: System.put_env("LC_TERMINAL", lc_terminal),
          else: System.delete_env("LC_TERMINAL")

        if term, do: System.put_env("TERM", term), else: System.delete_env("TERM")
      end)
    end

    test "shows a 3-D single-channel Mat" do
      mat = Mat.zeros({2, 3, 1}, :u8)
      assert capture_io(fn -> assert Mat.quicklook(mat) == mat end) =~ "]1337;File="
    end

    test "skips a tensor that is not an image" do
      tensor = Nx.iota({6}, type: :u8)
      assert capture_io(fn -> assert Mat.quicklook(tensor) == tensor end) == ""
    end
  end

  test "Kino renders a 3-D single-channel Mat as an image" do
    mat = Mat.zeros({2, 3, 1}, :u8)
    assert %{labels: ["Image" | _]} = Kino.Render.to_livebook(mat)
  end
end
