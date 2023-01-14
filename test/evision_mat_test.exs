defmodule Evision.Mat.Test do
  use ExUnit.Case

  alias Evision.Mat

  @tag :nx
  test "load an image from file and convert to Nx.tensor" do
    %Mat{} = mat = Evision.imread(Path.join([__DIR__, "testdata", "test.png"]))

    t = Evision.Mat.to_nx(mat)

    shape = Nx.shape(t)
    mat_shape = Evision.Mat.shape(mat)
    assert shape == mat_shape

    type = Nx.type(t)
    mat_type = Evision.Mat.type(mat)
    assert type == mat_type

    bin = Nx.to_binary(t)
    mat_bin = Evision.Mat.to_binary(mat)
    assert bin == mat_bin
  end

  @tag :nx
  test "load an image, convert to tensor and convert back from tensor" do
    %Mat{} = mat = Evision.imread(Path.join([__DIR__, "testdata", "test.png"]))

    t = Evision.Mat.to_nx(mat)

    %Mat{} = mat = Evision.Mat.from_nx(t)
    shape = Nx.shape(t)
    mat_shape = Evision.Mat.shape(mat)
    assert shape == mat_shape

    type = Nx.type(t)
    mat_type = Evision.Mat.type(mat)
    assert type == mat_type

    bin = Nx.to_binary(t)
    mat_bin = Evision.Mat.to_binary(mat)
    assert bin == mat_bin
  end

  @tag :nx
  test "convert from arbitrary tensor" do
    t = Nx.iota({2, 3, 2, 3, 2, 3}, type: {:s, 32})

    mat = Evision.Mat.from_nx(t)
    shape = Nx.shape(t)
    mat_shape = Evision.Mat.shape(mat)
    assert shape == mat_shape

    type = Nx.type(t)
    mat_type = Evision.Mat.type(mat)
    assert type == mat_type

    bin = Nx.to_binary(t)
    mat_bin = Evision.Mat.to_binary(mat)
    assert bin == mat_bin
  end

  @tag :nx
  test "expecting correct shaoe after transposing" do
    from_tensor = Evision.Mat.from_nx(Nx.iota({4, 3, 3}, type: :f32))
    assert {4, 3, 3} = Evision.Mat.shape(from_tensor)
    transposed = Evision.Mat.transpose(from_tensor, [1, 0, 2])
    assert {3, 4, 3} = Evision.Mat.shape(transposed)
  end

  @tag :nx
  test "transpose" do
    tensor =
      File.read!(Path.join([__DIR__, "testdata", "color_checker.etf"]))
      |> :erlang.binary_to_term()

    mat = Evision.Mat.from_nx(tensor, {297, 441, 3})
    assert {297, 441, 3} = Evision.Mat.shape(mat)

    transposed = Evision.Mat.transpose(mat, [1, 0, 2])
    assert {441, 297, 3} = Evision.Mat.shape(transposed)

    transposed = Evision.Mat.last_dim_as_channel(transposed)
    assert {441, 297, 3} = Evision.Mat.shape(transposed)

    bgr = Evision.cvtColor(transposed, Evision.cv_COLOR_RGB2BGR())
    assert {441, 297, 3} = Evision.Mat.shape(bgr)
  end

  @tag :nx
  test "update_roi" do
    image_tensor = Evision.Mat.literal([[[2,2,2]], [[2,3,4]], [[4,5,6]]], :u8)
    image_2d = Evision.Mat.last_dim_as_channel(image_tensor)
    Evision.Mat.to_nx(image_2d)

    patch_tensor = Evision.Mat.literal([[[7,8]], [[9,10]]], :u8)
    patch_2d = Evision.Mat.last_dim_as_channel(patch_tensor)
    Evision.Mat.to_nx(patch_2d)

    flatlist = [2, 7, 8, 2, 9, 10, 4, 5, 6]
    roi = [{0, 2}, {0, 1}, {1, 3}]
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_2d)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_tensor)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_2d)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_tensor)))

    flatlist = [2, 2, 2, 2, 7, 8, 4, 9, 10]
    roi = [{1, 3}, {0, 1}, {1, 3}]
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_2d)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_tensor)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_2d)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_tensor)))

    flatlist = [2, 2, 2, 7, 8, 4, 9, 10, 6]
    roi = [{1, 3}, {0, 1}, {0, 2}]
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_2d)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_tensor)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_2d)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_tensor)))

    flatlist = [7, 8, 2, 9, 10, 4, 4, 5, 6]
    roi = [{0, 2}, {0, 1}, {0, 2}]
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_2d)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_tensor)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_2d)))
    assert flatlist == Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_tensor)))
  end
end
