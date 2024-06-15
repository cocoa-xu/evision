defmodule Evision.Mat.Test do
  use ExUnit.Case

  alias Evision.Mat

  describe "arithmetic ops" do
    @tag :nx
    test "arithmetic op src should behave the same as in C++/Python" do
      shape = {3, 4, 3}
      mat = Evision.Mat.zeros(shape, :u8)
      zeros = Nx.tensor(0, type: :u8) |> Nx.broadcast(shape)
      assert 1 == Nx.to_number(Nx.all_close(
        zeros,
        Evision.Mat.to_nx(mat, {Nx.BinaryBackend, []})
      ))

      expected_results = Nx.tensor(1, type: :u8) |> Nx.broadcast(shape)
      result = Evision.add(mat, Nx.tensor([1], type: :u8))
      assert 1 == Nx.to_number(Nx.all_close(
        expected_results,
        Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
      ))

      result = Evision.add(mat, Nx.tensor(1, type: :u8))
      assert 1 == Nx.to_number(Nx.all_close(
        expected_results,
        Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
      ))

      result = Evision.add(mat, 1)
      assert 1 == Nx.to_number(Nx.all_close(
        expected_results,
        Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
      ))
    end

    @tag :nx
    test "arithmetic op src with :f16" do
      mat_val = 1
      tensor_val = 0.3456
      result = Evision.add(mat_val, Nx.tensor(tensor_val, type: :f16))

      expected_results = Nx.tensor(mat_val + tensor_val, type: :f16)
      assert 1 == Nx.to_number(Nx.all_close(
        expected_results,
        Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
      ))
    end

    @tag :nx
    test "arithmetic op src, int + int" do
      # >>> cv2.add(1, 1)
      # array([[2.],
      #       [2.],
      #       [2.],
      #       [2.]])
      mat_val = 1
      result = Evision.add(mat_val, mat_val)

      expected_results = Nx.tensor([[2], [2], [2], [2]], type: :f64)
      assert 1 == Nx.to_number(Nx.all_close(
        expected_results,
        Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
      ))
    end

    @tag :nx
    test "arithmetic op src, int + tuple" do
      # >>> cv2.add(1, (1, 2.2, 3.3))
      # array([[2.],
      #        [3.2],
      #        [4.3],
      #        [1.]])
      mat_val = 1
      result = Evision.add(mat_val, {1, 2.2, 3.3})

      expected_results = Nx.tensor([[2.0], [3.2], [4.3], [1]], type: :f64)
      assert 1 == Nx.to_number(Nx.all_close(
        expected_results,
        Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
      ))
    end

    @tag :nx
    test "arithmetic op src, 3-tuples + 3-tuples" do
      # >>> cv2.add((1,2,3), (4,5,6))
      # array([[5.],
      #        [7.],
      #        [9.0],
      #        [0.]])
      result = Evision.add({1,2,3}, {4,5,6})

      expected_results = Nx.tensor([[5], [7], [9], [0]], type: :f64)
      assert 1 == Nx.to_number(Nx.all_close(
        expected_results,
        Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
      ))
    end

    @tag :nx
    test "arithmetic op src, 3-tuple + 4-tuple" do
      # >>> import cv2
      # >>> import numpy as np
      # >>> cv2.add((1,2,3), (1,2,3,4))
      # array([[2.],
      #        [4.],
      #        [6.],
      #        [4.]])
      result = Evision.add({1,2,3}, {1,2,3,4})

      expected_results = Nx.tensor([[2], [4], [6], [4]], type: :f64)
      assert 1 == Nx.to_number(Nx.all_close(
        expected_results,
        Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
      ))
    end

    @tag :nx
    test "arithmetic op src, 3-tuple + 5-tuple" do
      # >>> import cv2
      # >>> import numpy as np
      # >>> cv2.add((1,2,3), (4,5,6,7,8))
      # array([[5.],
      #        [6.],
      #        [7.],
      #        [8.]
      #        [9.]])
      result = Evision.add({1,2,3}, {4,5,6,7,8})

      expected_results = Nx.tensor([[5], [6], [7], [8], [9]], type: :f64)
      assert 1 == Nx.to_number(Nx.all_close(
        expected_results,
        Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
      ))
    end
  end

  @tag :nx
  test "use Nx.tensor without calling last_dim_as_channel" do
    v = Nx.broadcast(Nx.tensor(1.0, type: :f64), {120, 120, 3})

    assert %Evision.Mat{
             channels: 3,
             dims: 2,
             type: {:f, 64},
             raw_type: 22,
             shape: {120, 120, 3}
           } = Evision.gaussianBlur(v, {31, 31}, 0)
  end

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

    bgr = Evision.cvtColor(transposed, Evision.Constant.cv_COLOR_RGB2BGR())
    assert {441, 297, 3} = Evision.Mat.shape(bgr)
  end

  @tag :nx
  test "update_roi" do
    image_tensor = Evision.Mat.literal([[[2, 2, 2]], [[2, 3, 4]], [[4, 5, 6]]], :u8)
    image_2d = Evision.Mat.last_dim_as_channel(image_tensor)
    Evision.Mat.to_nx(image_2d)

    patch_tensor = Evision.Mat.literal([[[7, 8]], [[9, 10]]], :u8)
    patch_2d = Evision.Mat.last_dim_as_channel(patch_tensor)
    Evision.Mat.to_nx(patch_2d)

    flatlist = [2, 7, 8, 2, 9, 10, 4, 5, 6]
    roi = [{0, 2}, {0, 1}, {1, 3}]

    assert flatlist ==
             Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_2d)))

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_tensor))
             )

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_2d))
             )

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_tensor))
             )

    flatlist = [2, 2, 2, 2, 7, 8, 4, 9, 10]
    roi = [{1, 3}, {0, 1}, {1, 3}]

    assert flatlist ==
             Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_2d)))

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_tensor))
             )

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_2d))
             )

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_tensor))
             )

    flatlist = [2, 2, 2, 7, 8, 4, 9, 10, 6]
    roi = [{1, 3}, {0, 1}, {0, 2}]

    assert flatlist ==
             Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_2d)))

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_tensor))
             )

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_2d))
             )

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_tensor))
             )

    flatlist = [7, 8, 2, 9, 10, 4, 4, 5, 6]
    roi = [{0, 2}, {0, 1}, {0, 2}]

    assert flatlist ==
             Nx.to_flat_list(Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_2d)))

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_2d, roi, patch_tensor))
             )

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_2d))
             )

    assert flatlist ==
             Nx.to_flat_list(
               Evision.Mat.to_nx(Evision.Mat.update_roi(image_tensor, roi, patch_tensor))
             )
  end
end
