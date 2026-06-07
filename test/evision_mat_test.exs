defmodule Evision.Mat.Test do
  use ExUnit.Case

  alias Evision.Mat

  describe "arithmetic ops" do
    @tag :nx
    test "arithmetic op src should behave the same as in C++/Python" do
      shape = {3, 4, 3}
      mat = Evision.Mat.zeros(shape, :u8)
      zeros = Nx.tensor(0, type: :u8) |> Nx.broadcast(shape)

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   zeros,
                   Evision.Mat.to_nx(mat, {Nx.BinaryBackend, []})
                 )
               )

      expected_results = Nx.tensor(1, type: :u8) |> Nx.broadcast(shape)
      result = Evision.add(mat, Nx.tensor([1], type: :u8))

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   expected_results,
                   Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
                 )
               )

      result = Evision.add(mat, Nx.tensor(1, type: :u8))

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   expected_results,
                   Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
                 )
               )

      result = Evision.add(mat, 1)

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   expected_results,
                   Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
                 )
               )
    end

    @tag :nx
    test "arithmetic op src with :f16" do
      mat_val = 1
      tensor_val = 0.3456
      result = Evision.add(mat_val, Nx.tensor(tensor_val, type: :f16))

      expected_results = Nx.tensor(mat_val + tensor_val, type: :f16)

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   expected_results,
                   Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
                 )
               )
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

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   expected_results,
                   Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
                 )
               )
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

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   expected_results,
                   Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
                 )
               )
    end

    @tag :nx
    test "arithmetic op src, 3-tuples + 3-tuples" do
      # >>> cv2.add((1,2,3), (4,5,6))
      # array([[5.],
      #        [7.],
      #        [9.0],
      #        [0.]])
      result = Evision.add({1, 2, 3}, {4, 5, 6})

      expected_results = Nx.tensor([[5], [7], [9], [0]], type: :f64)

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   expected_results,
                   Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
                 )
               )
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
      result = Evision.add({1, 2, 3}, {1, 2, 3, 4})

      expected_results = Nx.tensor([[2], [4], [6], [4]], type: :f64)

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   expected_results,
                   Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
                 )
               )
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
      result = Evision.add({1, 2, 3}, {4, 5, 6, 7, 8})

      expected_results = Nx.tensor([[5], [6], [7], [8], [9]], type: :f64)

      assert 1 ==
               Nx.to_number(
                 Nx.all_close(
                   expected_results,
                   Evision.Mat.to_nx(result, {Nx.BinaryBackend, []})
                 )
               )
    end
  end

  describe "native mat helper regressions" do
    test "clip clamps values to both bounds" do
      mat = Evision.Mat.from_binary_by_shape(<<0, 5, 10>>, {:u, 8}, {3})

      clipped = Evision.Mat.clip(mat, 2, 6)

      assert Evision.Mat.to_binary(clipped) == <<2, 5, 6>>
    end

    test "arange keeps the last stepped value when the interval is not evenly divisible" do
      mat = Evision.Mat.arange(0, 5, 2, :s32)

      expected =
        for value <- [0, 2, 4], into: <<>> do
          <<value::signed-little-size(32)>>
        end

      assert Evision.Mat.to_binary(mat) == expected
    end

    test "to_binary respects the element limit" do
      mat = Evision.Mat.from_binary_by_shape(<<1, 2, 3, 4>>, {:u, 8}, {4})

      assert Evision.Mat.to_binary(mat, 2) == <<1, 2>>
    end

    test "from_binary_by_shape rejects binaries smaller than the requested shape" do
      assert {:error, "size mismatch"} =
               Evision.Mat.from_binary_by_shape(<<1, 2>>, {:u, 8}, {2, 2})
    end

    test "to_batched repeat fills batches without reading past the source matrix" do
      mat = Evision.Mat.from_binary_by_shape(<<1, 2>>, {:u, 8}, {2})

      assert [batch] = Evision.Mat.to_batched(mat, 8, {2}, leftover: :repeat)
      assert Evision.Mat.to_binary(batch) == <<1, 2, 1, 2, 1, 2, 1, 2>>
    end

    test "to_batched 4-arg requires multi-channel shape to include channels as a dim" do
      # CV_8UC3 mat: 2 rows x 2 cols x 3 channels = 12 bytes
      bin = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12>>
      mat = Evision.Mat.from_binary(bin, {:u, 8}, 2, 2, 3)

      # Shape including channels works; output is single-channel with channels as last dim
      assert [batch] = Evision.Mat.to_batched(mat, 2, {2, 2, 3}, leftover: :discard)
      assert Evision.Mat.shape(batch) == {2, 2, 3}
      assert Evision.Mat.channels(batch) == 1
      assert Evision.Mat.to_binary(batch) == bin

      # Shape WITHOUT channels is rejected — this is the breaking change vs. older versions
      assert {:error, msg} = Evision.Mat.to_batched(mat, 2, {2, 2}, leftover: :discard)
      assert msg =~ "shape"
    end

    test "to_batched 3-arg works on multi-channel Mat (previously errored)" do
      bin = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12>>
      mat = Evision.Mat.from_binary(bin, {:u, 8}, 2, 2, 3)

      assert [batch] = Evision.Mat.to_batched(mat, 2, leftover: :discard)
      assert Evision.Mat.shape(batch) == {2, 2, 3}
      assert Evision.Mat.channels(batch) == 1
      assert Evision.Mat.to_binary(batch) == bin
    end

    test "update_roi does not mutate the caller's matrix" do
      original_bin = <<1, 2, 3, 4, 5, 6, 7, 8, 9>>
      mat = Evision.Mat.from_binary_by_shape(original_bin, {:u, 8}, {3, 3})
      patch = Evision.Mat.from_binary_by_shape(<<0, 0>>, {:u, 8}, {1, 2})

      updated = Evision.Mat.update_roi(mat, [{1, 2}, {0, 2}], patch)

      assert Evision.Mat.to_binary(updated) == <<1, 2, 3, 0, 0, 6, 7, 8, 9>>
      assert Evision.Mat.to_binary(mat) == original_bin
    end

    test "dot returns a scalar matching OpenCV Mat::dot semantics" do
      a_bin = for x <- [1.0, 2.0, 3.0], into: <<>>, do: <<x::float-little-size(64)>>
      b_bin = for x <- [4.0, 5.0, 6.0], into: <<>>, do: <<x::float-little-size(64)>>
      a = Evision.Mat.from_binary_by_shape(a_bin, {:f, 64}, {3})
      b = Evision.Mat.from_binary_by_shape(b_bin, {:f, 64}, {3})

      assert Evision.Mat.dot(a, b) == 32.0
    end

    test "dot treats matrices as flat 1D vectors in row-major order" do
      values = for v <- [1.0, 2.0, 3.0, 4.0], into: <<>>, do: <<v::float-little-size(64)>>
      mat = Evision.Mat.from_binary_by_shape(values, {:f, 64}, {2, 2})

      assert Evision.Mat.dot(mat, mat) == 1.0 + 4.0 + 9.0 + 16.0
    end

    test "matmul performs 2D matrix multiplication" do
      a_bin = for v <- [1.0, 2.0, 3.0, 4.0], into: <<>>, do: <<v::float-little-size(64)>>
      b_bin = for v <- [5.0, 6.0, 7.0, 8.0], into: <<>>, do: <<v::float-little-size(64)>>
      a = Evision.Mat.from_binary_by_shape(a_bin, {:f, 64}, {2, 2})
      b = Evision.Mat.from_binary_by_shape(b_bin, {:f, 64}, {2, 2})

      product = Evision.Mat.matmul(a, b)
      expected = for v <- [19.0, 22.0, 43.0, 50.0], into: <<>>, do: <<v::float-little-size(64)>>

      assert Evision.Mat.to_binary(product) == expected
      assert Evision.Mat.shape(product) == {2, 2}
    end

    test "matmul rejects shape mismatch" do
      a_bin = for v <- [1.0, 2.0, 3.0, 4.0], into: <<>>, do: <<v::float-little-size(32)>>
      b_bin = for v <- [1.0, 2.0, 3.0], into: <<>>, do: <<v::float-little-size(32)>>
      a = Evision.Mat.from_binary_by_shape(a_bin, {:f, 32}, {2, 2})
      b = Evision.Mat.from_binary_by_shape(b_bin, {:f, 32}, {3, 1})

      assert {:error, _msg} = Evision.Mat.matmul(a, b)
    end

    test "transpose rejects as_shape whose byte size does not match the source" do
      mat = Evision.Mat.from_binary_by_shape(<<1, 2, 3, 4>>, {:u, 8}, {4})

      assert {:error, msg} = Evision.Mat.transpose(mat, [1, 0], as_shape: {2, 3})
      assert msg =~ "as_shape"
    end

    test "at handles non-continuous matrices and returns the right scalar" do
      bin = for v <- 1..16, into: <<>>, do: <<v::little-size(8)>>
      mat = Evision.Mat.from_binary_by_shape(bin, {:u, 8}, {4, 4})

      assert Evision.Mat.at(mat, 0) == 1
      assert Evision.Mat.at(mat, 15) == 16
      assert {:error, _} = Evision.Mat.at(mat, 16)
    end

    test "arange supports negative step" do
      mat = Evision.Mat.arange(10, 0, -3, :s32)

      expected =
        for v <- [10, 7, 4, 1], into: <<>>, do: <<v::signed-little-size(32)>>

      assert Evision.Mat.to_binary(mat) == expected
    end

    test "arange rejects inverted bounds for the given step direction" do
      assert {:error, _} = Evision.Mat.arange(10, 0, 1, :s32)
      assert {:error, _} = Evision.Mat.arange(0, 10, -1, :s32)
    end

    test "arange/4 rejects zero step at the Elixir guard" do
      assert_raise FunctionClauseError, fn ->
        Evision.Mat.arange(0, 10, 0, :s32)
      end
    end

    test "ceil, floor, round all process every channel of multi-channel matrices" do
      values = for v <- [0.4, 0.6, 1.4, 1.6, 2.4, 2.6], into: <<>>, do: <<v::float-little-size(32)>>
      mat = Evision.Mat.from_binary(values, {:f, 32}, 1, 2, 3)

      ceiled_bin = Evision.Mat.to_binary(Evision.Mat.ceil(mat))
      floored_bin = Evision.Mat.to_binary(Evision.Mat.floor(mat))
      rounded_bin = Evision.Mat.to_binary(Evision.Mat.round(mat))

      expected_ceil =
        for v <- [1.0, 1.0, 2.0, 2.0, 3.0, 3.0], into: <<>>, do: <<v::float-little-size(32)>>

      expected_floor =
        for v <- [0.0, 0.0, 1.0, 1.0, 2.0, 2.0], into: <<>>, do: <<v::float-little-size(32)>>

      expected_round =
        for v <- [0.0, 1.0, 1.0, 2.0, 2.0, 3.0], into: <<>>, do: <<v::float-little-size(32)>>

      assert ceiled_bin == expected_ceil
      assert floored_bin == expected_floor
      assert rounded_bin == expected_round
    end

    test "negate flips every channel of multi-channel u8 matrices" do
      mat = Evision.Mat.from_binary(<<1, 2, 3, 4, 5, 6>>, {:u, 8}, 1, 2, 3)

      negated = Evision.Mat.negate(mat)

      assert Evision.Mat.to_binary(negated) == <<255, 254, 253, 252, 251, 250>>
    end

    test "to_binary in element units stays consistent between Mat and nx_tensor paths" do
      values = for v <- 1..4, into: <<>>, do: <<v::signed-little-size(32)>>
      mat = Evision.Mat.from_binary_by_shape(values, {:s, 32}, {4})
      expected_two_elements = for v <- [1, 2], into: <<>>, do: <<v::signed-little-size(32)>>

      assert Evision.Mat.to_binary(mat, 2) == expected_two_elements

      nx_tensor = %{
        __struct__: :nx_tensor,
        data: values,
        type: :s32,
        shape: {4}
      }

      assert Evision.Mat.to_binary(nx_tensor, 2) == expected_two_elements
    end
  end

  @tag :nx
  test "use Nx.tensor without calling last_dim_as_channel" do
    v = Nx.broadcast(Nx.tensor(1.0, type: :f64), {120, 120, 3})

    assert %Evision.Mat{
             channels: 3,
             dims: 2,
             type: {:f, 64},
             raw_type: 70,
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
