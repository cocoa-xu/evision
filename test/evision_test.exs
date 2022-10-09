defmodule Evision.Test do
  use ExUnit.Case

  test "Errors will be {:error, String.t()}" do
    random_ref = :erlang.make_ref()
    {:error, reason} = Evision.Mat.shape(random_ref)
    assert is_binary(reason)

    {:error, reason} = Evision.Nx.to_nx(random_ref)
    assert is_binary(reason)
  end

  test "imread non-exist image shoud return error message: \"empty matrix\"" do
    assert {:error, "empty matrix"} == Evision.imread("/dev/null")
  end

  test "Evision.Mat.as_type" do
    {:ok, mat} = Evision.Mat.from_binary_by_shape(<<1, 2, 3, 4>>, {:u, 8}, {2, 2})
    {:ok, mat} = Evision.Mat.as_type(mat, {:f, 32})
    {:ok, {:f, 32}} = Evision.Mat.type(mat)
    {:ok, mat} = Evision.Mat.as_type(mat, {:f, 64})
    {:ok, {:f, 64}} = Evision.Mat.type(mat)
  end

  @tag :nx
  test "Evision.Mat.as_type and verify value" do
    {:ok, mat} = Evision.Mat.from_binary_by_shape(<<1, 2, 3, 4>>, {:u, 8}, {2, 2})
    {:ok, mat} = Evision.Mat.as_type(mat, {:f, 32})
    {:ok, {:f, 32}} = Evision.Mat.type(mat)
    {:ok, mat} = Evision.Mat.as_type(mat, {:f, 64})
    {:ok, {:f, 64}} = Evision.Mat.type(mat)

    assert [1.0, 2.0, 3.0, 4.0] =
             mat
             |> Evision.Nx.to_nx()
             |> Nx.to_flat_list()
  end

  test "Evision.Mat.clone" do
    {:ok, mat} = Evision.Mat.from_binary_by_shape(<<1, 2, 3, 4>>, {:u, 8}, {2, 2})
    {:ok, cloned} = Evision.Mat.clone(mat)
    assert cloned != mat
    assert Evision.Mat.type(cloned) == Evision.Mat.type(mat)
    assert Evision.Mat.shape(cloned) == Evision.Mat.shape(mat)
    assert Evision.Mat.to_binary(cloned) == Evision.Mat.to_binary(mat)
  end

  test "decode png from file w/o alpha channel" do
    ret =
      Path.join(__DIR__, ["test.png"])
      |> Evision.imread()

    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = Evision.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = Evision.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3, 3} == shape

    ret = Evision.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<126, 145, 241, 78, 190, 136, 183, 122, 68, 187, 196, 244, 145, 205, 190, 200, 184,
             144>> == img_data
  end

  test "decode png from file w/ alpha channel" do
    ret =
      Path.join(__DIR__, ["test.png"])
      |> Evision.imread(flags: Evision.cv_IMREAD_UNCHANGED())

    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = Evision.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = Evision.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3, 4} == shape

    ret = Evision.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<126, 145, 241, 255, 78, 190, 136, 255, 183, 122, 68, 255, 187, 196, 244, 255, 145,
             205, 190, 255, 200, 184, 144, 255>> == img_data
  end

  test "decode image from file grayscale" do
    ret =
      Path.join([__DIR__, "test.png"])
      |> Evision.imread(flags: Evision.cv_IMREAD_GRAYSCALE())

    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = Evision.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = Evision.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3} == shape

    ret = Evision.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<171, 161, 112, 209, 193, 173>> == img_data
  end

  test "decode jpg from file" do
    ret =
      Path.join([__DIR__, "test.jpg"])
      |> Evision.imread()

    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = Evision.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = Evision.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3, 3} == shape

    ret = Evision.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<70, 128, 180, 61, 119, 171, 117, 143, 65, 112, 170, 222, 95, 153, 205, 140, 166, 88>> ==
             img_data
  end

  test "Evision.imreadmulti" do
    path = Path.join(__DIR__, ["imreadmulti_test.tiff"])
    ret = Evision.imreadmulti(path)
    assert :ok == elem(ret, 0)
    images = elem(ret, 1)
    assert Enum.count(images) == 2
  end

  test "Evision.imwritemulti" do
    input_path = Path.join([__DIR__, "imreadmulti_test.tiff"])
    output_path = Path.join([__DIR__, "imwritemulti_test.tiff"])
    {:ok, images} = Evision.imreadmulti(input_path)
    assert :ok = Evision.imwritemulti(output_path, images)

    ret = Evision.imreadmulti(output_path)
    assert :ok == elem(ret, 0)
    images = elem(ret, 1)
    assert Enum.count(images) == 2

    File.rm!(output_path)
  end

  test "Evision.imencode and Evision.imdecode" do
    {:ok, mat} = Evision.imread(Path.join(__DIR__, ["test.png"]))

    ret = Evision.imencode(".png", mat)
    assert :ok == elem(ret, 0)
    {:ok, encoded} = ret

    ret = Evision.imdecode(encoded, Evision.cv_IMREAD_ANYCOLOR())
    assert :ok == elem(ret, 0)
    {:ok, decoded_mat} = ret
    {:ok, {2, 3, 3}} = Evision.Mat.shape(decoded_mat)
  end

  test "Evision.resize" do
    {:ok, mat} = Evision.imread(Path.join([__DIR__, "test.png"]))

    resize_height = 4
    resize_width = 6
    {:ok, resized_mat} = Evision.resize(mat, [resize_height, resize_width])
    {:ok, {^resize_width, ^resize_height, 3}} = Evision.Mat.shape(resized_mat)
  end

  test "Evision.imwrite" do
    input_path = Path.join([__DIR__, "test.png"])
    output_path = Path.join([__DIR__, "imwrite_test.png"])
    {:ok, mat} = Evision.imread(input_path)
    assert :ok = Evision.imwrite(output_path, mat)

    ret = Evision.imread(output_path)
    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = Evision.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = Evision.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3, 3} == shape

    ret = Evision.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<126, 145, 241, 78, 190, 136, 183, 122, 68, 187, 196, 244, 145, 205, 190, 200, 184,
             144>> == img_data

    File.rm!(output_path)
  end

  test "Evision.mean" do
    {:ok, mat} =
      Path.join(__DIR__, ["test.png"])
      |> Evision.imread(flags: Evision.cv_IMREAD_GRAYSCALE())

    {:ok, bin} = Evision.Mat.to_binary(mat)
    avg = Enum.sum(:binary.bin_to_list(bin)) / byte_size(bin)
    {:ok, {avg_cv, 0.0, 0.0, 0.0}} = Evision.mean(mat)
    assert abs(avg - avg_cv) < 0.00000001
  end

  test "Evision.minMaxLoc" do
    {:ok, mat} =
      Path.join([__DIR__, "test.png"])
      |> Evision.imread(flags: Evision.cv_IMREAD_GRAYSCALE())

    {:ok, {112.0, 209.0, {2, 0}, {0, 1}}} = Evision.minMaxLoc(mat)
  end

  test "Evision.Mat.size" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    assert {2, [2, 3]} == Evision.Mat.size(img)
  end

  test "Evision.Mat.channels" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    assert 3 == Evision.Mat.channels(img)
  end

  test "Evision.Mat.depth" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    assert Evision.cv_8U() == Evision.Mat.depth(img)
  end

  test "Evision.Mat.raw_type" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    assert Evision.cv_8UC3() == Evision.Mat.raw_type(img)
  end

  test "Evision.Mat.isSubmatrix" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    assert false == Evision.Mat.isSubmatrix(img)
  end

  test "Evision.Mat.isContinuous" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    assert true == Evision.Mat.isContinuous(img)
  end

  test "Evision.Mat.elemSize" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    assert 3 == Evision.Mat.elemSize(img)
  end

  test "Evision.Mat.elemSize1" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    assert 1 == Evision.Mat.elemSize1(img)
  end

  test "Evision.Mat.total/{1,2,3}" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    assert 6 == Evision.Mat.total(img)
    assert 2 == Evision.Mat.total(img, 0, 1)
    assert 3 == Evision.Mat.total(img, 1, 2)
  end

  test "Evision.Mat.as_shape" do
    img = Evision.imread!(Path.join([__DIR__, "test.jpg"]))
    new_img = Evision.Mat.as_shape!(img, {3, 2, 3})
    assert {3, 2, 3} == Evision.Mat.shape!(new_img)
    assert Evision.Mat.to_binary!(img) == Evision.Mat.to_binary!(new_img)
  end

  test "Evision.Mat.literal/1" do
    %Evision.Mat{
      channels: 1,
      dims: 0,
      type: {:u, 8},
      raw_type: 0,
      shape: {},
      ref: any_ref
    } = Evision.Mat.literal!([])
    assert is_reference(any_ref)
  end

  test "Evision.Mat.literal/2" do
    %Evision.Mat{
      channels: 1,
      dims: 3,
      type: {:u, 8},
      raw_type: 0,
      shape: {1, 3, 3},
      ref: any_ref
    } = Evision.Mat.literal!([[[1,1,1],[2,2,2],[3,3,3]]], :u8)
    assert is_reference(any_ref)

    %Evision.Mat{
      channels: 1,
      dims: 3,
      type: {:f, 32},
      raw_type: 5,
      shape: {1, 3, 3},
      ref: any_ref
    } = Evision.Mat.literal!([[[1,1,1],[2,2,2],[3,3,3]]], :f32)
    assert is_reference(any_ref)
  end

  test "Evision.Mat.literal/3" do
    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {1, 3, 3},
      ref: any_ref
    } = Evision.Mat.literal!([[[1,1,1],[2,2,2],[3,3,3]]], :u8, as_2d: true)
    assert is_reference(any_ref)

    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:f, 32},
      raw_type: 21,
      shape: {1, 3, 3},
      ref: any_ref
    } = Evision.Mat.literal!([[[1,1,1],[2,2,2],[3,3,3]]], :f32, as_2d: true)
    assert is_reference(any_ref)
  end

  test "Evision.boxPoints/1" do
    # `RotatedRect` has to be a tuple, {centre, size, angle}
    Evision.boxPoints!({{224.0, 262.5}, {343.0, 344.0}, 90.0})

    # while `Point`/`Size` can be either a list or a tuple
    Evision.boxPoints!({[224.0, 262.5], [343.0, 344.0], 90.0})
  end
end
