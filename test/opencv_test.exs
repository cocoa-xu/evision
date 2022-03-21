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
    {:ok, mat} =
      Path.join(__DIR__, ["test.png"])
      |> Evision.imread()

    ret = Evision.imencode(".png", mat)
    assert :ok == elem(ret, 0)
    {:ok, encoded} = ret
    encoded = IO.iodata_to_binary(encoded)

    ret = Evision.imdecode(encoded, Evision.cv_IMREAD_ANYCOLOR())
    assert :ok == elem(ret, 0)
    {:ok, decoded_mat} = ret
    {:ok, {2, 3, 3}} = Evision.Mat.shape(decoded_mat)
  end

  test "Evision.resize" do
    {:ok, mat} =
      Path.join([__DIR__, "test.png"])
      |> Evision.imread()

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
end
