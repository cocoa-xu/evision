defmodule OpenCV.Test do
  use ExUnit.Case

  test "decode png from file w/o alpha channel" do
    ret =
      Path.join(__DIR__, ["test.png"])
      |> OpenCV.imread()
    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = OpenCV.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = OpenCV.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3, 3} == shape

    ret = OpenCV.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<126, 145, 241, 78, 190, 136, 183, 122, 68, 187, 196, 244, 145, 205, 190, 200,
              184, 144>> == img_data
  end

  test "decode png from file w/ alpha channel" do
    ret =
      Path.join(__DIR__, ["test.png"])
      |> OpenCV.imread(flags: OpenCV.cv_imread_unchanged)
    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = OpenCV.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = OpenCV.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3, 4} == shape

    ret = OpenCV.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<126, 145, 241, 255, 78, 190, 136, 255, 183, 122, 68, 255, 187, 196, 244, 255,
             145, 205, 190, 255, 200, 184, 144, 255>> == img_data
  end

  test "decode image from file grayscale" do
    ret =
      Path.join(__DIR__, ["test.png"])
      |> OpenCV.imread(flags: OpenCV.cv_imread_grayscale)
    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = OpenCV.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = OpenCV.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3} == shape

    ret = OpenCV.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<171, 161, 112, 209, 193, 173>> == img_data
  end

  test "decode jpg from file" do
    ret =
      Path.join(__DIR__, ["test.jpg"])
      |> OpenCV.imread()
    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = OpenCV.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = OpenCV.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3, 3} == shape

    ret = OpenCV.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<70, 128, 180, 61, 119, 171, 117, 143, 65, 112, 170, 222, 95, 153, 205, 140,
             166, 88>> == img_data
  end

  test "OpenCV.imreadmulti" do
    path = Path.join(__DIR__, ["imreadmulti_test.tiff"])
    ret = OpenCV.imreadmulti(path)
    assert :ok == elem(ret, 0)
    images = elem(ret, 1)
    assert Enum.count(images) == 2
  end

  test "OpenCV.imwritemulti" do
    input_path = Path.join(__DIR__, ["imreadmulti_test.tiff"])
    output_path = Path.join(__DIR__, ["imwritemulti_test.tiff"])
    {:ok, images} = OpenCV.imreadmulti(input_path)
    assert :ok = OpenCV.imwritemulti(output_path, images)

    ret = OpenCV.imreadmulti(output_path)
    assert :ok == elem(ret, 0)
    images = elem(ret, 1)
    assert Enum.count(images) == 2

    File.rm!(output_path)
  end

  test "OpenCV.imencode and OpenCV.imdecode" do
    {:ok, mat} =
      Path.join(__DIR__, ["test.png"])
      |> OpenCV.imread()

    ret = OpenCV.imencode(".png", mat)
    assert :ok == elem(ret, 0)
    {:ok, encoded} = ret
    encoded = IO.iodata_to_binary(encoded)

    ret = OpenCV.imdecode(encoded, OpenCV.cv_imread_anycolor())
    assert :ok == elem(ret, 0)
    {:ok, decoded_mat} = ret
    {:ok, {2, 3, 3}} = OpenCV.Mat.shape(decoded_mat)
  end

  test "OpenCV.resize" do
    {:ok, mat} =
      [__DIR__, "test.png"]
      |> Path.join
      |> OpenCV.imread()
    resize_height = 4
    resize_width = 6
    {:ok, resized_mat} = OpenCV.resize(mat, [resize_height, resize_width])
    {:ok, {^resize_width, ^resize_height, 3}} = OpenCV.Mat.shape(resized_mat)
  end

  test "OpenCV.imwrite" do
    input_path = Path.join(__DIR__, ["test.png"])
    output_path = Path.join(__DIR__, ["imwrite_test.png"])
    {:ok, mat} = OpenCV.imread(input_path)
    assert :ok = OpenCV.imwrite(output_path, mat)

    ret = OpenCV.imread(output_path)
    assert :ok == elem(ret, 0)
    mat = elem(ret, 1)

    ret = OpenCV.Mat.type(mat)
    assert :ok == elem(ret, 0)
    type = elem(ret, 1)
    assert {:u, 8} == type

    ret = OpenCV.Mat.shape(mat)
    assert :ok == elem(ret, 0)
    shape = elem(ret, 1)
    assert {2, 3, 3} == shape

    ret = OpenCV.Mat.to_binary(mat)
    assert :ok == elem(ret, 0)
    img_data = elem(ret, 1)

    assert <<126, 145, 241, 78, 190, 136, 183, 122, 68, 187, 196, 244, 145, 205, 190, 200,
             184, 144>> == img_data
    File.rm!(output_path)
  end

  test "OpenCV.mean" do
    {:ok, mat} =
      Path.join(__DIR__, ["test.png"])
      |> OpenCV.imread(flags: OpenCV.cv_imread_grayscale)
    {:ok, bin} = OpenCV.Mat.to_binary(mat)
    avg = Enum.sum(:binary.bin_to_list(bin)) / byte_size(bin)
    {:ok, {avg_cv, 0.0, 0.0, 0.0}} = OpenCV.mean(mat)
    assert abs(avg - avg_cv) < 0.00000001
  end
end
