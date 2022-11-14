defmodule Evision.Test do
  use ExUnit.Case

  alias Evision.Mat

  test "imread non-exist image should return error message: \"empty matrix\"" do
    assert {:error, "empty matrix"} == Evision.imread("/dev/null")
  end

  test "Evision.Mat.as_type" do
    %Mat{type: {:u, 8}} = mat = Evision.Mat.from_binary_by_shape(<<1, 2, 3, 4>>, {:u, 8}, {2, 2})
    %Mat{type: {:f, 32}} = mat = Evision.Mat.as_type(mat, {:f, 32})
    %Mat{type: {:f, 64}} = Evision.Mat.as_type(mat, {:f, 64})
  end

  @tag :nx
  test "Evision.Mat.as_type and verify value" do
    %Mat{type: {:u, 8}} = mat = Evision.Mat.from_binary_by_shape(<<1, 2, 3, 4>>, {:u, 8}, {2, 2})
    %Mat{type: {:f, 32}} = mat = Evision.Mat.as_type(mat, {:f, 32})
    %Mat{type: {:f, 64}} = Evision.Mat.as_type(mat, {:f, 64})

    assert [1.0, 2.0, 3.0, 4.0] == Nx.to_flat_list(Evision.Mat.to_nx(mat))
  end

  test "Evision.Mat.clone" do
    %Mat{type: {:u, 8}} = mat = Evision.Mat.from_binary_by_shape(<<1, 2, 3, 4>>, {:u, 8}, {2, 2})
    %Mat{} = cloned = Evision.Mat.clone(mat)
    assert cloned.ref != mat.ref
    assert Evision.Mat.type(cloned) == Evision.Mat.type(mat)
    assert Evision.Mat.shape(cloned) == Evision.Mat.shape(mat)
    assert Evision.Mat.to_binary(cloned) == Evision.Mat.to_binary(mat)
  end

  test "decode png from file w/o alpha channel" do
    %Mat{type: {:u, 8}, shape: {2, 3, 3}} =
      mat = Evision.imread(Path.join([__DIR__, "testdata", "test.png"]))

    img_data = Evision.Mat.to_binary(mat)

    assert <<126, 145, 241, 78, 190, 136, 183, 122, 68, 187, 196, 244, 145, 205, 190, 200, 184,
             144>> == img_data
  end

  test "decode png from file w/ alpha channel" do
    %Mat{type: {:u, 8}, shape: {2, 3, 4}} =
      mat =
      Evision.imread(Path.join([__DIR__, "testdata", "test.png"]),
        flags: Evision.cv_IMREAD_UNCHANGED()
      )

    img_data = Evision.Mat.to_binary(mat)

    assert <<126, 145, 241, 255, 78, 190, 136, 255, 183, 122, 68, 255, 187, 196, 244, 255, 145,
             205, 190, 255, 200, 184, 144, 255>> == img_data
  end

  test "decode image from file grayscale" do
    %Mat{type: {:u, 8}, shape: {2, 3}} =
      mat =
      Evision.imread(Path.join([__DIR__, "testdata", "test.png"]),
        flags: Evision.cv_IMREAD_GRAYSCALE()
      )

    img_data = Evision.Mat.to_binary(mat)
    assert <<171, 161, 112, 209, 193, 173>> == img_data
  end

  test "decode jpg from file" do
    %Mat{type: {:u, 8}, shape: {2, 3, 3}} =
      mat = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))

    img_data = Evision.Mat.to_binary(mat)

    assert <<70, 128, 180, 61, 119, 171, 117, 143, 65, 112, 170, 222, 95, 153, 205, 140, 166, 88>> ==
             img_data
  end

  test "Evision.imreadmulti" do
    [%Mat{}, %Mat{}] =
      Evision.imreadmulti(Path.join([__DIR__, "testdata", "imreadmulti_test.tiff"]))
  end

  test "Evision.imwritemulti" do
    input_path = Path.join([__DIR__, "testdata", "imreadmulti_test.tiff"])
    output_path = Path.join([__DIR__, "testdata", "imwritemulti_test.tiff"])
    images = Evision.imreadmulti(input_path)
    assert true = Evision.imwritemulti(output_path, images)

    [%Mat{}, %Mat{}] = Evision.imreadmulti(output_path)
    assert Enum.count(images) == 2

    File.rm!(output_path)
  end

  test "Evision.imencode and Evision.imdecode" do
    %Mat{shape: shape, type: type} =
      mat = Evision.imread(Path.join([__DIR__, "testdata", "test.png"]))

    encoded = Evision.imencode(".png", mat)
    assert is_binary(encoded)

    %Mat{shape: ^shape, type: ^type} = Evision.imdecode(encoded, Evision.cv_IMREAD_ANYCOLOR())
  end

  test "Evision.resize" do
    mat = Evision.imread(Path.join([__DIR__, "testdata", "test.png"]))

    resize_height = 4
    resize_width = 6

    %Mat{shape: {^resize_width, ^resize_height, 3}} =
      Evision.resize(mat, {resize_height, resize_width})
  end

  test "Evision.imwrite" do
    input_path = Path.join([__DIR__, "testdata", "test.png"])
    output_path = Path.join([__DIR__, "testdata", "imwrite_test.png"])
    mat = Evision.imread(input_path)
    assert Evision.imwrite(output_path, mat)

    %Mat{type: {:u, 8}, shape: {2, 3, 3}} = mat = Evision.imread(output_path)

    img_data = Evision.Mat.to_binary(mat)

    assert <<126, 145, 241, 78, 190, 136, 183, 122, 68, 187, 196, 244, 145, 205, 190, 200, 184,
             144>> == img_data

    File.rm!(output_path)
  end

  test "Evision.mean" do
    mat =
      Evision.imread(Path.join([__DIR__, "testdata", "test.png"]),
        flags: Evision.cv_IMREAD_GRAYSCALE()
      )

    bin = Evision.Mat.to_binary(mat)

    avg = Enum.sum(:binary.bin_to_list(bin)) / byte_size(bin)
    {avg_cv, 0.0, 0.0, 0.0} = Evision.mean(mat)
    assert abs(avg - avg_cv) < 0.00000001
  end

  test "Evision.minMaxLoc" do
    mat =
      Evision.imread(Path.join([__DIR__, "testdata", "test.png"]),
        flags: Evision.cv_IMREAD_GRAYSCALE()
      )

    {112.0, 209.0, {2, 0}, {0, 1}} = Evision.minMaxLoc(mat)
  end

  test "Evision.Mat.size" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    assert {2, [2, 3]} == Evision.Mat.size(img)
  end

  test "Evision.Mat.channels" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    assert 3 == Evision.Mat.channels(img)
  end

  test "Evision.Mat.depth" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    assert Evision.cv_8U() == Evision.Mat.depth(img)
  end

  test "Evision.Mat.raw_type" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    assert Evision.cv_8UC3() == Evision.Mat.raw_type(img)
  end

  test "Evision.Mat.isSubmatrix" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    assert false == Evision.Mat.isSubmatrix(img)
  end

  test "Evision.Mat.isContinuous" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    assert true == Evision.Mat.isContinuous(img)
  end

  test "Evision.Mat.elemSize" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    assert 3 == Evision.Mat.elemSize(img)
  end

  test "Evision.Mat.elemSize1" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    assert 1 == Evision.Mat.elemSize1(img)
  end

  test "Evision.Mat.total/{1,2,3}" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    assert 6 == Evision.Mat.total(img)
    assert 2 == Evision.Mat.total(img, 0, 1)
    assert 3 == Evision.Mat.total(img, 1, 2)
  end

  test "Evision.Mat.as_shape" do
    img = Evision.imread(Path.join([__DIR__, "testdata", "test.jpg"]))
    %Mat{shape: {3, 2, 3}} = new_img = Evision.Mat.as_shape(img, {3, 2, 3})
    assert Evision.Mat.to_binary(img) == Evision.Mat.to_binary(new_img)
  end

  test "Evision.Mat.literal/1" do
    %Evision.Mat{
      channels: 1,
      dims: 0,
      type: {:u, 8},
      raw_type: 0,
      shape: {},
      ref: any_ref
    } = Evision.Mat.literal([])

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
    } = Evision.Mat.literal([[[1, 1, 1], [2, 2, 2], [3, 3, 3]]], :u8)

    assert is_reference(any_ref)

    %Evision.Mat{
      channels: 1,
      dims: 3,
      type: {:f, 32},
      raw_type: 5,
      shape: {1, 3, 3},
      ref: any_ref
    } = Evision.Mat.literal([[[1, 1, 1], [2, 2, 2], [3, 3, 3]]], :f32)

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
    } = Evision.Mat.literal([[[1, 1, 1], [2, 2, 2], [3, 3, 3]]], :u8, as_2d: true)

    assert is_reference(any_ref)

    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:f, 32},
      raw_type: 21,
      shape: {1, 3, 3},
      ref: any_ref
    } = Evision.Mat.literal([[[1, 1, 1], [2, 2, 2], [3, 3, 3]]], :f32, as_2d: true)

    assert is_reference(any_ref)
  end

  test "Evision.boxPoints/1" do
    # `RotatedRect` has to be a tuple, {centre, size, angle}
    Evision.boxPoints({{224.0, 262.5}, {343.0, 344.0}, 90.0})
  end

  test "Evision.Mat.roi/2" do
    %Evision.Mat{} =
      img = Evision.imread(Path.join([__DIR__, "testdata", "qr_detector_test.png"]))

    # Mat operator()( const Rect& roi ) const;
    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {200, 100, 3}
    } = Evision.Mat.roi(img, {10, 10, 100, 200})

    # Mat operator()(const std::vector<Range>& ranges) const;
    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {90, 90, 3}
    } = Evision.Mat.roi(img, [{10, 100}, {10, 100}])

    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {90, 300, 3}
    } = Evision.Mat.roi(img, [{10, 100}, :all])
  end

  test "Evision.Mat.roi/3" do
    %Evision.Mat{} =
      img = Evision.imread(Path.join([__DIR__, "testdata", "qr_detector_test.png"]))

    # Mat operator()( Range rowRange, Range colRange ) const;
    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {90, 180, 3}
    } = Evision.Mat.roi(img, {10, 100}, {20, 200})

    %Evision.Mat{
      channels: 3,
      dims: 2,
      type: {:u, 8},
      raw_type: 16,
      shape: {300, 180, 3}
    } = Evision.Mat.roi(img, :all, {20, 200})
  end

  test "Evision.warpPerspective" do
    # Code translated from https://stackoverflow.com/a/64837860
    # read input
    %Evision.Mat{shape: {h, w, _}} =
      img = Evision.imread(Path.join([__DIR__, "testdata", "warp_perspective.png"]))

    # hypot.(list(number())) function returns the Euclidean norm
    hypot = fn l -> :math.sqrt(Enum.sum(Enum.map(l, fn i -> i * i end))) end

    # specify input coordinates for corners of red quadrilateral in order TL, TR, BR, BL as x,
    input = Nx.tensor([[136, 113], [206, 130], [173, 207], [132, 196]], type: :f32)

    # get top and left dimensions and set to output dimensions of red rectangle
    output_width = [
      Nx.to_number(Nx.subtract(input[[0, 0]], input[[1, 0]])),
      Nx.to_number(Nx.subtract(input[[0, 1]], input[[1, 1]]))
    ]

    output_width = round(hypot.(output_width))

    output_height = [
      Nx.to_number(Nx.subtract(input[[0, 0]], input[[3, 0]])),
      Nx.to_number(Nx.subtract(input[[0, 1]], input[[3, 1]]))
    ]

    output_height = round(hypot.(output_height))

    # set upper left coordinates for output rectangle
    x = Nx.to_number(input[[0, 0]])
    y = Nx.to_number(input[[0, 1]])

    # specify output coordinates for corners of red quadrilateral in order TL, TR, BR, BL as x,
    output =
      Nx.tensor(
        [
          [x, y],
          [x + output_width - 1, y],
          [x + output_width - 1, y + output_height - 1],
          [x, y + output_height - 1]
        ],
        type: :f32
      )

    # compute perspective matrix
    matrix = Evision.getPerspectiveTransform(input, output)

    # do perspective transformation setting area outside input to black
    # Note that output size is the same as the input image size
    %Evision.Mat{} =
      Evision.warpPerspective(
        img,
        matrix,
        {w, h},
        flags: Evision.cv_INTER_LINEAR(),
        borderMode: Evision.cv_BORDER_CONSTANT(),
        borderValue: {0, 0, 0}
      )

    # expecting error when any input argument is invalid
    {:error, _} =
      Evision.warpPerspective(
        img,
        img,
        {w, h},
        flags: Evision.cv_INTER_LINEAR(),
        borderMode: Evision.cv_BORDER_CONSTANT(),
        borderValue: {0, 0, 0}
      )
  end
end
