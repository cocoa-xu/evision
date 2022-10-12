defmodule Evision.Photo.HDR.Test do
  use ExUnit.Case

  alias Evision.CalibrateDebevec

  @moduletag timeout: 120_000

  @tag :photo
  @tag :require_downloading
  test "High Dynamic Range Imaging" do
    exposure_filenames =
      0..15
      |> Enum.map(&Integer.to_string(&1))
      |> Enum.map(&String.pad_leading(&1, 2, "0"))
      |> Enum.map(&("memorial" <> &1 <> ".png"))

    exposure_file_urls =
      exposure_filenames
      |> Enum.map(
        &("https://raw.githubusercontent.com/opencv/opencv_extra/4.x/testdata/cv/hdr/exposures/" <>
            &1)
      )

    exposure_file_save_paths =
      exposure_filenames
      |> Enum.map(&Path.join([__DIR__, "photo_hdr_test", &1]))

    assert true =
             exposure_file_urls
             |> Enum.zip(exposure_file_save_paths)
             |> Enum.map(fn {url, save_as} -> Evision.TestHelper.download!(url, save_as) end)
             |> Enum.all?(&(:ok = &1))

    list_txt_file = Path.join([__DIR__, "photo_hdr_test", "list.txt"])

    assert :ok =
             Evision.TestHelper.download!(
               "https://raw.githubusercontent.com/opencv/opencv_extra/4.x/testdata/cv/hdr/exposures/list.txt",
               list_txt_file
             )

    # load exposure sequences
    exposure_sequences =
      list_txt_file
      |> File.read!()
      |> String.split("\n")
      |> Enum.reject(&(String.length(&1) == 0))
      |> Enum.map(&String.replace(&1, "\r", "")) # remove "\r" for Windows
      |> Enum.map(&String.split(&1, " "))
      |> Enum.map(&List.to_tuple(&1))
      |> Enum.map(fn {image_filename, times} ->
        mat = Evision.imread(Path.join([__DIR__, "photo_hdr_test", image_filename]))
        {val, ""} = Float.parse(times)
        {mat, 1 / val}
      end)

    images =
      exposure_sequences
      |> Enum.map(&elem(&1, 0))

    times =
      exposure_sequences
      |> Enum.map(&elem(&1, 1))
      |> Enum.into(<<>>, fn d -> <<d::float-size(32)-little>> end)
      |> Evision.Mat.from_binary_by_shape({:f, 32}, {1, Enum.count(images)})

    %CalibrateDebevec{} = calibrate = Evision.createCalibrateDebevec()
    response = Evision.CalibrateDebevec.process(calibrate, images, times)

    merge_debevec = Evision.createMergeDebevec()
    hdr = Evision.MergeDebevec.process(merge_debevec, images, times, response: response)

    tonemap = Evision.createTonemap(gamma: 2.2)
    ldr = Evision.Tonemap.process(tonemap, hdr)

    merge_mertens = Evision.createMergeMertens()
    fusion = Evision.MergeMertens.process(merge_mertens, images)

    output_fusion_file = Path.join([__DIR__, "photo_hdr_test", "fusion.png"])

    fusion
    |> Evision.Nx.to_nx(Nx.BinaryBackend)
    |> Nx.multiply(255)
    |> Nx.clip(0, 255)
    |> Nx.as_type({:u, 8})
    |> Evision.Nx.to_mat_2d()
    |> then(&Evision.imwrite(output_fusion_file, &1))

    output_ldr_file = Path.join([__DIR__, "photo_hdr_test", "ldr.png"])
    f32_shape = Evision.Mat.shape(ldr)
    nan = <<0, 0, 192, 255>>
    positive_inf = <<0, 0, 128, 127>>
    negative_inf = <<0, 0, 128, 255>>

    ldr
    |> Evision.Mat.to_binary()
    |> Evision.TestHelper.chunk_binary(4)
    |> Enum.map(fn f32 ->
      case f32 do
        ^nan ->
          <<0, 0, 0, 0>>

        ^positive_inf ->
          <<0, 0, 0, 0>>

        ^negative_inf ->
          <<0, 0, 0, 0>>

        # legal value
        _ ->
          f32
      end
    end)
    |> IO.iodata_to_binary()
    |> Nx.from_binary({:f, 32})
    |> Nx.reshape(f32_shape)
    |> Nx.multiply(255)
    |> Nx.clip(0, 255)
    |> Nx.as_type({:u, 8})
    |> Evision.Nx.to_mat_2d()
    |> then(&Evision.imwrite(output_ldr_file, &1))

    output_hdr_file = Path.join([__DIR__, "photo_hdr_test", "hdr.hdr"])
    Evision.imwrite(output_hdr_file, hdr)
  end
end
