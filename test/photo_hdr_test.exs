defmodule OpenCV.Photo.HDR.Test do
  use ExUnit.Case
  use Bitwise

  @moduletag timeout: 120_000

  @tag :photo
  @tag :require_downloading
  test "High Dynamic Range Imaging" do
    exposure_filenames =
      0..15
      |> Enum.map(&Integer.to_string(&1))
      |> Enum.map(&String.pad_leading(&1, 2, "0"))
      |> Enum.map(&"memorial" <> &1 <> ".png")
    exposure_file_urls =
      exposure_filenames
      |> Enum.map(&"https://raw.githubusercontent.com/opencv/opencv_extra/4.x/testdata/cv/hdr/exposures/" <> &1)
    exposure_file_save_paths =
      exposure_filenames
      |> Enum.map(&Path.join([__DIR__, "photo_hdr_test", &1]))
    assert true =
      exposure_file_urls
        |> Enum.zip(exposure_file_save_paths)
        |> Enum.map(fn {url, save_as} -> OpenCV.TestHelper.download!(url, save_as) end)
        |> Enum.all?(&:ok = &1)

    list_txt_file = Path.join([__DIR__, "photo_hdr_test", "list.txt"])
    assert :ok = OpenCV.TestHelper.download!("https://raw.githubusercontent.com/opencv/opencv_extra/4.x/testdata/cv/hdr/exposures/list.txt", list_txt_file)

    # load exposure sequences
    exposure_sequences =
      list_txt_file
      |> File.read!()
      |> String.split("\n")
      |> Enum.reject(&String.length(&1)==0)
      |> Enum.map(&String.split(&1, " "))
      |> Enum.map(&List.to_tuple(&1))
      |> Enum.map(fn {image_filename, times} ->
        {:ok, mat} = OpenCV.imread(Path.join([__DIR__, "photo_hdr_test", image_filename]))
        {val, ""} = Float.parse(times)
        {mat, 1/val}
      end)

    images =
      exposure_sequences
      |> Enum.map(&elem(&1, 0))

    {:ok, times } =
      exposure_sequences
      |> Enum.map(&elem(&1, 1))
      |> Enum.into(<<>>, fn d -> << d::float()-size(32)-little >> end)
      |> OpenCV.Mat.from_binary_by_shape({:f, 32}, {1, Enum.count(images)})

    {:ok, calibrate} = OpenCV.createCalibrateDebevec()
    {:ok, response} = OpenCV.CalibrateDebevec.process(calibrate, images, times)

    {:ok, merge_debevec} = OpenCV.createMergeDebevec()
    {:ok, hdr} = OpenCV.MergeDebevec.process(merge_debevec, images, times, response: response)

    {:ok, tonemap} = OpenCV.createTonemap(gamma: 2.2)
    {:ok, ldr} = OpenCV.Tonemap.process(tonemap, hdr)

    {:ok, merge_mertens} = OpenCV.createMergeMertens()
    {:ok, fusion} = OpenCV.MergeMertens.process(merge_mertens, images)

    output_fusion_file = Path.join([__DIR__, "photo_hdr_test", "fusion.png"])
    fusion
      |> OpenCV.Nx.to_nx()
      |> Nx.multiply(255)
      |> Nx.clip(0, 255)
      |> Nx.as_type({:u, 8})
      |> OpenCV.Nx.to_mat
      |> then(&OpenCV.imwrite(output_fusion_file, elem(&1, 1)))

    output_ldr_file = Path.join([__DIR__, "photo_hdr_test", "ldr.png"])
    t =
      ldr
      |> OpenCV.Nx.to_nx()
    t =
      t
      |> Nx.multiply(255)
      |> Nx.clip(0, 255)
      |> Nx.as_type({:u, 8})
    t
      |> OpenCV.Nx.to_mat
      |> then(&OpenCV.imwrite(output_ldr_file, elem(&1, 1)))

    output_hdr_file = Path.join([__DIR__, "photo_hdr_test", "hdr.hdr"])
    OpenCV.imwrite(output_hdr_file, hdr)
  end
end
