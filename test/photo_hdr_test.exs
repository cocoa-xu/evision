defmodule OpenCV.Photo.HDR.Test do
  use ExUnit.Case
  use Bitwise

  @moduletag timeout: 120_000

  defmodule ReinterpretCast do
    def cast(list, source_type, :binary) when is_list(list) do
      case source_type do
        {:i, bits, :little} ->
          list
          |> Enum.into(<<>>, fn data ->
            << data::integer()-size(bits)-little >>
          end)
        {:i, bits, :big} ->
          list
          |> Enum.into(<<>>, fn data ->
            << data::integer()-size(bits)-big >>
          end)
        {:f, bits, :little} ->
          list
          |> Enum.into(<<>>, fn data ->
            << data::float()-size(bits)-little >>
          end)
        {:f, bits, :big} ->
          list
          |> Enum.into(<<>>, fn data ->
            << data::float()-size(bits)-big >>
          end)
      end
    end

    def cast(list, source_type, target_type) do
      list
      |> cast(source_type, :binary)
      |> cast(target_type)
    end

    def cast(binary, {type, bits, endianness})
    when is_binary(binary) and (type == :i or type == :f) and (endianness == :little or endianness == :big) do
      with 0 <- rem(bits, 8),
           chunk_size <- div(bits, 8),
           0 <- rem(byte_size(binary), div(bits, 8)) do
        binary
        |> chunk_binary(chunk_size)
        |> _cast({type, bits, endianness})
      end
    end

    defp _cast(binary_chunks, {:i, bits, :little}) do
      binary_chunks
      |> Enum.into([], fn chunk ->
        << to::integer()-size(bits)-little >> = chunk
        to
      end)
    end

    defp _cast(binary_chunks, {:i, bits, :big}) do
      binary_chunks
      |> Enum.into([], fn chunk ->
        << to::integer()-size(bits)-big >> = chunk
        to
      end)
    end

    defp _cast(binary_chunks, {:f, bits, :little}) do
      binary_chunks
      |> Enum.into([], fn chunk ->
        << to::float()-size(bits)-little >> = chunk
        to
      end)
    end

    defp _cast(binary_chunks, {:f, bits, :big}) do
      binary_chunks
      |> Enum.into([], fn chunk ->
        << to::float()-size(bits)-big >> = chunk
        to
      end)
    end

    defp chunk_binary(binary, chunk_size) when is_binary(binary) do
      total_bytes = byte_size(binary)
      full_chunks = div(total_bytes, chunk_size)
      chunks =
        if full_chunks > 0 do
          for i <- 0..(full_chunks-1), reduce: [] do
            acc -> [:binary.part(binary, chunk_size * i, chunk_size) | acc]
          end
        else
          []
        end
      remaining = rem(total_bytes, chunk_size)
      chunks =
        if remaining > 0 do
          [:binary.part(binary, chunk_size * full_chunks, remaining) | chunks]
        else
          chunks
        end
      Enum.reverse(chunks)
    end
  end

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

    f32_shape = Nx.shape(t)
    t
      |> Nx.to_binary()
      |> ReinterpretCast.cast({:i, 32, :little})
      |> Enum.map(fn i32 ->
        case i32 do
          # <<0, 0, 192, 255>>, i.e, NaN
          4290772992 -> 0
          # <<>>, i.e., INFINITY
          2139095040 -> 0
          # legal values
          _ -> i32
        end
      end)
      |> ReinterpretCast.cast({:i, 32, :little}, :binary)
      |> Nx.from_binary({:f, 32})
      |> Nx.reshape(f32_shape)
      |> Nx.as_type({:u, 8})
      |> OpenCV.Nx.to_mat
      |> then(&OpenCV.imwrite(output_ldr_file, elem(&1, 1)))

    output_hdr_file = Path.join([__DIR__, "photo_hdr_test", "hdr.hdr"])
    OpenCV.imwrite(output_hdr_file, hdr)
  end
end
