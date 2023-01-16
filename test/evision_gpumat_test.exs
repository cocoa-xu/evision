defmodule Evision.CUDA.GpuMat.Test do
  use ExUnit.Case

  @compile {:no_warn_undefined, Evision.CUDA.GpuMat}

  alias Evision.Mat

  @tag :require_cuda
  describe "Basic Operations" do
    if Code.ensure_loaded?(Evision.CUDA.GpuMat) do
      alias Evision.CUDA.GpuMat

      test "load an image from file" do
        %Mat{} = mat = Evision.imread(Path.join([__DIR__, "testdata", "test.png"]))

        gpumat = Evision.CUDA.GpuMat.gpuMat(mat)

        %GpuMat{
          channels: 3,
          type: {:u, 8},
          raw_type: 16,
          shape: {2, 3, 3},
          elemSize: 3
        } = gpumat
      end

      test "explicitly upload an Evision.Mat" do
        %Mat{} = mat = Evision.imread(Path.join([__DIR__, "testdata", "test.png"]))

        gpumat = GpuMat.gpuMat()

        gpumat = Evision.CUDA.GpuMat.upload(gpumat, mat)
        assert Evision.CUDA.GpuMat.cudaPtr(gpumat) > 0

        %GpuMat{
          channels: 3,
          type: {:u, 8},
          raw_type: 16,
          shape: {2, 3, 3},
          elemSize: 3
        } = gpumat
      end

      test "manually allocate a GpuMat" do
        gpumat = Evision.CUDA.GpuMat.gpuMat(1000, 1200, Evision.Constant.cv_8UC3())
        assert Evision.CUDA.GpuMat.cudaPtr(gpumat) > 0

        %GpuMat{
          channels: 3,
          type: {:u, 8},
          raw_type: 16,
          shape: {1000, 1200, 3},
          elemSize: 3
        } = gpumat
      end

      test "split channels" do
        %Mat{} = mat = Evision.imread(Path.join([__DIR__, "testdata", "test.png"]))

        gpumat = GpuMat.gpuMat(mat)
        [b, g, r] = Evision.CUDA.split(gpumat)

        %GpuMat{
          channels: 1,
          type: {:u, 8},
          raw_type: 0,
          shape: {2, 3, 1},
          elemSize: 1
        } = b

        %GpuMat{
          channels: 1,
          type: {:u, 8},
          raw_type: 0,
          shape: {2, 3, 1},
          elemSize: 1
        } = g

        %GpuMat{
          channels: 1,
          type: {:u, 8},
          raw_type: 0,
          shape: {2, 3, 1},
          elemSize: 1
        } = r
      end

      test "transpose" do
        %Mat{} = mat = Evision.imread(Path.join([__DIR__, "testdata", "test.png"]))

        gpumat = GpuMat.gpuMat(mat)
        [b, _, _] = Evision.CUDA.split(gpumat)

        %GpuMat{
          channels: 1,
          type: {:u, 8},
          raw_type: 0,
          shape: {2, 3, 1},
          elemSize: 1
        } = b

        bT = Evision.CUDA.transpose(b)

        %GpuMat{
          channels: 1,
          type: {:u, 8},
          raw_type: 0,
          shape: {3, 2, 1},
          elemSize: 1
        } = bT
      end
    end
  end
end
