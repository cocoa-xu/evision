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

        gpumat = GpuMat.gpuMat(mat)

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

      test "abs" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :s8)
        mat = Evision.CUDA.GpuMat.gpuMat(t)
        ret = Evision.CUDA.abs(mat)
        bin = Evision.Mat.to_binary(Evision.CUDA.GpuMat.download(ret))
        assert bin == Nx.to_binary(Nx.abs(t))
      end

      test "absSum" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        abs_sum = Nx.to_number(Nx.sum(Nx.abs(t)))
        {^abs_sum, 0.0, 0.0, 0.0} = Evision.CUDA.absSum(t)
      end

      test "absSum with mask" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        m = Nx.tensor([[1, 0, 0], [0, 0, 1]], type: :u8)
        abs_sum = Nx.to_number(Nx.sum(Nx.abs(Nx.multiply(t, m))))
        {^abs_sum, 0.0, 0.0, 0.0} = Evision.CUDA.absSum(t, mask: m)
      end

      test "absdiff" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        absdiff = Nx.to_binary(Nx.abs(Nx.subtract(t1, t2)))
        assert absdiff == Evision.Mat.to_binary(Evision.CUDA.absdiff(t1, t2))
      end

      test "add" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        sum = Nx.to_binary(Nx.add(t1, t2))
        assert sum == Evision.Mat.to_binary(Evision.CUDA.add(t1, t2))
      end

      test "addWeighted" do
        t1 = Nx.tensor([[100, 200, 300], [400, 500, 600]], type: :f32)
        alpha = 0.1
        t2 = Nx.tensor([[1000, 2000, 3000], [4000, 5000, 6000]], type: :f32)
        beta = 0.2
        gamma = 10

        weighted_sum = Nx.to_binary(Nx.add(Nx.add(Nx.multiply(t1, alpha), Nx.multiply(t2, beta)), gamma))
        assert weighted_sum == Evision.Mat.to_binary(Evision.CUDA.addWeighted(t1, alpha, t2, beta, gamma))
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
