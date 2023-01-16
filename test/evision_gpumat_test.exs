if !Kernel.function_exported?(Evision.CUDA, :split, 1) do
else
  defmodule Evision.CUDA.GpuMat.Test do
    use ExUnit.Case

    alias Evision.Mat
    alias Evision.CUDA.GpuMat

    @tag :require_cuda
    describe "Basic Operations" do
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

      test "calcHist" do
        t = Nx.tensor([[10, 10, 20], [20, 20, 30]], type: :u8)

        # The input matrix should have been uploaded to GPU
        {:error, _} = Evision.CUDA.calcHist(t)

        %GpuMat{} = result = Evision.CUDA.calcHist(GpuMat.gpuMat(t))
        %Mat{} = downloaded = GpuMat.download(result)

        ret = Nx.to_flat_list(Evision.Mat.to_nx(downloaded))
        expected =
          Nx.broadcast(Nx.tensor(0, type: :s32), {256})
          |> Nx.put_slice([10], Nx.tensor([2]))
          |> Nx.put_slice([20], Nx.tensor([3]))
          |> Nx.put_slice([30], Nx.tensor([1]))
          |> Nx.to_flat_list()

        assert ret == expected
      end

      test "calcNorm L1" do
        t = Nx.tensor([[10, 10, 20], [20, 20, 30]], type: :u8)

        norm_bin = Evision.Mat.to_binary(Evision.CUDA.calcNorm(t, Evision.Constant.cv_NORM_L1))
        expected = Nx.to_binary(Nx.as_type(Nx.sum(Nx.abs(t)), :f64))

        assert norm_bin == expected
      end

      test "calcNorm L2" do
        t = Nx.tensor([[1, 1]], type: :u8)

        norm_bin = Evision.Mat.to_binary(Evision.CUDA.calcNorm(t, Evision.Constant.cv_NORM_L2))
        expected = Nx.to_binary(Nx.sqrt(Nx.as_type(Nx.sum(Nx.power(t, 2)), :f64)))

        assert norm_bin == expected
      end

      test "calcNorm INF" do
        t = Nx.tensor([1, 42], type: :u8)

        norm_bin = Evision.Mat.to_binary(Evision.CUDA.calcNorm(t, Evision.Constant.cv_NORM_INF))
        expected = Nx.to_binary(Nx.as_type(Nx.take(t, Nx.argmax(t)), :s32))

        assert norm_bin == expected
      end

      test "calcNormDiff L1" do
        t1 = Nx.tensor([[10, 10], [20, 20]], type: :u8)
        t2 = Nx.tensor([[9, 9], [19, 19]], type: :u8)

        norm_bin = Evision.Mat.to_binary(Evision.CUDA.calcNormDiff(t1, t2, normType: Evision.Constant.cv_NORM_L1))
        expected = Nx.to_binary(Nx.as_type(Nx.sum(Nx.abs(Nx.subtract(t1, t2))), :s32))

        assert norm_bin == expected
      end

      test "calcNormDiff L2" do
        t1 = Nx.tensor([[10, 10], [20, 20]], type: :u8)
        t2 = Nx.tensor([[9, 9], [19, 19]], type: :u8)

        norm_bin = Evision.Mat.to_binary(Evision.CUDA.calcNormDiff(t1, t2, normType: Evision.Constant.cv_NORM_L2))
        expected = Nx.to_binary(Nx.as_type(Nx.sqrt(Nx.sum(Nx.abs(Nx.subtract(t1, t2)))), :f64))

        assert norm_bin == expected
      end

      test "calcNormDiff INF" do
        t1 = Nx.tensor([[10, 10], [20, 20]], type: :u8)
        t2 = Nx.tensor([[9, 9], [19, 15]], type: :u8)

        norm_bin = Evision.Mat.to_binary(Evision.CUDA.calcNormDiff(t1, t2, normType: Evision.Constant.cv_NORM_INF))
        diff = Nx.flatten(Nx.abs(Nx.subtract(t1, t2)))
        expected = Nx.to_binary(Nx.as_type(Nx.take(diff, Nx.argmax(diff)), :s32))

        assert norm_bin == expected
      end

      test "multiply" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        product = Nx.to_binary(Nx.multiply(t1, t2))
        assert product == Evision.Mat.to_binary(Evision.CUDA.multiply(t1, t2))
      end

      test "multiply with scale" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        scale = 2.0
        product = Nx.to_binary(Nx.multiply(Nx.multiply(t1, t2), scale))
        assert product == Evision.Mat.to_binary(Evision.CUDA.multiply(t1, t2, scale: scale))
      end

      test "subtract" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        diff = Nx.to_binary(Nx.subtract(t1, t2))
        assert diff == Evision.Mat.to_binary(Evision.CUDA.subtract(t1, t2))
      end

      test "sum" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        sum = Nx.to_number(Nx.sum(t))
        {cuda_sum, 0.0, 0.0, 0.0} = Evision.CUDA.sum(t)
        assert sum == cuda_sum
      end

      test "sum with mask" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        m = Nx.tensor([[1, 0, 0], [0, 0, 1]], type: :u8)
        sum = Nx.to_number(Nx.sum(Nx.multiply(t, m)))
        {cuda_sum, 0.0, 0.0, 0.0} = Evision.CUDA.sum(t, mask: m)
        assert sum == cuda_sum
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
