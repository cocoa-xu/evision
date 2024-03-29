if !Code.ensure_loaded?(Evision.CUDA.DFT) do
  defmodule Evision.CUDA.GpuMat.Test do
  end
else
  defmodule Evision.CUDA.GpuMat.Test do
    use ExUnit.Case

    alias Evision.Mat
    alias Evision.CUDA.GpuMat

    describe "Basic Operations" do
      @tag :require_cuda
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

      @tag :require_cuda
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

      @tag :require_cuda
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

      @tag :require_cuda
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

      @tag :require_cuda
      test "abs" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :s8)
        mat = Evision.CUDA.GpuMat.gpuMat(t)
        ret = Evision.CUDA.abs(mat)
        bin = Evision.Mat.to_binary(Evision.CUDA.GpuMat.download(ret))
        assert bin == Nx.to_binary(Nx.abs(t))
      end

      @tag :require_cuda
      test "absSum" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        abs_sum = Nx.to_number(Nx.sum(Nx.abs(t)))
        {^abs_sum, 0.0, 0.0, 0.0} = Evision.CUDA.absSum(t)
      end

      @tag :require_cuda
      test "absSum with mask" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        m = Nx.tensor([[1, 0, 0], [0, 0, 1]], type: :u8)
        abs_sum = Nx.to_number(Nx.sum(Nx.abs(Nx.multiply(t, m))))
        {^abs_sum, 0.0, 0.0, 0.0} = Evision.CUDA.absSum(t, mask: m)
      end

      @tag :require_cuda
      test "absdiff" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        absdiff = Nx.to_binary(Nx.abs(Nx.subtract(t1, t2)))
        assert absdiff == Evision.Mat.to_binary(Evision.CUDA.absdiff(t1, t2))
      end

      @tag :require_cuda
      test "add" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        sum = Nx.to_binary(Nx.add(t1, t2))
        assert sum == Evision.Mat.to_binary(Evision.CUDA.add(t1, t2))
      end

      @tag :require_cuda
      test "addWeighted" do
        t1 = Nx.tensor([[100, 200, 300], [400, 500, 600]], type: :f32)
        alpha = 0.1
        t2 = Nx.tensor([[1000, 2000, 3000], [4000, 5000, 6000]], type: :f32)
        beta = 0.2
        gamma = 10

        weighted_sum =
          Nx.to_binary(Nx.add(Nx.add(Nx.multiply(t1, alpha), Nx.multiply(t2, beta)), gamma))

        assert weighted_sum ==
                 Evision.Mat.to_binary(Evision.CUDA.addWeighted(t1, alpha, t2, beta, gamma))
      end

      @tag :require_cuda
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

      @tag :require_cuda
      test "calcNorm L1" do
        t = Nx.tensor([[10, 10, 20], [20, 20, 30]], type: :u8)

        norm_bin = Evision.Mat.to_binary(Evision.CUDA.calcNorm(t, Evision.Constant.cv_NORM_L1()))
        expected = Nx.to_binary(Nx.as_type(Nx.sum(Nx.abs(t)), :f64))

        assert norm_bin == expected
      end

      @tag :require_cuda
      test "calcNorm L2" do
        t = Nx.tensor([[1, 1]], type: :u8)

        norm_bin = Evision.Mat.to_binary(Evision.CUDA.calcNorm(t, Evision.Constant.cv_NORM_L2()))
        expected = Nx.to_binary(Nx.sqrt(Nx.as_type(Nx.sum(Nx.power(t, 2)), :f64)))

        assert norm_bin == expected
      end

      @tag :require_cuda
      test "calcNorm INF" do
        t = Nx.tensor([1, 42], type: :u8)

        norm_bin = Evision.Mat.to_binary(Evision.CUDA.calcNorm(t, Evision.Constant.cv_NORM_INF()))
        expected = Nx.to_binary(Nx.as_type(Nx.take(t, Nx.argmax(t)), :s32))

        assert norm_bin == expected
      end

      @tag :require_cuda
      test "calcNormDiff L1" do
        t1 = Nx.tensor([[10, 10], [20, 20]], type: :u8)
        t2 = Nx.tensor([[9, 9], [19, 19]], type: :u8)

        norm_bin =
          Evision.Mat.to_binary(
            Evision.CUDA.calcNormDiff(t1, t2, normType: Evision.Constant.cv_NORM_L1())
          )

        expected = Nx.to_binary(Nx.as_type(Nx.sum(Nx.abs(Nx.subtract(t1, t2))), :s32))

        assert norm_bin == expected
      end

      @tag :require_cuda
      test "calcNormDiff L2" do
        t1 = Nx.tensor([[10, 10], [20, 20]], type: :u8)
        t2 = Nx.tensor([[9, 9], [19, 19]], type: :u8)

        norm_bin =
          Evision.Mat.to_binary(
            Evision.CUDA.calcNormDiff(t1, t2, normType: Evision.Constant.cv_NORM_L2())
          )

        expected = Nx.to_binary(Nx.as_type(Nx.sqrt(Nx.sum(Nx.abs(Nx.subtract(t1, t2)))), :f64))

        assert norm_bin == expected
      end

      @tag :require_cuda
      test "calcNormDiff INF" do
        t1 = Nx.tensor([[10, 10], [20, 20]], type: :u8)
        t2 = Nx.tensor([[9, 9], [19, 15]], type: :u8)

        norm_bin =
          Evision.Mat.to_binary(
            Evision.CUDA.calcNormDiff(t1, t2, normType: Evision.Constant.cv_NORM_INF())
          )

        diff = Nx.flatten(Nx.abs(Nx.subtract(t1, t2)))
        expected = Nx.to_binary(Nx.as_type(Nx.take(diff, Nx.argmax(diff)), :s32))

        assert norm_bin == expected
      end

      @tag :require_cuda
      test "calcSqrSum" do
        t = Nx.tensor([[1, 1], [2, 2]], type: :u8)

        sum = Evision.Mat.to_binary(Evision.CUDA.calcSqrSum(t))
        expected = Nx.to_binary(Nx.as_type(Nx.sum(Nx.power(t, 2)), :f64))

        assert sum == expected
      end

      @tag :require_cuda
      test "calcSum" do
        t = Nx.tensor([[1, 1], [2, 2]], type: :u8)

        sum = Evision.Mat.to_binary(Evision.CUDA.calcSum(t))
        expected = Nx.to_binary(Nx.as_type(Nx.sum(t), :f64))

        assert sum == expected
      end

      @tag :require_cuda
      test "cartToPolar" do
        real = Nx.tensor([1, 2, 3, 4], type: :f32)
        imag = Nx.tensor([1, 2, 3, 4], type: :f32)

        {magnitude, angle} = Evision.CUDA.cartToPolar(real, imag)
        magnitude = Nx.reshape(Evision.Mat.to_nx(magnitude, Nx.BinaryBackend), {:auto})
        expected_magnitude = Nx.tensor([1.414213, 2.828427, 4.242640, 5.656854])
        assert Nx.to_number(Nx.all_close(magnitude, expected_magnitude, rtol: 0.0001)) == 1

        angle = Nx.reshape(Evision.Mat.to_nx(angle, Nx.BinaryBackend), {:auto})
        expected_angle = Nx.tensor([0.7853981, 0.7853981, 0.7853981, 0.7853981])
        assert Nx.to_number(Nx.all_close(angle, expected_angle, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "cartToPolar (angleInDegrees)" do
        real = Nx.tensor([1, 2, 3, 4], type: :f32)
        imag = Nx.tensor([1, 2, 3, 4], type: :f32)

        {magnitude, angle} = Evision.CUDA.cartToPolar(real, imag, angleInDegrees: true)
        magnitude = Nx.reshape(Evision.Mat.to_nx(magnitude, Nx.BinaryBackend), {:auto})
        expected_magnitude = Nx.tensor([1.414213, 2.828427, 4.242640, 5.656854])
        assert Nx.to_number(Nx.all_close(magnitude, expected_magnitude, rtol: 0.0001)) == 1

        angle = Nx.reshape(Evision.Mat.to_nx(angle, Nx.BinaryBackend), {:auto})
        expected_angle = Nx.tensor([45.0, 45.0, 45.0, 45.0])
        assert Nx.to_number(Nx.all_close(angle, expected_angle, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "compare CMP_EQ" do
        t1 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :u8)
        t2 = Nx.tensor([[5, 6], [3, 4], [1, 2]], type: :u8)

        ret = Evision.Mat.to_binary(Evision.CUDA.compare(t1, t2, Evision.Constant.cv_CMP_EQ()))
        expected = Nx.to_binary(Nx.multiply(Nx.equal(t1, t2), 255))

        assert ret == expected
      end

      @tag :require_cuda
      test "compare CMP_GT" do
        t1 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :u8)
        t2 = Nx.tensor([[5, 6], [3, 4], [1, 2]], type: :u8)

        ret = Evision.Mat.to_binary(Evision.CUDA.compare(t1, t2, Evision.Constant.cv_CMP_GT()))
        expected = Nx.to_binary(Nx.multiply(Nx.greater(t1, t2), 255))

        assert ret == expected
      end

      @tag :require_cuda
      test "compare CMP_GE" do
        t1 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :u8)
        t2 = Nx.tensor([[5, 6], [3, 4], [1, 2]], type: :u8)

        ret = Evision.Mat.to_binary(Evision.CUDA.compare(t1, t2, Evision.Constant.cv_CMP_GE()))
        expected = Nx.to_binary(Nx.multiply(Nx.greater_equal(t1, t2), 255))

        assert ret == expected
      end

      @tag :require_cuda
      test "compare CMP_LT" do
        t1 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :u8)
        t2 = Nx.tensor([[5, 6], [3, 4], [1, 2]], type: :u8)

        ret = Evision.Mat.to_binary(Evision.CUDA.compare(t1, t2, Evision.Constant.cv_CMP_LT()))
        expected = Nx.to_binary(Nx.multiply(Nx.less(t1, t2), 255))

        assert ret == expected
      end

      @tag :require_cuda
      test "compare CMP_LE" do
        t1 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :u8)
        t2 = Nx.tensor([[5, 6], [3, 4], [1, 2]], type: :u8)

        ret = Evision.Mat.to_binary(Evision.CUDA.compare(t1, t2, Evision.Constant.cv_CMP_LE()))
        expected = Nx.to_binary(Nx.multiply(Nx.less_equal(t1, t2), 255))

        assert ret == expected
      end

      @tag :require_cuda
      test "compare CMP_NE" do
        t1 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :u8)
        t2 = Nx.tensor([[5, 6], [3, 4], [1, 2]], type: :u8)

        ret = Evision.Mat.to_binary(Evision.CUDA.compare(t1, t2, Evision.Constant.cv_CMP_NE()))
        expected = Nx.to_binary(Nx.multiply(Nx.not_equal(t1, t2), 255))

        assert ret == expected
      end

      @tag :require_cuda
      test "exp" do
        t = Nx.tensor([1, 2, 3, 4], type: :f32)
        expected = Nx.exp(t)
        ret = Nx.reshape(Evision.Mat.to_nx(Evision.CUDA.exp(t), Nx.BinaryBackend), {:auto})
        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "flip (x-axis)" do
        t =
          Nx.tensor(
            [
              [0, 1, 0, 2],
              [3, 0, 4, 0]
            ],
            type: :u8
          )

        assert [3, 0, 4, 0, 0, 1, 0, 2] ==
                 Nx.to_flat_list(Evision.Mat.to_nx(Evision.CUDA.flip(t, 0)))
      end

      @tag :require_cuda
      test "flip (y-axis)" do
        t =
          Nx.tensor(
            [
              [0, 1, 0, 2],
              [3, 0, 4, 0]
            ],
            type: :u8
          )

        assert [2, 0, 1, 0, 0, 4, 0, 3] ==
                 Nx.to_flat_list(Evision.Mat.to_nx(Evision.CUDA.flip(t, 1)))
      end

      @tag :require_cuda
      test "flip (both axes)" do
        t =
          Nx.tensor(
            [
              [0, 1, 0, 2],
              [3, 0, 4, 0]
            ],
            type: :u8
          )

        assert [0, 4, 0, 3, 2, 0, 1, 0] ==
                 Nx.to_flat_list(Evision.Mat.to_nx(Evision.CUDA.flip(t, -1)))
      end

      @tag :require_cuda
      test "gemm" do
        # t1.shape == {2, 3}, t2.shape == {3, 2}, t3.shape == {2, 2}
        t1 = Nx.tensor([[1, 2, 3], [3, 4, 5]], type: :f32)
        t2 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :f32)
        t3 = Nx.tensor([[1000, 2000], [3000, 4000]], type: :f32)
        alpha = 0.5
        beta = 1.0

        expected =
          Nx.to_binary(
            Nx.add(
              Nx.multiply(Nx.dot(t1, t2), alpha),
              Nx.multiply(t3, beta)
            )
          )

        assert expected == Evision.Mat.to_binary(Evision.CUDA.gemm(t1, t2, alpha, t3, beta))
      end

      @tag :require_cuda
      test "gemm (GEMM_1_T)" do
        # t1.shape == {3, 2}, t2.shape == {3, 2}, t3.shape == {2, 2}
        t1 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :f32)
        t2 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :f32)
        t3 = Nx.tensor([[1000, 2000], [3000, 4000]], type: :f32)
        alpha = 0.5
        beta = 1.0

        expected =
          Nx.to_binary(
            Nx.add(
              Nx.multiply(Nx.dot(Nx.transpose(t1), t2), alpha),
              Nx.multiply(t3, beta)
            )
          )

        assert expected ==
                 Evision.Mat.to_binary(
                   Evision.CUDA.gemm(t1, t2, alpha, t3, beta,
                     flags: Evision.Constant.cv_GEMM_1_T()
                   )
                 )
      end

      @tag :require_cuda
      test "gemm (GEMM_3_T)" do
        # t1.shape == {2, 3}, t2.shape == {3, 2}, t3.shape == {2, 2}
        t1 = Nx.tensor([[1, 2, 3], [3, 4, 5]], type: :f32)
        t2 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :f32)
        t3 = Nx.tensor([[1000, 3000], [2000, 4000]], type: :f32)
        alpha = 0.5
        beta = 1.0

        expected =
          Nx.to_binary(
            Nx.add(
              Nx.multiply(Nx.dot(t1, t2), alpha),
              Nx.multiply(Nx.transpose(t3), beta)
            )
          )

        assert expected ==
                 Evision.Mat.to_binary(
                   Evision.CUDA.gemm(t1, t2, alpha, t3, beta,
                     flags: Evision.Constant.cv_GEMM_3_T()
                   )
                 )
      end

      @tag :require_cuda
      test "gemm (GEMM_1_T + GEMM_3_T)" do
        # t1.shape == {2, 3}, t2.shape == {3, 2}, t3.shape == {2, 2}
        t1 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :f32)
        t2 = Nx.tensor([[1, 2], [3, 4], [5, 6]], type: :f32)
        t3 = Nx.tensor([[1000, 3000], [2000, 4000]], type: :f32)
        alpha = 0.5
        beta = 1.0

        expected =
          Nx.to_binary(
            Nx.add(
              Nx.multiply(Nx.dot(Nx.transpose(t1), t2), alpha),
              Nx.multiply(Nx.transpose(t3), beta)
            )
          )

        assert expected ==
                 Evision.Mat.to_binary(
                   Evision.CUDA.gemm(t1, t2, alpha, t3, beta,
                     flags: Evision.Constant.cv_GEMM_1_T() + Evision.Constant.cv_GEMM_3_T()
                   )
                 )
      end

      @tag :require_cuda
      test "lshift" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :u8)
        expected = Nx.left_shift(t, 1)
        ret = Evision.Mat.to_nx(Evision.CUDA.lshift(t, {1}), Nx.BinaryBackend)
        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "lshift (3-channel)" do
        t = Nx.reshape(Nx.tensor([1, 1, 1, 1, 1, 1, 1, 1, 1], type: :u8), {3, 3})
        expected = Nx.left_shift(t, Nx.tensor([1, 2, 3]))

        ret =
          Nx.squeeze(
            Evision.Mat.to_nx(
              Evision.CUDA.lshift(
                Evision.Mat.last_dim_as_channel(Nx.reshape(t, {1, 3, 3})),
                {1, 2, 3}
              ),
              Nx.BinaryBackend
            )
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "log" do
        t = Nx.tensor([1, 10, 100, 1000], type: :f32)
        expected = Nx.log(t)
        ret = Nx.reshape(Evision.Mat.to_nx(Evision.CUDA.log(t), Nx.BinaryBackend), {:auto})
        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "magnitude(x, y)" do
        x = Nx.tensor([1, 0, 3, 4], type: :f32)
        y = Nx.tensor([1, 2, 0, 4], type: :f32)
        expected = Nx.sqrt(Nx.add(Nx.power(x, 2), Nx.power(y, 2)))

        ret =
          Nx.reshape(Evision.Mat.to_nx(Evision.CUDA.magnitude(x, y), Nx.BinaryBackend), {:auto})

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "magnitude(xy)" do
        x = Nx.tensor([1, 0, 3, 4], type: :f32)
        y = Nx.tensor([1, 2, 0, 4], type: :f32)
        expected = Nx.sqrt(Nx.add(Nx.power(x, 2), Nx.power(y, 2)))
        xy = Nx.transpose(Nx.stack([x, y]))
        xy = Evision.Mat.last_dim_as_channel(Evision.Mat.from_nx_2d(xy))
        ret = Nx.reshape(Evision.Mat.to_nx(Evision.CUDA.magnitude(xy), Nx.BinaryBackend), {:auto})
        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "magnitudeSqr(x, y)" do
        x = Nx.tensor([1, 0, 3, 4], type: :f32)
        y = Nx.tensor([1, 2, 0, 4], type: :f32)
        expected = Nx.add(Nx.power(x, 2), Nx.power(y, 2))

        ret =
          Nx.reshape(
            Evision.Mat.to_nx(Evision.CUDA.magnitudeSqr(x, y), Nx.BinaryBackend),
            {:auto}
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "magnitudeSqr(xy)" do
        x = Nx.tensor([1, 0, 3, 4], type: :f32)
        y = Nx.tensor([1, 2, 0, 4], type: :f32)
        expected = Nx.add(Nx.power(x, 2), Nx.power(y, 2))
        xy = Nx.transpose(Nx.stack([x, y]))
        xy = Evision.Mat.last_dim_as_channel(Evision.Mat.from_nx_2d(xy))

        ret =
          Nx.reshape(Evision.Mat.to_nx(Evision.CUDA.magnitudeSqr(xy), Nx.BinaryBackend), {:auto})

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "max" do
        t1 = Nx.tensor([1, 0, 3, 0], type: :f32)
        t2 = Nx.tensor([0, 2, 0, 4], type: :f32)
        expected = Nx.max(t1, t2)
        ret = Nx.reshape(Evision.Mat.to_nx(Evision.CUDA.max(t1, t2), Nx.BinaryBackend), {:auto})
        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "min" do
        t1 = Nx.tensor([1, 0, 3, 0], type: :f32)
        t2 = Nx.tensor([0, 2, 0, 4], type: :f32)
        expected = Nx.min(t1, t2)
        ret = Nx.reshape(Evision.Mat.to_nx(Evision.CUDA.min(t1, t2), Nx.BinaryBackend), {:auto})
        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "multiply" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        product = Nx.to_binary(Nx.multiply(t1, t2))
        assert product == Evision.Mat.to_binary(Evision.CUDA.multiply(t1, t2))
      end

      @tag :require_cuda
      test "multiply with scale" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        scale = 2.0
        product = Nx.to_binary(Nx.multiply(Nx.multiply(t1, t2), scale))
        assert product == Evision.Mat.to_binary(Evision.CUDA.multiply(t1, t2, scale: scale))
      end

      @tag :require_cuda
      test "polarToCart" do
        magnitude = Nx.tensor([1.414213, 2.828427, 4.242640, 5.656854], type: :f32)
        angle = Nx.tensor([0.7853981, 0.7853981, 0.7853981, 0.7853981], type: :f32)

        {real, imag} = Evision.CUDA.polarToCart(magnitude, angle)
        real = Nx.reshape(Evision.Mat.to_nx(real, Nx.BinaryBackend), {:auto})
        expected_real = Nx.tensor([1, 2, 3, 4])
        assert Nx.to_number(Nx.all_close(real, expected_real, rtol: 0.0001)) == 1

        imag = Nx.reshape(Evision.Mat.to_nx(imag, Nx.BinaryBackend), {:auto})
        expected_imag = Nx.tensor([1, 2, 3, 4])
        assert Nx.to_number(Nx.all_close(imag, expected_imag, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "polarToCart (angleInDegrees)" do
        magnitude = Nx.tensor([1.414213, 2.828427, 4.242640, 5.656854], type: :f32)
        angle = Nx.tensor([45.0, 45.0, 45.0, 45.0], type: :f32)

        {real, imag} = Evision.CUDA.polarToCart(magnitude, angle, angleInDegrees: true)
        real = Nx.reshape(Evision.Mat.to_nx(real, Nx.BinaryBackend), {:auto})
        expected_real = Nx.tensor([1, 2, 3, 4])
        assert Nx.to_number(Nx.all_close(real, expected_real, rtol: 0.0001)) == 1

        imag = Nx.reshape(Evision.Mat.to_nx(imag, Nx.BinaryBackend), {:auto})
        expected_imag = Nx.tensor([1, 2, 3, 4])
        assert Nx.to_number(Nx.all_close(imag, expected_imag, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "pow" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        power = 0
        expected = Nx.to_binary(Nx.power(t, power))
        assert expected == Evision.Mat.to_binary(Evision.CUDA.pow(t, power))

        power = 1
        expected = Nx.power(t, power)

        assert Nx.to_number(
                 Nx.all_close(
                   expected,
                   Evision.Mat.to_nx(Evision.CUDA.pow(t, power), Nx.BinaryBackend),
                   rtol: 0.0001
                 )
               ) == 1

        power = 2
        expected = Nx.power(t, power)

        assert Nx.to_number(
                 Nx.all_close(
                   expected,
                   Evision.Mat.to_nx(Evision.CUDA.pow(t, power), Nx.BinaryBackend),
                   rtol: 0.0001
                 )
               ) == 1

        power = 3
        expected = Nx.power(t, power)

        assert Nx.to_number(
                 Nx.all_close(
                   expected,
                   Evision.Mat.to_nx(Evision.CUDA.pow(t, power), Nx.BinaryBackend),
                   rtol: 0.0001
                 )
               ) == 1
      end

      @tag :require_cuda
      test "reduce SUM by row" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :u8)

        expected =
          Nx.reduce(t, Nx.tensor(0), [axes: [0], keep_axes: true], fn x, y -> Nx.add(x, y) end)

        ret =
          Evision.Mat.to_nx(
            Evision.CUDA.reduce(t, 0, Evision.Constant.cv_REDUCE_SUM()),
            Nx.BinaryBackend
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "reduce SUM by col" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :u8)

        expected =
          Nx.reduce(t, Nx.tensor(0), [axes: [1], keep_axes: true], fn x, y -> Nx.add(x, y) end)

        ret =
          Evision.Mat.to_nx(
            Evision.CUDA.reduce(t, 1, Evision.Constant.cv_REDUCE_SUM()),
            Nx.BinaryBackend
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "reduce AVG by row" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        expected = Nx.divide(Nx.sum(t, axes: [0]), 2)

        ret =
          Evision.Mat.to_nx(
            Evision.CUDA.reduce(t, 0, Evision.Constant.cv_REDUCE_AVG()),
            Nx.BinaryBackend
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "reduce AVG by col" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        expected = Nx.divide(Nx.sum(t, axes: [1]), 3)

        ret =
          Nx.reshape(
            Evision.Mat.to_nx(
              Evision.CUDA.reduce(t, 1, Evision.Constant.cv_REDUCE_AVG()),
              Nx.BinaryBackend
            ),
            {:auto}
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "reduce MAX by row" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        expected = Nx.reduce_max(t, axes: [0])

        ret =
          Nx.reshape(
            Evision.Mat.to_nx(
              Evision.CUDA.reduce(t, 0, Evision.Constant.cv_REDUCE_MAX()),
              Nx.BinaryBackend
            ),
            {:auto}
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "reduce MAX by col" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        expected = Nx.reduce_max(t, axes: [1])

        ret =
          Nx.reshape(
            Evision.Mat.to_nx(
              Evision.CUDA.reduce(t, 1, Evision.Constant.cv_REDUCE_MAX()),
              Nx.BinaryBackend
            ),
            {:auto}
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "reduce MIN by row" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        expected = Nx.reduce_min(t, axes: [0])

        ret =
          Nx.reshape(
            Evision.Mat.to_nx(
              Evision.CUDA.reduce(t, 0, Evision.Constant.cv_REDUCE_MIN()),
              Nx.BinaryBackend
            ),
            {:auto}
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "reduce MIN by col" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        expected = Nx.reduce_min(t, axes: [1])

        ret =
          Nx.reshape(
            Evision.Mat.to_nx(
              Evision.CUDA.reduce(t, 1, Evision.Constant.cv_REDUCE_MIN()),
              Nx.BinaryBackend
            ),
            {:auto}
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "rshift" do
        t = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :u8)
        expected = Nx.right_shift(t, 1)
        ret = Evision.Mat.to_nx(Evision.CUDA.rshift(t, {1}), Nx.BinaryBackend)
        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "rshift (3-channel)" do
        t = Nx.reshape(Nx.tensor([128, 128, 128, 64, 64, 64, 32, 32, 32], type: :u8), {3, 3})
        expected = Nx.right_shift(t, Nx.tensor([1, 2, 3]))

        ret =
          Nx.squeeze(
            Evision.Mat.to_nx(
              Evision.CUDA.rshift(
                Evision.Mat.last_dim_as_channel(Nx.reshape(t, {1, 3, 3})),
                {1, 2, 3}
              ),
              Nx.BinaryBackend
            )
          )

        assert Nx.to_number(Nx.all_close(expected, ret, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "sqr" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        expected = Nx.power(t, 2)

        assert Nx.to_number(
                 Nx.all_close(expected, Evision.Mat.to_nx(Evision.CUDA.sqr(t), Nx.BinaryBackend),
                   rtol: 0.0001
                 )
               ) == 1
      end

      @tag :require_cuda
      test "sqrIntegral" do
        t = Nx.tensor([1, 2, 3, 4, 5, 6], type: :u8)
        expected = Nx.as_type(Nx.cumulative_sum(Nx.power(t, 2)), :f64)

        sqr_sum =
          Nx.reshape(
            Evision.Mat.to_nx(Evision.CUDA.sqrIntegral(t)[[1..-1, 1]], Nx.BinaryBackend),
            {:auto}
          )

        assert Nx.to_number(Nx.all_close(expected, sqr_sum, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "sqrSum" do
        t = Nx.tensor([1, 2, 3, 4, 5, 6], type: :u8)
        expected = Nx.to_number(Nx.as_type(Nx.sum(Nx.power(t, 2)), :f64))
        {^expected, 0.0, 0.0, 0.0} = Evision.CUDA.sqrSum(t)
      end

      @tag :require_cuda
      test "sqrt" do
        t = Nx.tensor([1, 2, 3, 4, 5, 6, 7, 8, 9], type: :f32)
        expected = Nx.sqrt(t)
        sqrt = Nx.reshape(Evision.Mat.to_nx(Evision.CUDA.sqrt(t), Nx.BinaryBackend), {:auto})
        assert Nx.to_number(Nx.all_close(expected, sqrt, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "sqrt (integer)" do
        t = Nx.tensor([1, 2, 3, 4, 5, 6, 7, 8, 9], type: :u8)
        expected = Nx.as_type(Nx.round(Nx.sqrt(t)), :u8)
        sqrt = Nx.reshape(Evision.Mat.to_nx(Evision.CUDA.sqrt(t), Nx.BinaryBackend), {:auto})
        assert Nx.to_number(Nx.all_close(expected, sqrt, rtol: 0.0001)) == 1
      end

      @tag :require_cuda
      test "subtract" do
        t1 = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        t2 = Nx.tensor([[0, 1, 2], [3, 4, 5]], type: :f32)
        diff = Nx.to_binary(Nx.subtract(t1, t2))
        assert diff == Evision.Mat.to_binary(Evision.CUDA.subtract(t1, t2))
      end

      @tag :require_cuda
      test "sum" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        sum = Nx.to_number(Nx.sum(t))
        {cuda_sum, 0.0, 0.0, 0.0} = Evision.CUDA.sum(t)
        assert sum == cuda_sum
      end

      @tag :require_cuda
      test "sum with mask" do
        t = Nx.tensor([[-1, 2, -3], [4, -5, 6]], type: :f32)
        m = Nx.tensor([[1, 0, 0], [0, 0, 1]], type: :u8)
        sum = Nx.to_number(Nx.sum(Nx.multiply(t, m)))
        {cuda_sum, 0.0, 0.0, 0.0} = Evision.CUDA.sum(t, mask: m)
        assert sum == cuda_sum
      end

      @tag :require_cuda
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
