defmodule Evision.ML.SVM.Test do
  use ExUnit.Case
  alias Evision, as: Cv

  @moduletag timeout: 120_000

  @tag :ml
  test "svm" do
    labels = [1, -1, -1, -1]
    training_data = [[501, 10], [255, 10], [501, 255], [10, 501]]

    {:ok, labels_mat} =
      Cv.Mat.from_binary(
        Enum.into(labels, <<>>, fn d -> <<d::integer-size(32)-little>> end),
        {:s, 32},
        4,
        1,
        1
      )

    {:ok, training_data_mat} =
      Cv.Mat.from_binary(
        Enum.into(List.flatten(training_data), <<>>, fn d -> <<d::float-size(32)-little>> end),
        {:f, 32},
        4,
        2,
        1
      )

    {:ok, svm} = Cv.ML.SVM.create()
    {:ok, svm} = Cv.ML.SVM.setType(svm, Cv.cv_C_SVC())
    {:ok, svm} = Cv.ML.SVM.setKernel(svm, Cv.cv_LINEAR())
    {:ok, svm} = Cv.ML.SVM.setTermCriteria(svm, {Cv.cv_MAX_ITER(), 100, 0.000001})
    assert :ok = Cv.ML.SVM.train(svm, training_data_mat, Cv.cv_ROW_SAMPLE(), labels_mat)
    assert :ok = Cv.ML.SVM.isTrained(svm)

    {:ok, sv} = Cv.ML.SVM.getUncompressedSupportVectors(svm)
    {:ok, {rows, cols}} = Cv.Mat.shape(sv)
    {:ok, sv_binary} = Cv.Mat.to_binary(sv)
    float_bytes = 4

    support_vector =
      for i <- (rows - 1)..0, reduce: [] do
        support_vector ->
          current_vector =
            for j <- (cols - 1)..0, reduce: [] do
              vec ->
                <<float_data::float-size(32)-little>> =
                  :binary.part(sv_binary, (i * cols + j) * float_bytes, 4)

                [trunc(float_data) | vec]
            end

          [current_vector | support_vector]
      end

    assert [[501, 10], [255, 10], [501, 255]] == support_vector

    green = [0, 255, 0]
    blue = [255, 0, 0]
    width = 512
    height = 512

    response_data =
      for x <- (width - 1)..0, y <- (height - 1)..0, reduce: [] do
        acc ->
          {:ok, sample} =
            Cv.Mat.from_binary(
              <<y::float-size(32)-little, x::float-size(32)-little>>,
              {:f, 32},
              1,
              2,
              1
            )

          {:ok, {_, response_mat}} = Cv.ML.SVM.predict(svm, sample)
          assert {:ok, {1, 1}} = Cv.Mat.shape(response_mat)
          {:ok, <<response::float-size(32)-little>>} = Cv.Mat.to_binary(response_mat)
          response = trunc(response)
          assert response == 1 or response == -1

          case response do
            1 ->
              [green | acc]

            -1 ->
              [blue | acc]
          end
      end

    response_data = response_data |> List.flatten() |> IO.iodata_to_binary()

    {:ok, mat} = Cv.imread(Path.join(__DIR__, ["svm_test.png"]))
    {:ok, expected} = Cv.Mat.to_binary(mat)
    assert expected == response_data
  end
end
