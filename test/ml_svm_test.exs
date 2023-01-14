defmodule Evision.ML.SVM.Test do
  use ExUnit.Case
  alias Evision.Mat
  alias Evision.ML.SVM

  @moduletag timeout: 120_000

  @tag :ml
  test "svm" do
    labels = [1, -1, -1, -1]
    training_data = [[501, 10], [255, 10], [501, 255], [10, 501]]

    %Mat{} =
      labels_mat =
      Mat.from_binary(
        Enum.into(labels, <<>>, fn d -> <<d::integer-size(32)-little>> end),
        {:s, 32},
        4,
        1,
        1
      )

    %Mat{} =
      training_data_mat =
      Mat.from_binary(
        Enum.into(List.flatten(training_data), <<>>, fn d -> <<d::float-size(32)-little>> end),
        {:f, 32},
        4,
        2,
        1
      )

    svm = SVM.create()
    svm = SVM.setType(svm, Evision.Constant.cv_C_SVC())
    svm = SVM.setKernel(svm, Evision.Constant.cv_LINEAR())
    svm = SVM.setTermCriteria(svm, {Evision.Constant.cv_MAX_ITER(), 100, 0.000001})
    assert true = SVM.train(svm, training_data_mat, Evision.Constant.cv_ROW_SAMPLE(), labels_mat)
    assert true = SVM.isTrained(svm)

    %Mat{shape: {rows, cols}} = sv = SVM.getUncompressedSupportVectors(svm)
    sv_binary = Mat.to_binary(sv)
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
          %Mat{} =
            sample =
            Mat.from_binary(
              <<y::float-size(32)-little, x::float-size(32)-little>>,
              {:f, 32},
              1,
              2,
              1
            )

          {_, %Mat{shape: {1, 1}} = response_mat} = SVM.predict(svm, sample)
          <<response::float-size(32)-little>> = Mat.to_binary(response_mat)
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

    mat = Evision.imread(Path.join([__DIR__, "testdata", "svm_test.png"]))
    expected = Evision.Mat.to_binary(mat)
    assert expected == response_data
  end
end
