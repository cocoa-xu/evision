defmodule Evision.ML.RTrees.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  @tag :ml
  @tag :require_downloading
  test "random forest" do
    {features, labels} = Scidata.Wine.download()
    categories = Enum.count(Enum.uniq(labels))
    features = Evision.Mat.from_nx(Nx.tensor(features, type: :f32, backend: Evision.Backend))
    labels = Evision.Mat.from_nx(Nx.tensor(labels, type: :s32, backend: Evision.Backend))

    dataset = Evision.ML.TrainData.create(features, Evision.Constant.cv_ROW_SAMPLE(), labels)
    dataset = Evision.ML.TrainData.setTrainTestSplitRatio(dataset, 0.8, shuffle: true)

    rtree =
      Evision.ML.RTrees.create()
      |> Evision.ML.RTrees.setMaxDepth(10)
      |> Evision.ML.RTrees.setMaxCategories(categories)
      |> Evision.ML.RTrees.setCVFolds(1)
      |> Evision.ML.RTrees.setTermCriteria({Evision.Constant.cv_MAX_ITER(), 10, 0.00005})

    # train
    Evision.ML.RTrees.train(rtree, dataset)

    # calculate test error
    {test_error, _results} = Evision.ML.RTrees.calcError(rtree, dataset, true)

    # save to file
    rtree_binary = Path.join([__DIR__, "testdata", "rtree.bin"])
    File.rm(rtree_binary)
    Evision.ML.RTrees.save(rtree, rtree_binary)

    # load from file
    rtree2 = Evision.ML.RTrees.load(rtree_binary)
    File.rm(rtree_binary)
    {test_error_2, _results} = Evision.ML.RTrees.calcError(rtree2, dataset, true)
    assert test_error == test_error_2
  end
end
