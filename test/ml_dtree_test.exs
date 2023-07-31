defmodule Evision.ML.DTrees.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  @tag :ml
  @tag :require_downloading
  test "decision tree" do
    {features, labels} = Scidata.Wine.download()
    categories = Enum.count(Enum.uniq(labels))
    features = Evision.Mat.from_nx(Nx.tensor(features, type: :f32, backend: Evision.Backend))
    labels = Evision.Mat.from_nx(Nx.tensor(labels, type: :s32, backend: Evision.Backend))

    dataset = Evision.ML.TrainData.create(features, Evision.Constant.cv_ROW_SAMPLE(), labels)
    dataset = Evision.ML.TrainData.setTrainTestSplitRatio(dataset, 0.9, shuffle: true)

    dtree =
      Evision.ML.DTrees.create()
      |> Evision.ML.DTrees.setMaxDepth(7)
      |> Evision.ML.DTrees.setMaxCategories(categories)
      |> Evision.ML.DTrees.setCVFolds(0)

    # train
    Evision.ML.DTrees.train(dtree, dataset)

    # calculate test error
    {test_error, _results} = Evision.ML.DTrees.calcError(dtree, dataset, true)

    # save to file
    dtree_binary = Path.join([__DIR__, "testdata", "dtree.bin"])
    File.rm(dtree_binary)
    Evision.ML.DTrees.save(dtree, dtree_binary)

    # load from file
    dtree2 = Evision.ML.DTrees.load(dtree_binary)
    File.rm(dtree_binary)
    {test_error_2, _results} = Evision.ML.DTrees.calcError(dtree2, dataset, true)
    assert test_error == test_error_2
  end
end
