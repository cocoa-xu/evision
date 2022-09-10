defmodule Evision.ML.DTrees.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  @wine_mirror_base "https://mirrors.uwucocoa.moe/dataset/wine/"
  @tag :ml
  @tag :require_downloading
  test "decision tree" do
    {features, labels} = Scidata.Wine.download(base_url: @wine_mirror_base)
    categories = Enum.count(Enum.uniq(labels))
    features = Evision.Nx.to_mat!(Nx.tensor(features, type: :f32, backend: Evision.Backend))
    labels = Evision.Nx.to_mat!(Nx.tensor(labels, type: :s32, backend: Evision.Backend))

    dataset = Evision.ML.TrainData.create!(features, Evision.cv_ROW_SAMPLE(), labels)
    dataset = Evision.ML.TrainData.setTrainTestSplitRatio!(dataset, 0.9, shuffle: true)

    dtree = Evision.ML.DTrees.create!()
      |> Evision.ML.DTrees.setMaxDepth!(7)
      |> Evision.ML.DTrees.setMaxCategories!(categories)
      |> Evision.ML.DTrees.setCVFolds!(0)

    # train
    Evision.ML.DTrees.train!(dtree, dataset)

    # calculate test error
    {test_error, _results} = Evision.ML.DTrees.calcError!(dtree, dataset, true)

    # save to file
    dtree_binary = Path.join(__DIR__, "dtree.bin")
    File.rm(dtree_binary)
    Evision.ML.DTrees.save!(dtree, dtree_binary)

    # load from file
    dtree2 = Evision.ML.DTrees.load!(dtree_binary)
    File.rm(dtree_binary)
    {test_error_2, _results} = Evision.ML.DTrees.calcError!(dtree2, dataset, true)
    assert test_error == test_error_2
  end
end
