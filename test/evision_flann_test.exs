defmodule Evision.Flann.Test do
  use ExUnit.Case

  # Regression coverage for the cvflann enums (PR #311). These constants
  # live in modules/flann/include/opencv2/flann/defines.h, which OpenCV
  # does not feed to the Python binding generator; gen2.py injects that
  # header itself. A bug in the 4.13 upgrade dropped that injection on
  # the new gen_python_config.json path and the modules below silently
  # disappeared — so an explicit value check on each enum is what catches
  # that regression.

  describe "Evision.Flann.Algorithm (flann_algorithm_t)" do
    test "FLANN_INDEX_* values match OpenCV's cvflann::flann_algorithm_t" do
      assert Evision.Flann.Algorithm.cv_FLANN_INDEX_LINEAR() == 0
      assert Evision.Flann.Algorithm.cv_FLANN_INDEX_KDTREE() == 1
      assert Evision.Flann.Algorithm.cv_FLANN_INDEX_KMEANS() == 2
      assert Evision.Flann.Algorithm.cv_FLANN_INDEX_COMPOSITE() == 3
      assert Evision.Flann.Algorithm.cv_FLANN_INDEX_KDTREE_SINGLE() == 4
      assert Evision.Flann.Algorithm.cv_FLANN_INDEX_HIERARCHICAL() == 5
      assert Evision.Flann.Algorithm.cv_FLANN_INDEX_LSH() == 6
      assert Evision.Flann.Algorithm.cv_FLANN_INDEX_SAVED() == 254
      assert Evision.Flann.Algorithm.cv_FLANN_INDEX_AUTOTUNED() == 255
    end
  end

  describe "Evision.Flann.CentersInit (flann_centers_init_t)" do
    test "FLANN_CENTERS_* values match OpenCV's cvflann::flann_centers_init_t" do
      assert Evision.Flann.CentersInit.cv_FLANN_CENTERS_RANDOM() == 0
      assert Evision.Flann.CentersInit.cv_FLANN_CENTERS_GONZALES() == 1
      assert Evision.Flann.CentersInit.cv_FLANN_CENTERS_KMEANSPP() == 2
      assert Evision.Flann.CentersInit.cv_FLANN_CENTERS_GROUPWISE() == 3
    end
  end

  describe "Evision.Flann.Distance (flann_distance_t)" do
    test "FLANN_DIST_* values match OpenCV's cvflann::flann_distance_t" do
      assert Evision.Flann.Distance.cv_FLANN_DIST_EUCLIDEAN() == 1
      assert Evision.Flann.Distance.cv_FLANN_DIST_L2() == 1
      assert Evision.Flann.Distance.cv_FLANN_DIST_MANHATTAN() == 2
      assert Evision.Flann.Distance.cv_FLANN_DIST_L1() == 2
      assert Evision.Flann.Distance.cv_FLANN_DIST_MINKOWSKI() == 3
      assert Evision.Flann.Distance.cv_FLANN_DIST_MAX() == 4
      assert Evision.Flann.Distance.cv_FLANN_DIST_HIST_INTERSECT() == 5
    end
  end

  describe "Evision.Flann.LogLevel (flann_log_level_t)" do
    test "FLANN_LOG_* values match OpenCV's cvflann::flann_log_level_t" do
      assert Evision.Flann.LogLevel.cv_FLANN_LOG_NONE() == 0
      assert Evision.Flann.LogLevel.cv_FLANN_LOG_FATAL() == 1
      assert Evision.Flann.LogLevel.cv_FLANN_LOG_ERROR() == 2
      assert Evision.Flann.LogLevel.cv_FLANN_LOG_WARN() == 3
      assert Evision.Flann.LogLevel.cv_FLANN_LOG_INFO() == 4
    end
  end

  describe "FlannBasedMatcher accepts the restored constants" do
    test "constructor with KDTREE indexParams (the user-reported reproducer)" do
      matcher =
        Evision.FlannBasedMatcher.flannBasedMatcher(
          indexParams: %{
            "algorithm" => Evision.Flann.Algorithm.cv_FLANN_INDEX_KDTREE(),
            "trees" => 5
          },
          searchParams: %{"checks" => 50}
        )

      assert %Evision.FlannBasedMatcher{} = matcher
    end
  end
end
