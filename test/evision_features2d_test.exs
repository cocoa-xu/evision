defmodule Evision.Features2D.Test do
  use ExUnit.Case

  setup do
    %Evision.Mat{} = image = Evision.imread(Path.join([__DIR__, "testdata", "back.jpg"]))
    opts = [nfeatures: 500]
    %{image: image, opts: opts}
  end

  describe "implicit cast to Evision.Features2D" do
    test "cast from Evision.AKAZE.t()", %{image: image} do
      detector = Evision.AKAZE.create(nil)
      features = Evision.Detail.computeImageFeatures2(detector, image)
      assert Evision.Detail.ImageFeatures == features.__struct__
    end

    test "cast from Evision.AffineFeature.t()", %{image: image} do
      detector = Evision.AKAZE.create(nil)
      affine_detector = Evision.AffineFeature.create(detector)
      features = Evision.Detail.computeImageFeatures2(affine_detector, image)
      assert Evision.Detail.ImageFeatures == features.__struct__
    end

    test "cast from Evision.BRISK.t()", %{image: image} do
      detector = Evision.BRISK.create(thresh: 30, octaves: 3)
      features = Evision.Detail.computeImageFeatures2(detector, image)
      assert Evision.Detail.ImageFeatures == features.__struct__
    end

    test "cast from Evision.KAZE.t()", %{image: image} do
      detector = Evision.KAZE.create(nil)
      features = Evision.Detail.computeImageFeatures2(detector, image)
      assert Evision.Detail.ImageFeatures == features.__struct__
    end

    test "cast from Evision.ORB.t()", %{image: image, opts: opts} do
      detector = Evision.ORB.create(opts)
      features = Evision.Detail.computeImageFeatures2(detector, image)
      assert Evision.Detail.ImageFeatures == features.__struct__
    end
  end

  describe "implicit cast to Evision.Features2D okay but not implemented detectAndCompute" do
    test "cast from Evision.AgastFeatureDetector.t()", %{image: image} do
      detector = Evision.AgastFeatureDetector.create(nil)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "cast from Evision.FastFeatureDetector.t()", %{image: image} do
      detector = Evision.FastFeatureDetector.create(nil)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "cast from Evision.GFTTDetector.t()", %{image: image} do
      detector = Evision.GFTTDetector.create(nil)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "cast from Evision.MSER.t()", %{image: image} do
      detector = Evision.MSER.create(nil)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "cast from Evision.SimpleBlobDetector.t()", %{image: image} do
      detector = Evision.SimpleBlobDetector.create(nil)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end
  end
end
