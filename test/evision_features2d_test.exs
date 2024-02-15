defmodule Evision.Features2D.Test do
  use ExUnit.Case

  setup do
    %Evision.Mat{} = image = Evision.imread(Path.join([__DIR__, "testdata", "back.jpg"]))
    opts = [nfeatures: 500]
    %{image: image, opts: opts}
  end

  describe "implicit cast to Evision.Features2D" do
    test "cast from Evision.AKAZE.t()", %{image: image, opts: opts} do
      detector = Evision.AKAZE.create(opts)
      features = Evision.Detail.computeImageFeatures2(detector, image)
      assert Evision.Detail.ImageFeatures == features.__struct__
    end

    test "cast from Evision.AffineFeature.t()", %{image: image, opts: opts} do
      detector = Evision.AKAZE.create(opts)
      affine_detector = Evision.AffineFeature.create(detector)
      features = Evision.Detail.computeImageFeatures2(affine_detector, image)
      assert Evision.Detail.ImageFeatures == features.__struct__
    end

    test "cast from Evision.BRISK.t()", %{image: image, opts: opts} do
      detector = Evision.BRISK.create(opts)
      features = Evision.Detail.computeImageFeatures2(detector, image)
      assert Evision.Detail.ImageFeatures == features.__struct__
    end

    test "cast from Evision.KAZE.t()", %{image: image, opts: opts} do
      detector = Evision.KAZE.create(opts)
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
    test "cast from Evision.AgastFeatureDetector.t()", %{image: image, opts: opts} do
      detector = Evision.AgastFeatureDetector.create(opts)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "cast from Evision.FastFeatureDetector.t()", %{image: image, opts: opts} do
      detector = Evision.FastFeatureDetector.create(opts)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "cast from Evision.GFTTDetector.t()", %{image: image, opts: opts} do
      detector = Evision.GFTTDetector.create(opts)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "cast from Evision.MSER.t()", %{image: image, opts: opts} do
      detector = Evision.MSER.create(opts)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "cast from Evision.SimpleBlobDetector.t()", %{image: image, opts: opts} do
      detector = Evision.SimpleBlobDetector.create(opts)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end
  end
end
