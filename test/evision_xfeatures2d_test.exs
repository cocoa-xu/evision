defmodule Evision.XFeatures2D.Test do
  use ExUnit.Case

  setup do
    %Evision.Mat{} = image = Evision.imread(Path.join([__DIR__, "testdata", "back.jpg"]))
    %{image: image}
  end

  describe "implicit cast to Evision.XFeatures2D okay" do
    test "from Evision.XFeatures2D.BEBLID.t()", %{image: image} do
      detector = Evision.XFeatures2D.BEBLID.create(0.5)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.BoostDesc.t()", %{image: image} do
      detector = Evision.XFeatures2D.BoostDesc.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.BriefDescriptorExtractor.t()", %{image: image} do
      detector = Evision.XFeatures2D.BriefDescriptorExtractor.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.DAISY.t()", %{image: image} do
      detector = Evision.XFeatures2D.DAISY.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.FREAK.t()", %{image: image} do
      detector = Evision.XFeatures2D.FREAK.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.LATCH.t()", %{image: image} do
      detector = Evision.XFeatures2D.LATCH.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.LUCID.t()", %{image: image} do
      detector = Evision.XFeatures2D.LUCID.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.MSDDetector.t()", %{image: image} do
      detector = Evision.XFeatures2D.MSDDetector.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.StarDetector.t()", %{image: image} do
      detector = Evision.XFeatures2D.StarDetector.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.TBMR.t()", %{image: image} do
      detector = Evision.XFeatures2D.TBMR.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.TEBLID.t()", %{image: image} do
      detector = Evision.XFeatures2D.TEBLID.create(1.0)
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end

    test "from Evision.XFeatures2D.VGG.t()", %{image: image} do
      detector = Evision.XFeatures2D.VGG.create()
      {:error, msg} = Evision.Detail.computeImageFeatures2(detector, image)
      assert msg =~ "The function/feature is not implemented"
    end
  end
end
