defmodule Evision.Face.FaceRecognizerTest do
  use ExUnit.Case

  setup do
    %Evision.Mat{} = image = Evision.imread(
      Path.join([__DIR__, "testdata", "back.jpg"]), 
      flags: Evision.Constant.cv_IMREAD_GRAYSCALE())
    %{image: Evision.Mat.as_type(image, :s32)}
  end

  describe "functions from base class Evision.Face.FaceRecognizer is available in sub-classes" do
    test "Evision.Face.EigenFaceRecognizer.t()", %{image: image} do
      module = Evision.Face.EigenFaceRecognizer
      face_recogniser = module.create()
      face_recogniser = module.train(face_recogniser, [image], Nx.tensor([0], type: :s32))
      assert face_recogniser.__struct__ == module
    end

    test "Evision.Face.FisherFaceRecognizer.t()", %{image: image} do
      module = Evision.Face.FisherFaceRecognizer
      face_recogniser = module.create()
      face_recogniser = module.train(face_recogniser, [image, image], Nx.tensor([0, 1], type: :s32))
      assert face_recogniser.__struct__ == module
    end

    test "Evision.Face.LBPHFaceRecognizer.t()", %{image: image} do
      module = Evision.Face.LBPHFaceRecognizer
      face_recogniser = module.create()
      face_recogniser = module.train(face_recogniser, [image], Nx.tensor([0], type: :s32))
      assert face_recogniser.__struct__ == module
    end
  end
end
