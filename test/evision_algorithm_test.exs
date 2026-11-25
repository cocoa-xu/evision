defmodule Evision.Algorithm.Test do
  use ExUnit.Case, async: true

  test "write and read reach the subclass" do
    write_flags = Bitwise.bor(Evision.Constant.cv_WRITE(), Evision.Constant.cv_MEMORY())
    fs = Evision.FileStorage.fileStorage("bm.yml", write_flags)
    Evision.StereoBM.create(numDisparities: 32, blockSize: 15) |> Evision.StereoBM.write(fs)
    yaml = Evision.FileStorage.releaseAndGetString(fs)
    assert yaml =~ "numDisparities: 32"

    read_flags = Bitwise.bor(Evision.Constant.cv_READ(), Evision.Constant.cv_MEMORY())
    fs = Evision.FileStorage.fileStorage(yaml, read_flags)
    bm = Evision.StereoBM.create() |> Evision.StereoBM.read(Evision.FileStorage.root(fs))
    assert Evision.StereoBM.getNumDisparities(bm) == 32
    assert Evision.StereoBM.getBlockSize(bm) == 15
    Evision.FileStorage.release(fs)
  end

  test "clear and getDefaultName reach the subclass" do
    samples = Evision.Mat.literal([[0.0, 0.0], [1.0, 1.0], [0.0, 1.0], [1.0, 0.0]], :f32)
    responses = Evision.Mat.literal([[0], [1], [0], [1]], :s32)
    svm = Evision.ML.SVM.create()
    assert Evision.ML.SVM.train(svm, samples, Evision.Constant.cv_ROW_SAMPLE(), responses)
    assert Evision.ML.SVM.isTrained(svm)

    svm = Evision.ML.SVM.clear(svm)
    refute Evision.ML.SVM.isTrained(svm)
    assert Evision.ML.SVM.getDefaultName(svm) == "opencv_ml_svm"
  end
end
