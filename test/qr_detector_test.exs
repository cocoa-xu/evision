defmodule Evision.QRCodeDetector.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  test "detect and decide QRCode in an image" do
    qr = Evision.QRCodeDetector.qrCodeDetector()
    img = Evision.imread(Path.join([__DIR__, "qr_detector_test.png"]), flags: 0)
    {text, points, straight_qrcode} = Evision.QRCodeDetector.detectAndDecode(qr, img)
    assert text == "https://github.com/cocoa-xu/evision"
    points_nx = Nx.tensor([[
        [34.0, 34.0],
        [265.0, 34.0],
        [265.0, 265.0],
        [34.0, 265.0]
    ]], type: :f32)
    points_out = Evision.Nx.to_nx(points, Nx.BinaryBackend)
    assert 1 == Nx.to_number(Nx.all_close(points_nx, points_out))

    bin = Evision.Mat.to_binary(straight_qrcode)
    expected = File.read!(Path.join([__DIR__, "straight_qrcode.bin"]))
    assert bin == expected
  end
end
