defmodule Evision.QRCodeEncoder.Test do
  use ExUnit.Case

  @moduletag timeout: 120_000

  test "Encode and decode QRCode" do
    string_to_encode = "This is a string"

    {^string_to_encode, _, _} =
      Evision.QRCodeEncoder.encode(Evision.QRCodeEncoder.create(), string_to_encode)
      |> Evision.resize({300, 300}, interpolation: Evision.cv_INTER_AREA())
      |> Evision.Mat.quicklook()
      |> then(
        &Evision.QRCodeDetector.detectAndDecode(Evision.QRCodeDetector.qrCodeDetector(), &1)
      )
  end
end
