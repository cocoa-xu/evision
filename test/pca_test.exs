defmodule Evision.PCA.Test do
  use ExUnit.Case
  import Bitwise
  alias Evision.Mat

  @moduletag timeout: 120_000

  def drawAxis(src, {px, py}, {qx, qy}, colour, scale) do
    angle = :math.atan2(py - qy, px - qx)
    hypotenuse = :math.sqrt((py - qy) * (py - qy) + (px - qx) * (px - qx))
    qx = trunc(px - scale * hypotenuse * :math.cos(angle))
    qy = trunc(py - scale * hypotenuse * :math.sin(angle))

    %Mat{} =
      src =
      Evision.line(src, {px, py}, {qx, qy}, colour, thickness: 1)

    px = trunc(qx + 9 * :math.cos(angle + :math.pi() / 4))
    py = trunc(qy + 9 * :math.sin(angle + :math.pi() / 4))

    %Mat{} =
      src =
      Evision.line(src, {px, py}, {qx, qy}, colour, thickness: 1)

    px = trunc(qx + 9 * :math.cos(angle - :math.pi() / 4))
    py = trunc(qy + 9 * :math.sin(angle - :math.pi() / 4))

    Evision.line(src, {px, py}, {qx, qy}, colour, thickness: 1)
  end

  @tag :nx
  test "Use the OpenCV class Evision.PCA to calculate the orientation of an object." do
    gray =
      Evision.imread(Path.join([__DIR__, "testdata", "pca_test.jpg"]),
        flags: Evision.Constant.cv_IMREAD_GRAYSCALE()
      )

    {_, bw} =
      Evision.threshold(
        gray,
        50,
        255,
        Evision.Constant.cv_THRESH_BINARY() ||| Evision.Constant.cv_THRESH_OTSU()
      )

    {contours, _} =
      Evision.findContours(
        bw,
        Evision.Constant.cv_RETR_LIST(),
        Evision.Constant.cv_CHAIN_APPROX_NONE()
      )

    contours =
      contours
      |> Enum.map(&{Evision.contourArea(&1), &1})
      |> Enum.reject(fn {area, _c} -> area < 100 or area > 100_000 end)

    assert [17192.0, 16830.0, 16150.5, 15367.5, 15571.0, 14842.0] ==
             Enum.map(contours, &elem(&1, 0))

    contours = Enum.map(contours, &elem(&1, 1))

    pca_analysis =
      for c <- contours, reduce: [] do
        acc ->
          %Mat{shape: shape, type: type} = c
          sz = elem(shape, 0)
          pts_binary = Evision.Mat.to_binary(c)
          data_pts = Evision.Mat.from_binary(pts_binary, type, sz, 2, 1)
          data_pts = Evision.Mat.as_type(data_pts, {:f, 64})

          # Perform PCA analysis
          {mean, eigenvectors, eigenvalues} = Evision.pcaCompute2(data_pts, Evision.Mat.empty())

          eigenvectors = Evision.Mat.to_nx(eigenvectors, Nx.BinaryBackend)
          eigenvalues = Evision.Mat.to_nx(eigenvalues, Nx.BinaryBackend)

          <<centre_x::float-size(64)-little, centre_y::float-size(64)-little, _::binary>> =
            Evision.Mat.to_binary(mean)

          centre_x = trunc(centre_x)
          centre_y = trunc(centre_y)

          eval00 = Nx.slice(eigenvalues, [0, 0], [1, 1]) |> Nx.to_flat_list() |> Enum.at(0)
          eval10 = Nx.slice(eigenvalues, [1, 0], [1, 1]) |> Nx.to_flat_list() |> Enum.at(0)

          evec00 = Nx.slice(eigenvectors, [0, 0], [1, 1]) |> Nx.to_flat_list() |> Enum.at(0)
          evec01 = Nx.slice(eigenvectors, [0, 1], [1, 1]) |> Nx.to_flat_list() |> Enum.at(0)
          evec10 = Nx.slice(eigenvectors, [1, 0], [1, 1]) |> Nx.to_flat_list() |> Enum.at(0)
          evec11 = Nx.slice(eigenvectors, [1, 1], [1, 1]) |> Nx.to_flat_list() |> Enum.at(0)

          p1 =
            {trunc(Float.round(centre_x + 0.02 * evec00 * eval00)),
             trunc(Float.round(centre_y + 0.02 * evec01 * eval00))}

          p2 =
            {trunc(Float.round(centre_x - 0.02 * evec10 * eval10)),
             trunc(Float.round(centre_y - 0.02 * evec11 * eval10))}

          cntr = {centre_x, centre_y}
          [{cntr, p1, p2} | acc]
      end

    pca_analysis = Enum.reverse(pca_analysis)
    assert 6 == Enum.count(pca_analysis)

    src = Evision.imread(Path.join([__DIR__, "testdata", "pca_test.jpg"]))

    src =
      for index <- 0..(Enum.count(contours) - 1), reduce: src do
        src ->
          Evision.drawContours(src, contours, index, {0, 0, 255}, thickness: 2)
      end

    src =
      for {cntr, p1, p2} <- pca_analysis, reduce: src do
        src ->
          src = Evision.circle(src, cntr, 3, {255, 0, 255}, thickness: 2)
          src = drawAxis(src, cntr, p1, {0, 255, 0}, 1)
          drawAxis(src, cntr, p2, {255, 255, 0}, 5)
      end

    output_path = Path.join([__DIR__, "testdata", "pca_test_out.png"])
    Evision.imwrite(output_path, src)
    File.rm!(output_path)
  end
end
