defmodule Evision.PCA.Test do
  use ExUnit.Case
  use Bitwise
  alias Evision, as: OpenCV

  @moduletag timeout: 120_000

  def drawAxis(src, {px, py}, {qx, qy}, colour, scale) do
    angle = :math.atan2(py - qy, px - qx)
    hypotenuse = :math.sqrt((py - qy) * (py - qy) + (px - qx) * (px - qx))
    qx = trunc(px - scale * hypotenuse * :math.cos(angle))
    qy = trunc(py - scale * hypotenuse * :math.sin(angle))

    {:ok, src} =
      OpenCV.line(src, [px, py], [qx, qy], colour, thickness: 1, style: OpenCV.cv_LINE_AA())

    px = trunc(qx + 9 * :math.cos(angle + :math.pi() / 4))
    py = trunc(qy + 9 * :math.sin(angle + :math.pi() / 4))

    {:ok, src} =
      OpenCV.line(src, [px, py], [qx, qy], colour, thickness: 1, style: OpenCV.cv_LINE_AA())

    px = trunc(qx + 9 * :math.cos(angle - :math.pi() / 4))
    py = trunc(qy + 9 * :math.sin(angle - :math.pi() / 4))
    OpenCV.line(src, [px, py], [qx, qy], colour, thickness: 1, style: OpenCV.cv_LINE_AA())
  end

  @tag :nx
  test "Use the OpenCV class OpenCV.PCA to calculate the orientation of an object." do
    {:ok, gray} =
      Path.join(__DIR__, "opencv_pca_test.jpg")
      |> OpenCV.imread(flags: OpenCV.cv_IMREAD_GRAYSCALE())

    {:ok, {_, bw}} =
      OpenCV.threshold(gray, 50, 255, OpenCV.cv_THRESH_BINARY() ||| OpenCV.cv_THRESH_OTSU())

    {:ok, {contours, _}} =
      OpenCV.findContours(bw, OpenCV.cv_RETR_LIST(), OpenCV.cv_CHAIN_APPROX_NONE())

    contours =
      contours
      |> Enum.map(&{elem(OpenCV.contourArea(&1), 1), &1})
      |> Enum.reject(fn {area, _c} -> area < 100 or area > 100_000 end)

    assert [17192.0, 16830.0, 16150.5, 15367.5, 15571.0, 14842.0] ==
             Enum.map(contours, &elem(&1, 0))

    contours = Enum.map(contours, &elem(&1, 1))

    pca_analysis =
      for c <- contours, reduce: [] do
        acc ->
          {:ok, shape} = OpenCV.Mat.shape(c)
          sz = elem(shape, 0)
          {:ok, pts_binary} = OpenCV.Mat.to_binary(c)
          {:ok, type} = OpenCV.Mat.type(c)
          {:ok, data_pts} = OpenCV.Mat.from_binary(pts_binary, type, sz, 2, 1)
          {:ok, data_pts} = OpenCV.Mat.as_type(data_pts, {:f, 64})

          # Perform PCA analysis
          {:ok, {mean, eigenvectors, eigenvalues}} = OpenCV.pcaCompute2(data_pts, OpenCV.Mat.empty!())
          eigenvectors = OpenCV.Nx.to_nx(eigenvectors)
          eigenvalues = OpenCV.Nx.to_nx(eigenvalues)

          {:ok,
           <<centre_x::float()-size(64)-little, centre_y::float()-size(64)-little, _::binary>>} =
            OpenCV.Mat.to_binary(mean)

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

          cntr = [centre_x, centre_y]
          [{cntr, p1, p2} | acc]
      end

    pca_analysis = Enum.reverse(pca_analysis)
    assert 6 == Enum.count(pca_analysis)

    {:ok, src} =
      Path.join(__DIR__, "opencv_pca_test.jpg")
      |> OpenCV.imread()

    src =
      for index <- 0..(Enum.count(contours) - 1), reduce: src do
        src ->
          {:ok, src} = OpenCV.drawContours(src, contours, index, [0, 0, 255], thickness: 2)
          src
      end

    src =
      for {cntr, p1, p2} <- pca_analysis, reduce: src do
        src ->
          {:ok, src} = OpenCV.circle(src, cntr, 3, [255, 0, 255], thickness: 2)
          {:ok, src} = drawAxis(src, List.to_tuple(cntr), p1, [0, 255, 0], 1)
          {:ok, src} = drawAxis(src, List.to_tuple(cntr), p2, [255, 255, 0], 5)
          src
      end

    output_path = Path.join(__DIR__, "opencv_pca_test_out.png")
    OpenCV.imwrite(output_path, src)
    File.rm!(output_path)
  end
end
