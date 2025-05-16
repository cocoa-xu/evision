defmodule Evision.Camera.Test do
  use ExUnit.Case

  describe "camera" do
    test "calibration" do
      img = Evision.imread(Path.join([__DIR__, "testdata", "test-circle-grid.jpg"]))
      gray = Evision.cvtColor(img, Evision.Constant.cv_COLOR_BGR2GRAY())
      pattern_size = {12, 8}
      square_size = 0.012
      corners = Evision.findCirclesGrid(gray, pattern_size)
      assert is_struct(corners, Evision.Mat)

      obj_points = generate_object_points(pattern_size, square_size)
      img_points = [corners]

      {height, width, _} = Evision.Mat.shape(img)
      image_size = {width, height}

      {ret, camera_matrix, dist_coeffs, _rvecs, _tvecs} =
        Evision.calibrateCamera(
          objectPoints: [obj_points],
          imagePoints: img_points,
          imageSize: image_size
        )
    end

    defp generate_object_points({rows, cols}, square_size) do
      points =
        for i <- 0..(rows - 1), j <- 0..(cols - 1) do
          x = j * square_size
          y = i * square_size
          z = 0.0
          [x, y, z]
        end

      List.flatten(points) |> Enum.chunk_every(3)
    end
  end
end
