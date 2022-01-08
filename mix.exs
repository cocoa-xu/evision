defmodule Evision.MixProject do
  use Mix.Project

  @app :evision
  @version "0.1.0-dev"
  @opencv_version "4.5.4"
  @source_url "https://github.com/cocoa-xu/evision/tree/#{@opencv_version}"

  def project do
    {cmake_options, enabled_modules} = generate_cmake_options()
    [
      app: @app,
      name: "Evision",
      version: @version,
      elixir: "~> 1.11-dev",
      deps: deps(),
      docs: docs(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      elixirc_paths: elixirc_paths(Mix.env()),
      source_url: "https://github.com/cocox-xu/evision",
      description: description(),
      package: package(),
      make_env: %{
        "OPENCV_VER" => @opencv_version,
        "MAKE_BUILD_FLAGS" => System.get_env("MAKE_BUILD_FLAGS", "-j#{System.schedulers_online()}"),
        "CMAKE_OPTIONS" => cmake_options,
        "ENABLED_CV_MODULES" => enabled_modules
      }
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(_), do: ~w(lib)

  defp default_enabled_modules do
    "calib3d,core,features2d,flann,highgui,imgcodecs,imgproc,ml,photo,stitching,ts,video,videoio"
  end

  defp default_disable_modules do
    "dnn,gapi,world,python2,python3,java,objdetect"
  end

  defp default_img_codecs do
    "png,jpeg,tiff,webp,openjpeg,jasper,openexr"
  end

  defp generate_cmake_options do
    enabled_modules = System.get_env("enabled_modules", default_enabled_modules())   |> String.split(",", trim: true)
    disabled_modules = System.get_env("disabled_modules", default_disable_modules()) |> String.split(",", trim: true)
    enabled_img_codecs = System.get_env("img_codecs", default_img_codecs()) |> String.split(",", trim: true)
    compile_mode = System.get_env("compile_mode", "auto")
    all_modules = (default_enabled_modules() <> "," <> default_disable_modules()) |> String.split(",", trim: true) |> Enum.uniq

    {enabled_modules, disabled_modules} = case compile_mode do
      "auto" ->
        {enabled_modules, disabled_modules}
      "only_enabled_modules" ->
        {enabled_modules, all_modules -- enabled_modules}
      "except_disabled_modules" ->
        {all_modules -- disabled_modules, disabled_modules}
      unrecognised_mode ->
        IO.error("unrecognised compile_mode: #{unrecognised_mode}")
    end

    options = (enabled_modules
      |> Enum.map(&("-D BUILD_opencv_#{&1}=ON"))
      |> Enum.join(" "))
    <> " " <> (disabled_modules
      |> Enum.map(&("-D BUILD_opencv_#{&1}=OFF"))
      |> Enum.join(" "))
    <> " " <> (enabled_img_codecs
      |> Enum.map(&("-D BUILD_#{&1 |> String.upcase}=ON"))
      |> Enum.join(" "))
    <> " "

    {options, enabled_modules |> Enum.join(",")}
  end

  defp deps do
    [
      {:elixir_make, "~> 0.6"},
      {:ex_doc, "~> 0.23", only: :dev, runtime: false}
    ]
  end

  defp docs do
    [
      main: "OpenCV",
      source_ref: "v#{@version}",
      source_url: @source_url,
      before_closing_body_tag: &before_closing_body_tag/1,
      groups_for_functions: [
        cv: &(&1[:namespace] == :cv),
        "cv.Error": &(&1[:namespace] == :"cv.Error"),
        "cv.ipp": &(&1[:namespace] == :"cv.ipp"),
        "cv.utils": &(&1[:namespace] == :"cv.utils"),
        "cv.utils.fs": &(&1[:namespace] == :"cv.utils.fs"),
        "cv.detail": &(&1[:namespace] == :"cv.detail"),
        "cv.cuda": &(&1[:namespace] == :"cv.cuda"),
        "cv.ocl": &(&1[:namespace] == :"cv.ocl"),
        "cv.ogl": &(&1[:namespace] == :"cv.ogl"),
        "cv.parallel": &(&1[:namespace] == :"cv.parallel"),
        "cv.samples": &(&1[:namespace] == :"cv.samples"),
        "cv.flann": &(&1[:namespace] == :"cv.flann"),
        "cv.segmentation": &(&1[:namespace] == :"cv.segmentation"),
        "cv.ml": &(&1[:namespace] == :"cv.ml"),
        "cv.videoio_registry": &(&1[:namespace] == :"cv.videoio_registry"),
        "cv.fisheye": &(&1[:namespace] == :"cv.fisheye"),
        Constants: &(&1[:type] == :constants),
      ]
    ]
  end

  defp before_closing_body_tag(:html) do
    ~S(
    <script type="text/x-mathjax-config">
    MathJax.Hub.Config({
        tex2jax: {
            inlineMath: [["\\f$", "\\f$"]],
            displayMath: [["\\f[", "\\f]"]],
            skipTags: ["script", "noscript", "style", "textarea"],
            processEnvironments: false
        },
        extensions: ["tex2jax.js", "TeX/AMSmath.js", "TeX/AMSsymbols.js"],
        jax: ["input/TeX", "output/HTML-CSS"],
    }\);
    //<![CDATA[
    MathJax.Hub.Config(
    {
        TeX: {
            Macros: {
                matTT: ["\\[ \\left|\\begin{array}{ccc} #1 & #2 & #3\\\\ #4 & #5 & #6\\\\ #7 & #8 & #9 \\end{array}\\right| \\]", 9],
                fork: ["\\left\\{ \\begin{array}{l l} #1 & \\mbox{#2}\\\\ #3 & \\mbox{#4}\\\\ \\end{array} \\right.", 4],
                forkthree: ["\\left\\{ \\begin{array}{l l} #1 & \\mbox{#2}\\\\ #3 & \\mbox{#4}\\\\ #5 & \\mbox{#6}\\\\ \\end{array} \\right.", 6],
                forkfour: ["\\left\\{ \\begin{array}{l l} #1 & \\mbox{#2}\\\\ #3 & \\mbox{#4}\\\\ #5 & \\mbox{#6}\\\\ #7 & \\mbox{#8}\\\\ \\end{array} \\right.", 8],
                vecthree: ["\\begin{bmatrix} #1\\\\ #2\\\\ #3 \\end{bmatrix}", 3],
                vecthreethree: ["\\begin{bmatrix} #1 & #2 & #3\\\\ #4 & #5 & #6\\\\ #7 & #8 & #9 \\end{bmatrix}", 9],
                cameramatrix: ["#1 = \\begin{bmatrix} f_x & 0 & c_x\\\\ 0 & f_y & c_y\\\\ 0 & 0 & 1 \\end{bmatrix}", 1],
                distcoeffs: ["(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6 [, s_1, s_2, s_3, s_4[, \\tau_x, \\tau_y]]]]\) \\text{ of 4, 5, 8, 12 or 14 elements}"],
                distcoeffsfisheye: ["(k_1, k_2, k_3, k_4\)"],
                hdotsfor: ["\\dots", 1],
                mathbbm: ["\\mathbb{#1}", 1],
                bordermatrix: ["\\matrix{#1}", 1]
            }
        }
    }
    \);
    //]]>
    </script>
    <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js"></script>
    )
  end

  defp before_closing_body_tag(_), do: ""

  defp description() do
    "OpenCV-Erlang/Elixir bindings."
  end

  defp package() do
    [
      name: "evision",
      # These are the default files included in the package
      files: ~w(lib c_src py_src nerves 3rd_party priv .formatter.exs mix.exs README* readme* LICENSE*
                license* CHANGELOG* changelog* src),
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/cocoa-xu/evision"}
    ]
  end
end
