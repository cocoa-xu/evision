defmodule Evision.MixProject.Metadata do
  @moduledoc false

  def app, do: :evision
  def version, do: "0.1.8"
  def last_released_version, do: "0.1.8"
  def github_url, do: "https://github.com/cocoa-xu/evision"
  def opencv_version, do: "4.6.0"
  # only means compatible. need to write more tests
  def compatible_opencv_versions, do: ["4.5.3", "4.5.4", "4.5.5", "4.6.0"]
end

defmodule Mix.Tasks.Compile.EvisionPrecompiled do
  @moduledoc false

  use Mix.Task
  require Logger
  alias Evision.MixProject.Metadata

  @available_versions [
    "0.1.7",
    "0.1.6",
    "0.1.5",
    "0.1.4",
    "0.1.3",
    "0.1.2",
    "0.1.1",
    "0.1.0-dev",
  ]

  @available_targets [
    "aarch64-apple-darwin",
    "x86_64-apple-darwin",
    "aarch64-linux-gnu",
    "aarch64-linux-musl",
    "x86_64-linux-gnu",
    "x86_64-linux-musl",
    "armv7l-linux-gnueabihf",
    "ppc64le-linux-gnu",
    "s390x-linux-gnu",
    "riscv64-linux-gnu",
    "riscv64-linux-musl",
    "x86_64-windows-msvc",
  ]

  def available_nif_urls(version \\ Metadata.version()) do
    Enum.map(@available_targets, fn target -> get_download_url(target, version) end)
  end

  def current_target_nif_url(version \\ Metadata.version()) do
    {target, _} = get_target()
    get_download_url(target, version)
  end

  def checksum_file(app \\ Mix.Project.config()[:app]) when is_atom(app) do
    # Saves the file in the project root.
    Path.join(File.cwd!(), "checksum-#{to_string(app)}.exs")
  end

  defp read_checksum_map(app \\ Mix.Project.config()[:app]) when is_atom(app) do
    file = checksum_file(app)
    with {:ok, contents} <- File.read(file),
         {%{} = contents, _} <- Code.eval_string(contents) do
      contents
    else
      _ -> %{}
    end
  end

  def get_target do
    target = String.split(to_string(:erlang.system_info(:system_architecture)), "-")
    [arch, os, abi] = case Enum.count(target) do
      3 ->
        target
      4 ->
        [arch, _vendor, os, abi] = target
        [arch, os, abi]
    end

    abi =
      case abi do
        "darwin" <> _ ->
          "darwin"

        "win32" ->
          {compiler_id, _} = :erlang.system_info(:c_compiler_used)

          case compiler_id do
            :msc -> "msvc"
            _ -> to_string(compiler_id)
          end

        _ ->
          abi
      end

    abi = System.get_env("TARGET_ABI", abi)
    os = System.get_env("TARGET_OS", os)
    arch = System.get_env("TARGET_ARCH", arch)
    {Enum.join([arch, os, abi], "-"), [arch, os, abi]}
  end

  def use_precompiled?(log? \\ false) do
    prefer_precompiled_default =
      if "dev" in Version.parse!(Metadata.version()).pre do
        "false"
      else
        "true"
      end
    prefer_precompiled = System.get_env("EVISION_PREFER_PRECOMPILED", prefer_precompiled_default)
    use_precompiled? = prefer_precompiled == "true"

    if log? do
      if use_precompiled? do
        Logger.info("EVISION_PREFER_PRECOMPILED: true; try to download and use the precompiled library.")
      else
        Logger.info("EVISION_PREFER_PRECOMPILED: #{prefer_precompiled}; will not use precompiled binaries.")
      end
    end

    use_precompiled?
  end

  def available_for_version?(version, log? \\ false) do
    available_for_version? = Enum.member?(@available_versions, version)

    if log? do
      if available_for_version? do
        Logger.info("Requested version `#{version}` has precompiled binaries.")
      else
        Logger.warning("Requested version `#{version}` does not have precompiled binaries.")
      end
    end

    available_for_version?
  end

  def available_for_target?(target, log? \\ false) do
    available_for_target? = Enum.member?(@available_targets, target)

    if log? do
      if available_for_target? do
        Logger.info("Current target `#{target}` has precompiled binaries.")
      else
        Logger.warning("Current target `#{target}` does not have precompiled binaries.")
      end
    end

    available_for_target?
  end

  def download!(url, save_as, overwrite \\ false)

  def download!(url, save_as, false) do
    unless File.exists?(save_as) do
      download!(url, save_as, true)
    end

    checksum(save_as)
  end

  def download!(url, save_as, true) do
    http_opts = []
    opts = [body_format: :binary]
    arg = {url, []}

    case :httpc.request(:get, arg, http_opts, opts) do
      {:ok, {{_, 200, _}, _, body}} ->
        File.write!(save_as, body)
        checksum(save_as)

      {:error, reason} ->
        raise RuntimeError, "Cannot download file from #{url}: #{reason}."
    end
  end

  def filename(target, version, with_ext \\ "") do
    "evision-#{target}-#{version}#{with_ext}"
  end

  def get_download_url(target, version) do
    tar_file = filename(target, version, ".tar.gz")
    "#{Metadata.github_url()}/releases/download/v#{version}/#{tar_file}"
  end

  def cache_dir do
    cache_opts = if System.get_env("MIX_XDG"), do: %{os: :linux}, else: %{}
    cache_dir = :filename.basedir(:user_cache, "", cache_opts)

    cache_dir = System.get_env("ELIXIR_MAKE_CACHE_DIR", cache_dir)

    File.mkdir_p!(cache_dir)
    cache_dir
  end

  @checksum_algo :sha256
  def checksum(file_path, algo \\ @checksum_algo) do
    case File.read(file_path) do
      {:ok, content} ->
        file_hash =
          algo
          |> :crypto.hash(content)
          |> Base.encode16(case: :lower)

        {:ok, "#{algo}", "#{file_hash}"}

      {:error, reason} ->
        {:error,
         "cannot read the file for checksum comparison: #{inspect(file_path)}. " <>
           "Reason: #{inspect(reason)}"}
    end
  end

  def app_priv(app \\ Metadata.app()) when is_atom(app) do
    build_path = Mix.Project.build_path()
    Path.join([build_path, "lib", "#{app}", "priv"])
  end

  def prepare(target, os, version) do
    name = filename(target, version)
    filename = filename(target, version, ".tar.gz")
    cache_dir = cache_dir()
    cache_file = Path.join([cache_dir, filename])
    unarchive_dest_dir = Path.join([cache_dir, name])
    cached_priv_dir = Path.join([cache_dir, name, "priv"])
    cached_elixir_dir = Path.join([cache_dir, name, "elixir_generated"])

    evision_so_file =
      if os == "windows" do
        "evision.dll"
      else
        "evision.so"
      end
    evision_so_file = Path.join([app_priv(), evision_so_file])

    # first we check if we already have the NIF file
    # if not, then we defintely need to copy it (and other files) from somewhere
    needs_copy = !File.exists?(evision_so_file)

    if needs_copy do
      # to copy the nif file and other files
      # we first check if they are cached in the cache dir
      if !File.dir?(unarchive_dest_dir) do
        # if `unarchive_dest_dir` does not exists
        # then perhaps we need to extract these files from the precompiled binary tarball
        #
        # to extract files from the tarball
        # we check if the tarball is cached in the cache dir
        {needs_download, needs_unarchive} =
          if File.exists?(cache_file) do
            # and it has to be a regular file
            if File.regular?(cache_file) do
              # of course we have to compute its checksum
              {:ok, algo, cache_file_checksum} = checksum(cache_file)
              Logger.info("Precompiled binary tarball cached at #{cache_file}, #{algo}=#{cache_file_checksum}")

              # and verify the checksum in the map
              {checksum_matched?, algo_in_map, checksum_in_map} = verify_checksum(cache_file, algo, cache_file_checksum)
              if checksum_matched? do
                # if these checksum matches
                # we can extract files from the tarball
                {false, true}
              else
                Logger.error("Checksum mismatched: #{cache_file}[#{algo}=#{cache_file_checksum}], expected:[#{algo_in_map}=#{checksum_in_map}]")
                Logger.warning("Will delete cached tarball #{cache_file} and re-download")
                # otherwise we delete the cached file and download it again
                {true, true}
              end
            else
              # not a regular file
              # remove it and download again
              File.rm_rf!(cache_file)
              Logger.warning("Cached file at #{cache_file} is not a regular file, will delete it and re-download")
              {true, true}
            end
          else
            {true, true}
          end

        if needs_download do
          download_url = get_download_url(target, version)

          {:ok, _} = Application.ensure_all_started(:inets)
          {:ok, _} = Application.ensure_all_started(:ssl)

          # of course we have to verify its checksum too
          {:ok, algo, checksum} = download!(download_url, cache_file, true)
          Logger.info("Precompiled binary tarball downloaded and saved to #{cache_file}, #{algo}=#{checksum}")

          {checksum_matched?, algo_in_map, checksum_in_map} = verify_checksum(cache_file, algo, checksum)
          if !checksum_matched? do
            msg = "Checksum mismatched: downloaded file: #{cache_file}[#{algo}=#{checksum}], expected:[#{algo_in_map}=#{checksum_in_map}]"
            Logger.error(msg)
            raise RuntimeError, msg
          end
        end

        if needs_unarchive do
          File.rm_rf!(unarchive_dest_dir)
          unarchive!(cache_file, cache_dir)
        end
      end
      # if `unarchive_dest_dir` exists and is a directory
      # then we can simply copy they to the expected locations

      deploy_from_dir!(cached_priv_dir, cached_elixir_dir)
    else
      :ok
    end
  end

  def verify_checksum(cache_file, algo, cache_file_checksum) do
    checksum_map = read_checksum_map()
    basename = Path.basename(cache_file)

    case Map.fetch(checksum_map, basename) do
      {:ok, algo_with_hash} ->
        [algo_in_map, hash] = String.split(algo_with_hash, ":")
        {cache_file_checksum == hash and algo_in_map == algo, algo_in_map, hash}

      :error ->
        {:error,
         "the precompiled NIF file does not exist in the checksum file. " <>
           "Please consider run: `EVISION_FETCH_PRECOMPILED=true mix evision.fetch --all` to generate the checksum file."}
    end
  end

  def deploy_from_dir!(cached_priv_dir, cached_elixir_dir) do
    app_priv = app_priv()
    generated_elixir_dir = Path.join([File.cwd!, "lib", "generated"])
    if File.exists?(app_priv) do
      File.rm_rf!(app_priv)
    end
    if File.exists?(generated_elixir_dir) do
      File.rm_rf!(generated_elixir_dir)
    end

    Logger.info("Copying priv directory: #{cached_priv_dir} => #{app_priv}")
    with {"", 0} <- System.cmd("cp", ["-a", cached_priv_dir, app_priv]) do
      :ok
    else
      {msg, code} ->
        msg = "Failed to copy priv directory, `cp` exited with code #{code}: #{inspect(msg)}"
        Logger.error(msg)
        raise RuntimeError, msg
    end

    Logger.info("Copying generated Elixir binding files: #{cached_elixir_dir} => #{generated_elixir_dir}")
    with {"", 0} <- System.cmd("cp", ["-a", cached_elixir_dir, generated_elixir_dir]) do
      :ok
    else
      {msg, code} ->
        msg = "Failed to copy generated Elixir binding files, `cp` exited with code #{code}: #{inspect(msg)}"
        Logger.info(msg)
        raise RuntimeError, msg
    end
  end

  def unarchive!(filepath, to_directory) do
    File.mkdir_p!(to_directory)
    with :ok <- :erl_tar.extract(filepath, [:compressed, {:cwd, to_directory}]) do
      :ok
    else
      err ->
        msg = "Failed to unarchive tarball file: #{filepath}, error: #{inspect(err)}"
        Logger.error(msg)
        raise RuntimeError, msg
    end
  end

  def deploy_type(log? \\ false) do
    {target, [_arch, _os, abi]} = get_target()
    version = Metadata.last_released_version()
    if use_precompiled?(log?) and available_for_version?(version, log?) and available_for_target?(target, log?) do
      {:precompiled, abi}
    else
      {:build_from_source, abi}
    end
  end

  @impl true
  def run(_args) do
    with {:precompiled, _} <- deploy_type(true) do
      {target, [_arch, os, _abi]} = get_target()
      version = Metadata.last_released_version()
      prepare(target, os, version)
    else
      _ ->
        raise RuntimeError, "Cannot use precompiled binaries."
    end
  end
end

defmodule Evision.MixProject do
  use Mix.Project
  require Logger
  alias Evision.MixProject.Metadata
  alias Mix.Tasks.Compile.EvisionPrecompiled

  @source_url "#{Metadata.github_url()}/tree/v#{Metadata.last_released_version()}"

  def project do
    {compilers, make_env} =
      if System.get_env("EVISION_FETCH_PRECOMPILED") != "true" do
        {deploy_type, target_abi} = EvisionPrecompiled.deploy_type(false)
        if deploy_type == :build_from_source do
          {cmake_options, enabled_modules} = generate_cmake_options()
          make_env = %{
            "HAVE_NINJA" => "#{System.find_executable("ninja") != nil}",
            "OPENCV_VER" => opencv_versions(System.get_env("OPENCV_VER", Metadata.opencv_version())),
            "MAKE_BUILD_FLAGS" =>
              System.get_env("MAKE_BUILD_FLAGS", "-j#{System.schedulers_online()}"),
            "CMAKE_OPTIONS" => cmake_options,
            "ENABLED_CV_MODULES" => enabled_modules,
            "EVISION_PREFER_PRECOMPILED" => "false",
            "EVISION_PRECOMPILED_VERSION" => Metadata.last_released_version(),
            "TARGET_ABI" => System.get_env("TARGET_ABI", target_abi),
            "EVISION_GENERATE_LANG" => System.get_env("EVISION_GENERATE_LANG", "elixir"),
          }
          {[:elixir_make] ++ Mix.compilers(), make_env}
        else
          {[:evision_precompiled] ++ Mix.compilers(), %{}}
        end
    else
      {Mix.compilers(), %{}}
    end

    [
      app: Metadata.app(),
      name: "Evision",
      version: Metadata.version(),
      elixir: "~> 1.11",
      deps: deps(),
      docs: docs(),
      compilers: compilers,
      source_url: Metadata.github_url(),
      description: description(),
      package: package(),
      make_executable: make_executable(),
      make_makefile: make_makefile(),
      make_env: make_env
    ]
  end

  def make_executable() do
    case :os.type() do
      {:win32, _} -> "nmake"
      _ -> "make"
    end
  end

  def make_makefile() do
    case :os.type() do
      {:win32, _} -> "Makefile.win"
      _ -> "Makefile"
    end
  end

  def opencv_versions(version) do
    if Enum.member?(Metadata.compatible_opencv_versions(), version) do
      version
    else
      Logger.warning(
        "OpenCV version #{version} is not in the compatible list, you may encounter compile errors"
      )

      Logger.warning(
        "Compatible OpenCV versions: " <> (Metadata.compatible_opencv_versions() |> Enum.join(", "))
      )

      version
    end
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  @all_modules [
    :calib3d,
    :core,
    :features2d,
    :flann,
    :highgui,
    :imgcodecs,
    :imgproc,
    :ml,
    :photo,
    :stitching,
    :ts,
    :video,
    :videoio,
    :dnn,
    :gapi,
    :world,
    :python2,
    :python3
  ]

  @enabled_modules [
    :calib3d,
    :core,
    :features2d,
    :flann,
    :highgui,
    :imgcodecs,
    :imgproc,
    :ml,
    :photo,
    :stitching,
    :ts,
    :video,
    :videoio,
    :dnn
  ]

  @disabled_modules [
    # not supported yet
    :gapi,
    # no need for this
    :world,
    # no need for this
    :python2,
    # no need for this
    :python3,
    # no need for this
    :java
  ]

  @enabled_img_codecs [
    :png,
    :jpeg,
    :tiff,
    :webp,
    :openjpeg,
    :jasper,
    :openexr
  ]

  # To make things easier, you can set `compile_mode` to `only_enabled_modules` so
  # that only modules specified in `enabled_modules` will be compiled. Like-wise,
  # set `except_disabled_modules` to only exclude modules in `disabled_modules`.
  # By default, the value of `compile_mode` is `auto`, which means to leave unspecified
  # modules to CMake to decide.
  @compile_mode :auto

  defp generate_cmake_options() do
    enabled_modules = Application.get_env(:evision, :enabled_modules, @enabled_modules)
    disabled_modules = Application.get_env(:evision, :disabled_modules, @disabled_modules)
    enabled_img_codecs = Application.get_env(:evision, :enabled_img_codecs, @enabled_img_codecs)
    compile_mode = Application.get_env(:evision, :compile_mode, @compile_mode)

    {cmake_options, enabled_modules} =
      case compile_mode do
        :auto ->
          cmake_options =
            (enabled_modules
             |> Enum.map(&"-D BUILD_opencv_#{Atom.to_string(&1)}=ON")
             |> Enum.join(" ")) <>
              " " <>
              (disabled_modules
               |> Enum.map(&"-D BUILD_opencv_#{&1}=OFF")
               |> Enum.join(" "))

          {cmake_options, enabled_modules}

        :only_enabled_modules ->
          cmake_options =
            "-D BUILD_LIST=" <>
              (enabled_modules
               |> Enum.map(&Atom.to_string(&1))
               |> Enum.join(","))

          {cmake_options, enabled_modules}

        :except_disabled_modules ->
          enabled_modules = @all_modules -- disabled_modules

          cmake_options =
            "-D BUILD_LIST=" <>
              (enabled_modules
               |> Enum.map(&Atom.to_string(&1))
               |> Enum.join(","))

          {cmake_options, enabled_modules}

        unrecognised_mode ->
          Mix.raise("unrecognised compile_mode for evision: #{inspect(unrecognised_mode)}")
      end

    options =
      cmake_options <>
        " " <>
        (enabled_img_codecs
         |> Enum.map(&"-D BUILD_#{Atom.to_string(&1) |> String.upcase()}=ON")
         |> Enum.join(" ")) <>
        " "

    {options, enabled_modules |> Enum.map(&Atom.to_string(&1)) |> Enum.join(",")}
  end

  defp deps do
    [
      # compilation
      {:elixir_make, "~> 0.6", runtime: false},
      # runtime
      {:dll_loader_helper, "~> 0.1"},
      {:nx, "~> 0.3"},
      # optional
      {:kino, "~> 0.7", optional: true},
      # docs
      {:ex_doc, "~> 0.28", only: :docs, runtime: false},
      # test
      {:scidata, "~> 0.1", only: :test},
      {:castore, "~> 0.1", only: :test, override: true}
    ]
  end

  defp docs do
    [
      main: "Evision",
      source_ref: "v#{Metadata.version()}",
      source_url: @source_url,
      before_closing_body_tag: &before_closing_body_tag/1,
      groups_for_functions: [
        cv: &(&1[:namespace] == :cv),
        "cv.Mat": &(&1[:namespace] == :"cv.Mat"),
        "cv.highgui": &(&1[:namespace] == :"cv.highgui"),
        external: &(&1[:namespace] == :external),
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
        Constants: &(&1[:type] == :constants)
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
    "OpenCV-Erlang/Elixir binding."
  end

  defp package() do
    [
      name: "evision",
      # These are the default files included in the package
      files:
        ~w(
          c_src/evision_custom_headers
          c_src/modules
          c_src/ArgInfo.hpp
          c_src/erlcompat.hpp
          c_src/evision.cpp
          c_src/evision_custom_headers.h
          c_src/nif_utils.hpp
          py_src/*.py
          scripts
          patches
          cc_toolchain
          Makefile
          Makefile.win
          CMakeLists.txt
          lib/*.ex
          lib/evision
          .formatter.exs
          mix.exs
          checksum-evision.exs
          src/evision_mat.erl
          src/evision_highgui.erl
          src/evision.app.src
          rebar.config
          README* LICENSE* CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => Metadata.github_url()}
    ]
  end
end
