defmodule Evision.MixProject.Metadata do
  @moduledoc false

  def app, do: :evision
  def version, do: "0.2.9"
  def github_url, do: "https://github.com/cocoa-xu/evision"
  def opencv_version, do: "4.10.0"
  # only means compatible. need to write more tests
  def compatible_opencv_versions,
    do: ["4.5.3", "4.5.4", "4.5.5", "4.6.0", "4.7.0", "4.8.0", "4.9.0", "4.10.0"]

  def default_cuda_version, do: {"12", "9"}
  def all_cuda_version(_), do: [{"11", "8"}, {"11", "9"}, {"12", "8"}, {"12", "9"}]
end

defmodule Mix.Tasks.Compile.EvisionPrecompiled do
  @moduledoc false

  use Mix.Task
  require Logger
  alias Evision.MixProject.Metadata

  @available_targets [
    "aarch64-apple-darwin",
    "x86_64-apple-darwin",
    "aarch64-linux-gnu",
    "aarch64-linux-musl",
    "x86_64-linux-gnu",
    "x86_64-linux-musl",
    "armv7l-linux-gnueabihf",
    "armv6-linux-gnueabihf",
    "ppc64le-linux-gnu",
    "s390x-linux-gnu",
    "riscv64-linux-gnu",
    "riscv64-linux-musl",
    "i686-linux-gnu",
    "aarch64-windows-msvc",
    "x86_64-windows-msvc",
    "aarch64-apple-darwin-ios"
  ]

  @compile_nif_version "2.16"

  def get_available_nif_versions do
    ["2.16", "2.17"]
  end

  def available_nif_urls(_host_nif_version, version \\ Metadata.version()) do
    nif_version = get_compile_nif_version()

    Enum.reduce(@available_targets, [], fn target, acc ->
      no_contrib = get_download_url(target, version, nif_version, false, false, "", "")
      with_contrib = get_download_url(target, version, nif_version, true, false, "", "")

      with_cuda =
        if target in ["x86_64-linux-gnu", "aarch64-linux-gnu", "x86_64-windows-msvc"] do
          Enum.map(Metadata.all_cuda_version(target), fn {cuda_ver, cudnn_version} ->
            get_download_url(target, version, nif_version, true, true, cuda_ver, cudnn_version)
          end)
        else
          []
        end

      [no_contrib, with_contrib] ++ with_cuda ++ acc
    end)
  end

  def current_target_nif_url(_host_nif_version, version \\ Metadata.version()) do
    {target, _} = get_target()
    nif_version = get_compile_nif_version()
    enable_contrib = System.get_env("EVISION_ENABLE_CONTRIB", "true") == "true"

    if enable_contrib do
      System.put_env("EVISION_ENABLE_CONTRIB", "true")
    else
      System.put_env("EVISION_ENABLE_CONTRIB", "false")
    end

    enable_cuda = System.get_env("EVISION_ENABLE_CUDA", "false") == "true"

    {cuda_version, cudnn_version} =
      if enable_cuda do
        {defalt_cuda, default_cudnn} = Metadata.default_cuda_version()
        System.put_env("EVISION_ENABLE_CUDA", "true")

        {
          System.get_env("EVISION_CUDA_VERSION", defalt_cuda),
          System.get_env("EVISION_CUDNN_VERSION", default_cudnn)
        }
      else
        System.put_env("EVISION_ENABLE_CUDA", "false")
        {"", ""}
      end

    get_download_url(
      target,
      version,
      nif_version,
      enable_contrib,
      enable_cuda,
      cuda_version,
      cudnn_version
    )
  end

  def checksum_file(app \\ Mix.Project.config()[:app]) when is_atom(app) do
    # Saves the file in the project root.
    {
      Path.join(File.cwd!(), "checksum.exs"),
      Path.join([File.cwd!(), "checksum.erl"])
    }
  end

  defp read_checksum_map(app \\ Mix.Project.config()[:app]) when is_atom(app) do
    {file, _} = checksum_file(app)

    with {:ok, contents} <- File.read(file),
         {%{} = contents, _} <- Code.eval_string(contents) do
      contents
    else
      _ -> %{}
    end
  end

  def get_compile_nif_version do
    @compile_nif_version
  end

  def get_nif_version do
    to_string(:erlang.system_info(:nif_version))
  end

  def get_target do
    case System.get_env("MIX_TARGET") do
      "ios" ->
        {"aarch64-apple-darwin-ios", ["aarch64", "apple", "darwin"]}

      "xros" ->
        {"aarch64-apple-darwin-xros", ["aarch64", "apple", "darwin"]}

      _ ->
        target = String.split(to_string(:erlang.system_info(:system_architecture)), "-")

        [arch, os, abi] =
          case Enum.count(target) do
            3 ->
              target

            4 ->
              [arch, _vendor, os, abi] = target
              [arch, os, abi]

            1 ->
              with ["win32"] <- target do
                ["x86_64", "windows", "msvc"]
              else
                [unknown_target] ->
                  [unknown_target, "unknown", nil]
              end
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

        arch =
          if os == "windows" do
            case String.downcase(System.get_env("PROCESSOR_ARCHITECTURE")) do
              "arm64" ->
                "aarch64"

              arch when arch in ["x64", "x86_64", "amd64"] ->
                "x86_64"

              arch ->
                arch
            end
          else
            arch
          end

        abi = System.get_env("TARGET_ABI", abi)
        os = System.get_env("TARGET_OS", os)

        arch =
          if String.match?(System.get_env("TARGET_CPU", ""), ~r/arm11[357]6/) do
            "armv6"
          else
            System.get_env("TARGET_ARCH", arch)
          end

        {Enum.join([arch, os, abi], "-"), [arch, os, abi]}
    end
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
        Logger.info(
          "EVISION_PREFER_PRECOMPILED: true; try to download and use the precompiled library."
        )
      else
        Logger.info(
          "EVISION_PREFER_PRECOMPILED: #{prefer_precompiled}; will not use precompiled binaries."
        )
      end
    end

    use_precompiled?
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

  def available_for_nif_version?(nif_version, log? \\ false) do
    host_nif_version =
      case Enum.count(String.split(nif_version, ".")) do
        2 ->
          Version.parse!("#{nif_version}.0")

        3 ->
          Version.parse!(nif_version)

        _ ->
          raise RuntimeError, "unknown nif_version: #{nif_version}"
      end

    compile_nif_version = get_compile_nif_version()

    available_for_nif_version? =
      Version.compare(Version.parse!("#{compile_nif_version}.0"), host_nif_version) != :gt

    if log? do
      if available_for_nif_version? do
        Logger.info(
          "Current host NIF version is `#{nif_version}`, will use precompiled binaries with NIF version #{compile_nif_version}."
        )
      else
        Logger.warning(
          "Current host NIF version `#{nif_version}` does not have precompiled binaries."
        )
      end
    end

    available_for_nif_version?
  end

  # https_opts and related code are taken from
  # https://github.com/elixir-cldr/cldr_utils/blob/master/lib/cldr/http/http.ex
  @certificate_locations [
                           # Configured cacertfile
                           System.get_env("ELIXIR_MAKE_CACERT"),

                           # A little hack to use cacerts.pem in CAStore/certfi
                           ## when `:evision` is a dependency in other application
                           ## => Mix.ProjectStack.project_file()
                           ## when `:evision` is the top application
                           ## => __ENV__.file
                           Path.join([
                             Path.dirname(Mix.ProjectStack.project_file() || __ENV__.file),
                             "deps/castore/priv/cacerts.pem"
                           ]),
                           Path.join([
                             Path.dirname(Mix.ProjectStack.project_file() || __ENV__.file),
                             "deps/certfi/priv/cacerts.pem"
                           ]),

                           # Debian/Ubuntu/Gentoo etc.
                           "/etc/ssl/certs/ca-certificates.crt",

                           # Fedora/RHEL 6
                           "/etc/pki/tls/certs/ca-bundle.crt",

                           # OpenSUSE
                           "/etc/ssl/ca-bundle.pem",

                           # OpenELEC
                           "/etc/pki/tls/cacert.pem",

                           # CentOS/RHEL 7
                           "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem",

                           # Open SSL on MacOS
                           "/usr/local/etc/openssl/cert.pem",

                           # MacOS & Alpine Linux
                           "/etc/ssl/cert.pem"
                         ]
                         |> Enum.reject(&is_nil/1)

  @doc false
  def certificate_store do
    @certificate_locations
    |> Enum.find(&File.exists?/1)
    |> warning_if_no_cacertfile!
    |> :erlang.binary_to_list()
  end

  defp warning_if_no_cacertfile!(nil) do
    Logger.warning("""
    No certificate trust store was found.
    Tried looking for: #{inspect(@certificate_locations)}
    A certificate trust store is required in
    order to download locales for your configuration.
    Since elixir_make could not detect a system
    installed certificate trust store one of the
    following actions may be taken:
    1. Install the hex package `castore`. It will
      be automatically detected after recompilation.

    2. Install the hex package `certifi`. It will
      be automatically detected after recomilation.

    3. Specify the location of a certificate trust store
       by configuring it in environment variable:

        export ELIXIR_MAKE_CACERT="/path/to/cacerts.pem"
    """)

    ""
  end

  defp warning_if_no_cacertfile!(file) do
    file
  end

  defp https_opts(hostname) do
    cert_file = certificate_store()

    if secure_ssl?() and cert_file != [] do
      [
        ssl: [
          verify: :verify_peer,
          cacertfile: certificate_store(),
          depth: 4,
          ciphers: preferred_ciphers(),
          versions: protocol_versions(),
          eccs: preferred_eccs(),
          reuse_sessions: true,
          server_name_indication: hostname,
          secure_renegotiate: true,
          customize_hostname_check: [
            match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
          ]
        ]
      ]
    else
      [
        ssl: [
          verify: :verify_none,
          server_name_indication: hostname,
          secure_renegotiate: true,
          reuse_sessions: true,
          versions: protocol_versions(),
          ciphers: preferred_ciphers()
        ]
      ]
    end
  end

  def preferred_ciphers do
    preferred_ciphers = [
      # Cipher suites (TLS 1.3): TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256
      %{cipher: :aes_128_gcm, key_exchange: :any, mac: :aead, prf: :sha256},
      %{cipher: :aes_256_gcm, key_exchange: :any, mac: :aead, prf: :sha384},
      %{cipher: :chacha20_poly1305, key_exchange: :any, mac: :aead, prf: :sha256},
      # Cipher suites (TLS 1.2): ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:
      # ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:
      # ECDHE-RSA-CHACHA20-POLY1305:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384
      %{cipher: :aes_128_gcm, key_exchange: :ecdhe_ecdsa, mac: :aead, prf: :sha256},
      %{cipher: :aes_128_gcm, key_exchange: :ecdhe_rsa, mac: :aead, prf: :sha256},
      %{cipher: :aes_256_gcm, key_exchange: :ecdh_ecdsa, mac: :aead, prf: :sha384},
      %{cipher: :aes_256_gcm, key_exchange: :ecdh_rsa, mac: :aead, prf: :sha384},
      %{cipher: :chacha20_poly1305, key_exchange: :ecdhe_ecdsa, mac: :aead, prf: :sha256},
      %{cipher: :chacha20_poly1305, key_exchange: :ecdhe_rsa, mac: :aead, prf: :sha256},
      %{cipher: :aes_128_gcm, key_exchange: :dhe_rsa, mac: :aead, prf: :sha256},
      %{cipher: :aes_256_gcm, key_exchange: :dhe_rsa, mac: :aead, prf: :sha384}
    ]

    :ssl.filter_cipher_suites(preferred_ciphers, [])
  end

  def protocol_versions do
    if otp_version() < 25 do
      [:"tlsv1.2"]
    else
      [:"tlsv1.2", :"tlsv1.3"]
    end
  end

  def otp_version do
    :erlang.system_info(:otp_release) |> List.to_integer()
  end

  def preferred_eccs do
    # TLS curves: X25519, prime256v1, secp384r1
    preferred_eccs = [:secp256r1, :secp384r1]
    :ssl.eccs() -- :ssl.eccs() -- preferred_eccs
  end

  def secure_ssl? do
    case System.get_env("ELIXIR_MAKE_UNSAFE_HTTPS") do
      nil -> true
      "FALSE" -> false
      "false" -> false
      "nil" -> false
      "NIL" -> false
      _other -> true
    end
  end

  def download!(url, save_as, overwrite \\ false)

  def download!(url, save_as, false) do
    unless File.exists?(save_as) do
      download!(url, save_as, true)
    end

    checksum(save_as)
  end

  def download!(url, save_as, true) do
    with {:ok, body} <- download!(url) do
      File.write!(save_as, body)
      checksum(save_as)
    end
  end

  def download!(url) do
    opts = [body_format: :binary]
    arg = {url, []}

    # https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/inets
    # TODO: This may no longer be necessary from Erlang/OTP 26.0 or later.
    http_options = https_opts(String.to_charlist(URI.parse(url).host))

    if proxy = System.get_env("HTTP_PROXY") || System.get_env("http_proxy") do
      Mix.shell().info("Using HTTP_PROXY: #{proxy}")
      %{host: host, port: port} = URI.parse(proxy)

      :httpc.set_options([{:proxy, {{String.to_charlist(host), port}, []}}])
    end

    if proxy = System.get_env("HTTPS_PROXY") || System.get_env("https_proxy") do
      Mix.shell().info("Using HTTPS_PROXY: #{proxy}")
      %{host: host, port: port} = URI.parse(proxy)
      :httpc.set_options([{:https_proxy, {{String.to_charlist(host), port}, []}}])
    end

    Mix.shell().info("Downloading precompiled tarball from: #{url}")

    case :httpc.request(:get, arg, http_options, opts) do
      {:ok, {{_, 200, _}, _, body}} ->
        {:ok, body}

      {:error, reason} ->
        raise RuntimeError, "Cannot download file from #{url}: #{reason}."
    end
  end

  def filename(
        target,
        version,
        nif_version,
        enable_contrib,
        enable_cuda,
        cuda_version,
        cudnn_version,
        with_ext \\ ""
      )

  def filename(
        target,
        version,
        nif_version,
        _enable_contrib = false,
        _enable_cuda,
        _cuda_version,
        _cudnn_version,
        with_ext
      ) do
    "evision-nif_#{nif_version}-#{target}-#{version}#{with_ext}"
  end

  def filename(
        target,
        version,
        nif_version,
        _enable_contrib = true,
        _enable_cuda = false,
        _cuda_version,
        _cudnn_version,
        with_ext
      ) do
    "evision-nif_#{nif_version}-#{target}-contrib-#{version}#{with_ext}"
  end

  def filename(
        target,
        version,
        nif_version,
        _enable_contrib = true,
        _enable_cuda = true,
        cuda_version,
        cudnn_version,
        with_ext
      ) do
    "evision-nif_#{nif_version}-#{target}-contrib-cuda#{cuda_version}-cudnn#{cudnn_version}-#{version}#{with_ext}"
  end

  def get_download_url(
        target,
        version,
        nif_version,
        enable_contrib,
        enable_cuda,
        cuda_version,
        cudnn_version
      ) do
    tar_file =
      filename(
        target,
        version,
        nif_version,
        enable_contrib,
        enable_cuda,
        cuda_version,
        cudnn_version,
        ".tar.gz"
      )

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
  def checksum_algo, do: @checksum_algo

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

  def prepare(
        target,
        os,
        version,
        nif_version,
        enable_contrib,
        enable_cuda,
        cuda_version,
        cudnn_version
      ) do
    name =
      filename(
        target,
        version,
        nif_version,
        enable_contrib,
        enable_cuda,
        cuda_version,
        cudnn_version
      )

    filename =
      filename(
        target,
        version,
        nif_version,
        enable_contrib,
        enable_cuda,
        cuda_version,
        cudnn_version,
        ".tar.gz"
      )

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

    windows_fix_so_file =
      if os == "windows" do
        "windows_fix.dll"
      else
        "windows_fix.so"
      end

    evision_so_file = Path.join([app_priv(), evision_so_file])
    windows_fix_so_file = Path.join([app_priv(), windows_fix_so_file])

    # first we check if we already have the NIF file
    # if not, then we defintely need to copy it (and other files) from somewhere
    needs_copy = !File.exists?(evision_so_file) or !File.exists?(windows_fix_so_file)

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

              Logger.info(
                "Precompiled binary tarball cached at #{cache_file}, #{algo}=#{cache_file_checksum}"
              )

              # and verify the checksum in the map
              {checksum_matched?, algo_in_map, checksum_in_map} =
                verify_checksum(cache_file, algo, cache_file_checksum)

              if checksum_matched? do
                # if these checksum matches
                # we can extract files from the tarball
                {false, true}
              else
                Logger.error(
                  "Checksum mismatched: #{cache_file}[#{algo}=#{cache_file_checksum}], expected:[#{algo_in_map}=#{checksum_in_map}]"
                )

                Logger.warning("Will delete cached tarball #{cache_file} and re-download")
                # otherwise we delete the cached file and download it again
                {true, true}
              end
            else
              # not a regular file
              # remove it and download again
              File.rm_rf!(cache_file)

              Logger.warning(
                "Cached file at #{cache_file} is not a regular file, will delete it and re-download"
              )

              {true, true}
            end
          else
            {true, true}
          end

        if needs_download do
          download_url =
            get_download_url(
              target,
              version,
              nif_version,
              enable_contrib,
              enable_cuda,
              cuda_version,
              cudnn_version
            )

          {:ok, _} = Application.ensure_all_started(:inets)
          {:ok, _} = Application.ensure_all_started(:ssl)

          # of course we have to verify its checksum too
          {:ok, algo, checksum} = download!(download_url, cache_file, true)

          Logger.info(
            "Precompiled binary tarball downloaded and saved to #{cache_file}, #{algo}=#{checksum}"
          )

          {checksum_matched?, algo_in_map, checksum_in_map} =
            verify_checksum(cache_file, algo, checksum)

          if !checksum_matched? do
            msg =
              "Checksum mismatched: downloaded file: #{cache_file}[#{algo}=#{checksum}], expected:[#{algo_in_map}=#{checksum_in_map}]"

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
    generated_elixir_dir = Path.join([File.cwd!(), "lib", "generated"])

    if File.exists?(app_priv) do
      File.rm_rf!(app_priv)
    end

    if File.exists?(generated_elixir_dir) do
      File.rm_rf!(generated_elixir_dir)
    end

    Logger.info("Copying priv directory: #{cached_priv_dir} => #{app_priv}")

    with {:ok, _} <- File.cp_r(cached_priv_dir, app_priv) do
      :ok
    else
      error ->
        msg = "Failed to copy priv directory, `File.cp_r`: #{inspect(error)}"
        Logger.error(msg)
        raise RuntimeError, msg
    end

    Logger.info(
      "Copying generated Elixir binding files: #{cached_elixir_dir} => #{generated_elixir_dir}"
    )

    with {:ok, _} <- File.cp_r(cached_elixir_dir, generated_elixir_dir) do
      :ok
    else
      error ->
        msg = "Failed to copy generated Elixir binding files, `File.cp_r`: #{inspect(error)}"
        Logger.error(msg)
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
    {:ok, %Version{pre: pre}} = Version.parse(Metadata.version())
    nif_version = get_nif_version()

    if use_precompiled?(log?) and pre != ["dev"] and
         available_for_target?(target, log?) and available_for_nif_version?(nif_version, log?) do
      {:precompiled, abi}
    else
      {:build_from_source, abi}
    end
  end

  @impl true
  def run(_args) do
    {target, [_arch, os, _abi]} = get_target()

    evision_so_file =
      if os == "windows" do
        "evision.dll"
      else
        "evision.so"
      end

    windows_fix_so_file =
      if os == "windows" do
        "windows_fix.dll"
      else
        "windows_fix.so"
      end

    evision_so_file = Path.join([app_priv(), evision_so_file])
    windows_fix_so_file = Path.join([app_priv(), windows_fix_so_file])

    if !File.exists?(evision_so_file) or !File.exists?(windows_fix_so_file) do
      with {:precompiled, _} <- deploy_type(true) do
        version = Metadata.version()
        nif_version = get_compile_nif_version()
        enable_contrib = System.get_env("EVISION_ENABLE_CONTRIB", "true") == "true"
        enable_cuda = System.get_env("EVISION_ENABLE_CUDA", "false") == "true"
        {default_cuda_version, default_cudnn_version} = Metadata.default_cuda_version()
        cuda_version = System.get_env("EVISION_CUDA_VERSION", default_cuda_version)
        cudnn_version = System.get_env("EVISION_CUDNN_VERSION", default_cudnn_version)

        prepare(
          target,
          os,
          version,
          nif_version,
          enable_contrib,
          enable_cuda,
          cuda_version,
          cudnn_version
        )
      else
        _ ->
          raise RuntimeError, "Cannot use precompiled binaries."
      end
    else
      :ok
    end
  end
end

defmodule Evision.MixProject do
  use Mix.Project
  require Logger
  alias Evision.MixProject.Metadata
  alias Mix.Tasks.Compile.EvisionPrecompiled

  @source_url "#{Metadata.github_url()}/tree/v#{Metadata.version()}"

  def project do
    {compilers, make_env} =
      if System.get_env("EVISION_FETCH_PRECOMPILED") != "true" do
        {deploy_type, target_abi} = EvisionPrecompiled.deploy_type(false)

        if deploy_type == :build_from_source do
          {cmake_options, enabled_modules} = generate_cmake_options()

          make_env = %{
            "HAVE_NINJA" => "#{System.find_executable("ninja") != nil}",
            "OPENCV_VER" =>
              opencv_versions(System.get_env("OPENCV_VER", Metadata.opencv_version())),
            "MAKE_BUILD_FLAGS" =>
              System.get_env("MAKE_BUILD_FLAGS", "-j#{System.schedulers_online()}"),
            "CMAKE_OPTIONS" => cmake_options,
            "ENABLED_CV_MODULES" => enabled_modules,
            "TARGET_ABI" => System.get_env("TARGET_ABI", target_abi),
            "EVISION_GENERATE_LANG" => System.get_env("EVISION_GENERATE_LANG", "elixir"),
            "EVISION_PREFER_PRECOMPILED" => "false"
          }

          make_env =
            case :os.type() do
              {:win32, _} ->
                Map.put_new(
                  make_env,
                  "PYTHON3_EXECUTABLE",
                  System.find_executable("python3") || System.find_executable("python")
                )

              _ ->
                make_env
            end

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
      aliases: aliases(),
      elixirc_paths: elixirc_paths(),
      erlc_paths: erlc_paths(),
      compilers: compilers,
      source_url: Metadata.github_url(),
      description: description(),
      package: package(),
      make_executable: make_executable(),
      make_makefile: make_makefile(),
      make_env: make_env,
      xref: [
        exclude: [
          :wx,
          :wxBitmap,
          :wxBoxSizer,
          :wxDC,
          :wxFrame,
          :wxImage,
          :wxMemoryDC,
          :wxPanel,
          :wxSizer,
          :wxStaticBoxSizer,
          :wxWindowDC,
          :wx_object
        ]
      ]
    ]
  end

  defp aliases do
    [
      compile_opencv: &compile_opencv/1
    ]
  end

  defp compile_opencv(_) do
    config = Mix.Project.config()
    Mix.shell().print_app()
    priv? = File.dir?("priv")
    Mix.Project.ensure_structure()
    config = Keyword.put_new(config, :make_targets, ["opencv"])

    compiler_file =
      Path.join([
        Path.dirname(Mix.ProjectStack.project_file() || __ENV__.file),
        "deps/elixir_make/lib/elixir_make/compiler.ex"
      ])

    with [{ElixirMake.Compiler, _}] <- Code.require_file(compiler_file) do
      ElixirMake.Compiler.make(config, [])
      # IF there was no priv before and now there is one, we assume
      # the user wants to copy it. If priv already existed and was
      # written to it, then it won't be copied if build_embedded is
      # set to true.
      if not priv? and File.dir?("priv") do
        Mix.Project.build_structure()
      end

      {:ok, []}
    else
      _ ->
        Mix.shell().error("Cannot compile OpenCV only: failed to require file #{compiler_file}")
    end
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
        "Compatible OpenCV versions: " <>
          (Metadata.compatible_opencv_versions() |> Enum.join(", "))
      )

      version
    end
  end

  def application do
    [
      extra_applications: [:logger, :inets, :ssl],
      mod: {Evision.Application, []}
    ]
  end

  @module_configuration %{
    # opencv/opencv_contrib
    opencv: [
      # module name: is_enabled
      # note that these true values here only mean that we requested
      # these module to be compiled
      # some of them can be disabled due to dependency issues
      calib3d: true,
      core: true,
      dnn: true,
      features2d: true,
      flann: true,
      highgui: true,
      imgcodecs: true,
      imgproc: true,
      ml: true,
      photo: true,
      stitching: true,
      ts: true,
      video: true,
      videoio: true,
      gapi: false,
      world: false,
      python2: false,
      python3: false,
      java: false
    ],
    opencv_contrib: [
      aruco: true,
      barcode: true,
      bgsegm: true,
      bioinspired: true,
      dnn_superres: true,
      face: true,
      hfs: true,
      img_hash: true,
      line_descriptor: true,
      mcc: true,
      plot: true,
      quality: true,
      rapid: true,
      reg: true,
      rgbd: true,
      saliency: true,
      shape: true,
      stereo: true,
      structured_light: true,
      surface_matching: true,
      text: true,
      tracking: true,
      wechat_qrcode:
        System.get_env("MIX_TARGET") != "ios" or System.get_env("MIX_TARGET") != "xros",
      xfeatures2d: true,
      ximgproc: true,
      xphoto: true,

      # no bindings yet
      datasets: false,
      dnn_objdetect: false,
      dpm: false,
      optflow: false,
      sfm: false,
      videostab: false,
      xobjdetect: false
    ],
    cuda: [
      cudaarithm: true,
      cudabgsegm: true,
      cudacodec: true,
      cudafeatures2d: true,
      cudafilters: true,
      cudaimgproc: true,
      cudalegacy: true,
      cudaobjdetect: true,
      cudaoptflow: true,
      cudastereo: true,
      cudawarping: true,
      cudev: true
    ]
  }
  defp module_configuration, do: @module_configuration

  @enabled_img_codecs (if System.get_env("MIX_TARGET") == "ios" or
                            System.get_env("MIX_TARGET") == "xros" do
                         []
                       else
                         [:png, :jpeg, :tiff, :webp, :openjpeg, :jasper, :openexr]
                       end)

  # To make things easier, you can set `compile_mode` to `only_enabled_modules` so
  # that only modules specified in `enabled_modules` will be compiled. Like-wise,
  # set `except_disabled_modules` to only exclude modules in `disabled_modules`.
  # By default, the value of `compile_mode` is `auto`, which means to leave unspecified
  # modules to CMake to decide.
  @compile_mode :auto

  defp generate_cmake_options() do
    mc = module_configuration()

    enable_cuda = System.get_env("EVISION_ENABLE_CUDA", "false")
    enable_opencv_cuda = enable_cuda == "true"

    if enable_opencv_cuda do
      System.put_env("EVISION_ENABLE_CUDA", "true")
    else
      System.put_env("EVISION_ENABLE_CUDA", "false")
    end

    enable_contrib = System.get_env("EVISION_ENABLE_CONTRIB", "true")
    enable_opencv_contrib = enable_contrib == "true"

    if enable_opencv_cuda and !enable_opencv_contrib do
      Logger.warning(
        "EVISION_ENABLE_CUDA is set to true, while EVISION_ENABLE_CONTRIB is set to false. CUDA support will NOT be available."
      )
    end

    if enable_opencv_contrib do
      System.put_env("EVISION_ENABLE_CONTRIB", "true")
    else
      System.put_env("EVISION_ENABLE_CONTRIB", "false")
    end

    all_modules =
      Enum.map(mc.opencv, fn {m, _} -> m end) ++ Enum.map(mc.opencv_contrib, fn {m, _} -> m end)

    enabled_modules =
      Enum.filter(mc.opencv, fn {_, e} -> e end) ++
        if enable_opencv_contrib do
          Enum.filter(mc.opencv_contrib, fn {_, e} -> e end)
        else
          []
        end ++
        if enable_opencv_cuda do
          Enum.filter(mc.cuda, fn {_, e} -> e end)
        else
          []
        end

    disabled_modules =
      Enum.filter(mc.opencv, fn {_, e} -> !e end) ++
        if enable_opencv_contrib do
          Enum.filter(mc.opencv_contrib, fn {_, e} -> !e end)
        else
          []
        end

    enabled_modules = Keyword.keys(enabled_modules)
    disabled_modules = Keyword.keys(disabled_modules)
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
          enabled_modules = all_modules -- disabled_modules

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

    options =
      if enable_opencv_cuda and enable_opencv_contrib do
        "#{options} -D WITH_CUDA=ON"
      else
        options
      end

    {options, enabled_modules |> Enum.map(&Atom.to_string(&1)) |> Enum.join(",")}
  end

  defp deps do
    [
      # compilation
      {:elixir_make, "~> 0.7", runtime: false},
      {:castore, "~> 0.1 or ~> 1.0"},

      # runtime
      {:nx, "~> 0.6"},

      # optional
      ## kino: for smart cells and better output in livebook
      {:kino, "~> 0.11", optional: true},
      ## progress_bar: for the model zoo smart cell
      {:progress_bar, "~> 2.0 or ~> 3.0", optional: true},

      # docs
      {:ex_doc, "~> 0.34.0", only: :docs, runtime: false},

      # test
      {:scidata, "~> 0.1", only: :test}
    ]
  end

  defp docs do
    [
      main: "Evision",
      source_ref: "v#{Metadata.version()}",
      source_url: @source_url,
      extras: [
        "CHANGELOG.md",
        "Cheatsheet.cheatmd",
        "README.md",
        # Evision
        "examples/getting_started.livemd",
        "examples/qrcode.livemd",
        "examples/warp_perspective.livemd",
        "examples/warp_polar.livemd",
        "examples/find_and_draw_contours.livemd",
        "examples/stitching.livemd",
        "examples/pca.livemd",
        "examples/photo-hdr.livemd",
        "examples/sudoku.livemd",
        "examples/cifar10.livemd",
        # Evision.DNN
        "examples/dnn-googlenet.livemd",
        "examples/dnn-detection-model.livemd",
        "examples/densenet121_benchmark.livemd",
        # Evision.ML
        "examples/ml-svm.livemd",
        "examples/ml-decision_tree_and_random_forest.livemd"
      ],
      before_closing_body_tag: &before_closing_body_tag/1,
      groups_for_docs: [
        external: &(&1[:namespace] == :external),
        Enumerator: &(&1[:enum] == true)
      ]
    ]
  end

  defp before_closing_body_tag(:html) do
    ~S'
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
    });
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
                distcoeffs: ["(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6 [, s_1, s_2, s_3, s_4[, \\tau_x, \\tau_y]]]]) \\text{ of 4, 5, 8, 12 or 14 elements}"],
                distcoeffsfisheye: ["(k_1, k_2, k_3, k_4)"],
                hdotsfor: ["\\dots", 1],
                mathbbm: ["\\mathbb{#1}", 1],
                bordermatrix: ["\\matrix{#1}", 1]
            }
        }
    }
    );
    //]]>
    </script>
    <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js"></script>
    '
  end

  defp before_closing_body_tag(_), do: ""

  defp description() do
    "OpenCV-Erlang/Elixir binding."
  end

  def links do
    %{
      "GitHub" => Metadata.github_url(),
      "Readme" => "#{Metadata.github_url()}/blob/v#{Metadata.version()}/README.md",
      "Changelog" => "#{Metadata.github_url()}/blob/v#{Metadata.version()}/CHANGELOG.md"
    }
  end

  defp elixirc_paths, do: ["lib"]
  defp erlc_paths, do: []

  defp package() do
    [
      name: "evision",
      # These are the default files included in the package
      files: ~w(
          c_src/evision_custom_headers
          c_src/modules
          c_src/windows_fix
          c_src/ArgInfo.hpp
          c_src/erlcompat.hpp
          c_src/evision_consts.h
          c_src/evision_custom_headers.h
          c_src/evision.cpp
          c_src/nif_utils.hpp
          cc_toolchain
          lib/assets
          lib/evision
          lib/mix
          lib/smartcell
          lib/zoo
          lib/*.ex
          patches
          py_src/*.py
          scripts
          src/evision_mat.erl
          src/evision_highgui.erl
          src/evision_internal_structurise.erl
          src/evision.app.src
          checksum.exs
          checksum.erl
          CMakeLists.txt
          evision_precompiled.erl
          Makefile
          Makefile.win
          .formatter.exs
          mix.exs
          rebar.config
          README*
          LICENSE*
          CHANGELOG*),
      licenses: ["Apache-2.0"],
      links: links()
    ]
  end
end
