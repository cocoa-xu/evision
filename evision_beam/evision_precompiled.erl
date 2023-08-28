-module(evision_precompiled).
-export([is_precompiled_binary_available/0, install_precompiled_binary_if_available/0]).
-import(checksum, [checksum/0]).

-define(PRECOMPILED_TARBALL_NAME, "evision-nif_~s-~s-~s").
-define(PRECOMPILED_DOWNLOAD_URL, "https://github.com/cocoa-xu/evision/releases/download/v~s/~s").
-define(EVISION_SO_FILE, "priv/evision.so").
-define(EVISION_DLL_FILE, "priv/evision.dll").
-define(ERLANG_GENERATED_SOURCE_DIR, "src/generated").

-include_lib("kernel/include/file.hrl").

app_version() ->
    {ok, Cwd} = file:get_cwd(),
    Src = filename:join([Cwd, "src", "evision.app.src"]),
    case file:read_file(Src) of
        {ok, BinContent} ->
            Content = binary_to_list(BinContent),
            case erl_scan:string(Content) of
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of
                        {ok, {application, evision, App}} ->
                            case proplists:get_value(vsn, App) of
                                undefined ->
                                    "unknown";
                                Version ->
                                    case is_list(Version) of
                                        true ->
                                            Version;
                                        false ->
                                            "unknown"
                                    end
                            end;
                        {error, _} -> 
                            "unknown"
                    end;
                {error, _} ->
                    "unknown"
            end;
        {error, _} ->
            "unknown"
    end.

is_dev() ->
    AppVersion = app_version(),
    case string:find(AppVersion, "-dev") of
        nomatch ->
            {false, AppVersion};
        _ ->
            {true, AppVersion}
    end.

only_darwin(DarwinABI) ->
    case string:prefix(DarwinABI, "darwin") of
        nomatch ->
            DarwinABI;
        _ ->
            "darwin"
    end.

maybe_override_by_env(EnvName, Default) ->
    case os:getenv(EnvName) of
        false ->
            Default;
        Value ->
            Value
    end.

get_target() ->
    TargetParts = string:split(erlang:system_info(system_architecture), "-", all),
    [G_ARCH, G_OS, G_ABI] = case length(TargetParts) of
        4 ->
            [T_ARCH, _, T_OS, T_ABI] = TargetParts,
            [T_ARCH, T_OS, only_darwin(T_ABI)];
        3 ->
            [T_ARCH, T_OS, T_ABI] = TargetParts,
            [T_ARCH, T_OS, only_darwin(T_ABI)];
        1 ->
            case TargetParts of
                ["win32"] ->
                    WIN_ARCH = case maybe_override_by_env("PROCESSOR_ARCHITECTURE", "x86_64") of
                        "ARM64" ->
                            "aarch64";
                        PROCESSOR_ARCHITECTURE ->
                            PROCESSOR_ARCHITECTURE
                    end,
                    [WIN_ARCH, "windows", "msvc"];
                _ ->
                    ["unknown", "unknown", "unknown"]
            end
    end,
    ARCH = maybe_override_by_env("TARGET_ARCH", G_ARCH),
    OS = maybe_override_by_env("TARGET_OS", G_OS),
    ABI = maybe_override_by_env("TARGET_ABI", G_ABI),
    TRIPLET = io_lib:fwrite("~s-~s-~s", [ARCH, OS, ABI]),
    TRIPLET.

get_nif_version() ->
    erlang:system_info(nif_version).

get_expected_checksum(TarballFilename) ->
    Map = checksum:checksum(),
    case maps:is_key(TarballFilename, Map) of
        true ->
            AlgoAndChecksum = maps:get(TarballFilename, Map),
            case string:split(AlgoAndChecksum, ":") of
                [Algo, Checksum] ->
                    {Algo, Checksum};
                _ ->
                    false
            end;
        false ->
            false
    end.

is_precompiled_binary_available() ->
    case is_dev() of
        {true, _} ->
            false;
        {false, AppVersion} ->
            Target = get_target(),
            NifVersion = get_nif_version(),
            Name = lists:flatten(io_lib:fwrite(?PRECOMPILED_TARBALL_NAME, [NifVersion, Target, AppVersion])),
            TarballFilename = lists:flatten(io_lib:fwrite("~s.tar.gz", [Name])),
            case get_expected_checksum(TarballFilename) of
                false ->
                    {error, lists:flatten(io_lib:fwrite("Cannot find checksum for ~s~n", [Name]))};
                {Algo, Checksum} ->
                    TarballURL = lists:flatten(io_lib:fwrite(?PRECOMPILED_DOWNLOAD_URL, [AppVersion, TarballFilename])),
                    {true, Name, TarballFilename, TarballURL, Algo, Checksum}
            end
    end.

cache_opts() ->
    case os:getenv("MIX_XDG") of
        false ->
            #{};
        _ ->
            #{os => linux}
    end.

%% code from https://erlangforums.com/t/base-16-in-erlang/1468/3
base16_loop(Bin) when is_binary(Bin) ->
    base16_loop(Bin, <<>>).

base16_loop(<<N:8/unit:4,Rest/bitstring>>, Acc) ->
    Hex = integer_to_binary(N, 16),
    PadLen = 8 - byte_size(Hex),
    base16_loop(Rest, <<Acc/binary,
                        <<"00000000">>:PadLen/binary,
                        (integer_to_binary(N, 16))/binary>>);
base16_loop(<<N:1/unit:4,Rest/bitstring>>, Acc) ->
    base16_loop(Rest, <<Acc/binary, (integer_to_binary(N, 16))/binary>>);
base16_loop(<<>>, Acc) ->
    Acc.

compute_checksum(ChecksumAlgo, Filepath) ->
    case file:read_file(Filepath) of
        {ok, BinContent} ->
            Hash = crypto:hash(list_to_atom(ChecksumAlgo), BinContent),
            Checksum = string:to_lower(binary_to_list(base16_loop(Hash))),
            {Filepath, ChecksumAlgo, Checksum};
        _ ->
            {Filepath, nil, nil}
    end.

verify_cached_file(GetChecksumIfExists, ChecksumAlgo, CacheTo) ->
    case GetChecksumIfExists and filelib:is_regular(CacheTo) of
        true ->
            compute_checksum(ChecksumAlgo, CacheTo);
        false ->
            {CacheTo, nil, nil}
    end.

get_cached_path(Filename) ->
    CacheBaseDir = filename:basedir(user_cache, "", cache_opts()),
    CacheDir = maybe_override_by_env("ELIXIR_MAKE_CACHE_DIR", CacheBaseDir),
    file:make_dir(CacheDir),
    {CacheDir, filename:join([CacheDir, Filename])}.

cache_path(Filename, ChecksumAlgo, GetChecksumIfExists) ->
    {CacheDir, FullPath} = get_cached_path(Filename),
    case filelib:is_dir(CacheDir) of
        true ->
            verify_cached_file(GetChecksumIfExists, ChecksumAlgo, FullPath);
        false ->
            {FullPath, nil, nil}
    end.

download_precompiled_binary(URL, CacheTo, ChecksumAlgo) ->
    case do_download(URL) of
        {ok, Body} ->
            file:write_file(CacheTo, Body),
            {FullPath, _, Checksum} = verify_cached_file(true, ChecksumAlgo, CacheTo),
            io:fwrite("[INFO] Precompiled binary tarball downloaded and saved to ~s, ~s=~s~n", [FullPath, ChecksumAlgo, Checksum]),
            {FullPath, ChecksumAlgo, Checksum};
        {error, DownloadError} ->
            io:fwrite("[ERROR] Cannot download precompiled binary from ~p: ~p~n", [URL, DownloadError]),
            {CacheTo, nil, nil}
    end.

download(URL, CacheFilename, ChecksumAlgo, ExpectedChecksum, Overwrite) ->
    {CacheTo, Algo, CachedFileChecksum} = cache_path(CacheFilename, ChecksumAlgo, not Overwrite),
    case CachedFileChecksum of
        nil ->
            io:fwrite("[INFO] not downloaded, will download!~n"),
            download_precompiled_binary(URL, CacheTo, ChecksumAlgo);
        _ -> 
            case ExpectedChecksum == CachedFileChecksum of
                true ->
                    io:fwrite("[INFO] Precompiled binary tarball cached at ~s, checksum[~p]=~p\r\n", [CacheTo, Algo, CachedFileChecksum]),
                    {CacheTo, Algo, CachedFileChecksum};
                false ->
                    io:fwrite("[ERROR] Checksum mismatched ~s[~s=~s], expected:[~p=~p]\r\n", [CacheTo, Algo, CachedFileChecksum, Algo, ExpectedChecksum]),
                    io:fwrite("[WARNING] Will delete cached tarball ~s and re-download", [CacheTo]),
                    file:delete(CacheTo),
                    download_precompiled_binary(URL, CacheTo, ChecksumAlgo)
            end
    end.

certificate_store() ->
    PossibleLocations = [
        %% Configured cacertfile
        os:getenv("ELIXIR_MAKE_CACERT"),

        %% Debian/Ubuntu/Gentoo etc.
        "/etc/ssl/certs/ca-certificates.crt",

        %% Fedora/RHEL 6
        "/etc/pki/tls/certs/ca-bundle.crt",

        %5 OpenSUSE
        "/etc/ssl/ca-bundle.pem",

        %% OpenELEC
        "/etc/pki/tls/cacert.pem",

        %% CentOS/RHEL 7
        "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem",

        %% Open SSL on MacOS
        "/usr/local/etc/openssl/cert.pem",

        %% MacOS & Alpine Linux
        "/etc/ssl/cert.pem"
    ],
    CheckExistance = lists:map(fun (F) -> 
        {filelib:is_file(F), F}
    end, PossibleLocations),
    ExistingOnes = lists:dropwhile(fun ({X, _}) -> X == false end, CheckExistance),
    case length(ExistingOnes) of
        Len when Len > 0 ->
            {_, Cert} = hd(ExistingOnes),
            Cert;
        _ ->
            io:fwrite("[WARNING] Cannot find CA certificate store in default locations: ~p~n", [PossibleLocations]),
            io:fwrite("You can set environment variable ELIXIR_MAKE_CACERT to the SSL cert on your system.~n"),
            nil
    end.

preferred_ciphers() ->
    PreferredCiphers = [
      %% Cipher suites (TLS 1.3): TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256
      #{cipher => aes_128_gcm, key_exchange => any, mac => aead, prf => sha256},
      #{cipher => aes_256_gcm, key_exchange => any, mac => aead, prf => sha384},
      #{cipher => chacha20_poly1305, key_exchange => any, mac => aead, prf => sha256},
      %% Cipher suites (TLS 1.2): ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:
      %% ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:
      %% ECDHE-RSA-CHACHA20-POLY1305:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384
      #{cipher => aes_128_gcm, key_exchange => ecdhe_ecdsa, mac => aead, prf => sha256},
      #{cipher => aes_128_gcm, key_exchange => ecdhe_rsa, mac => aead, prf => sha256},
      #{cipher => aes_256_gcm, key_exchange => ecdh_ecdsa, mac => aead, prf => sha384},
      #{cipher => aes_256_gcm, key_exchange => ecdh_rsa, mac => aead, prf => sha384},
      #{cipher => chacha20_poly1305, key_exchange => ecdhe_ecdsa, mac => aead, prf => sha256},
      #{cipher => chacha20_poly1305, key_exchange => ecdhe_rsa, mac => aead, prf => sha256},
      #{cipher => aes_128_gcm, key_exchange => dhe_rsa, mac => aead, prf => sha256},
      #{cipher => aes_256_gcm, key_exchange => dhe_rsa, mac => aead, prf => sha384}
    ],
    ssl:filter_cipher_suites(PreferredCiphers, []).

protocol_versions() ->
    case list_to_integer(erlang:system_info(otp_release)) of
        Version when Version < 25 ->
            ['tlsv1.2'];
        _ ->
            ['tlsv1.2', 'tlsv1.3']
    end.

preferred_eccs() ->
    %% TLS curves: X25519, prime256v1, secp384r1
    PreferredECCS = [secp256r1, secp384r1],
    ssl:eccs() -- ssl:eccs() -- PreferredECCS.

secure_ssl() ->
    case os:getenv("ELIXIR_MAKE_UNSAFE_HTTPS") of
        nil -> true;
        "FALSE" -> false;
        "false" -> false;
        "nil" -> false;
        "NIL" -> false;
        _ -> true
    end.

https_opts(Hostname) ->
    CertFile = certificate_store(),
    case {secure_ssl(), is_list(CertFile)} of
        {true, true} ->
            [
                {
                    ssl, [
                        {verify, verify_peer},
                        {cacertfile, CertFile},
                        {depth, 4},
                        {ciphers, preferred_ciphers()},
                        {versions, protocol_versions()},
                        {eccs, preferred_eccs()},
                        {reuse_sessions, true},
                        {server_name_indication, Hostname},
                        {secure_renegotiate, true},
                        {customize_hostname_check, [
                            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                        ]}
                    ]
                }
            ];
        _ ->
            [
                {
                    ssl, [
                        {verify, verify_none},
                        {ciphers, preferred_ciphers()},
                        {versions, protocol_versions()},
                        {reuse_sessions, true},
                        {server_name_indication, Hostname},
                        {secure_renegotiate, true}
                    ]
                }
            ]
    end.

do_download(URL) ->
    application:ensure_started(inets),
    ssl:start(),
    HttpOtps = https_opts("github.com"),
    Request = {URL, []},
    case httpc:request(get, Request, HttpOtps, [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
          {ok, Body};
        {error, Reason} ->
          {error, Reason};
        Err ->
            {error, lists:flatten(io_lib:fwrite("Cannot download file from ~s: ~p~n", [URL, Err]))}
    end.

is_already_installed() ->
    case string:find(get_target(), "windows-msvc") of
        nomatch ->
            filelib:is_regular(?EVISION_SO_FILE);
        _ ->
            filelib:is_regular(?EVISION_DLL_FILE)
    end.

install_precompiled_binary_if_available() ->
    case is_already_installed() of
        false ->
            case is_precompiled_binary_available() of
                {true, Name, TarballFilename, TarballURL, ExpectedAlgo, ExpectedChecksum} ->
                    case download(TarballURL, TarballFilename, ExpectedAlgo, ExpectedChecksum, false) of
                        {_, _, nil} ->
                            exit(failed);
                        {TarballFileFullPath, _, ExpectedChecksum} ->
                            file:del_dir_r("tmp_priv"),
                            Status = 
                                case erl_tar:extract(TarballFileFullPath, [compressed, {cwd, "tmp_priv"}]) of
                                    ok ->
                                        file:del_dir_r(?ERLANG_GENERATED_SOURCE_DIR),
                                        file:del_dir_r("priv"),
                                        TmpSrc = filename:join(["tmp_priv", Name, "erlang_generated"]),
                                        TmpPriv = filename:join(["tmp_priv", Name, "priv"]),
                                        io:fwrite("[INFO] Moving generated Erlang binding files: ~p => ~p~n", [TmpSrc, ?ERLANG_GENERATED_SOURCE_DIR]),
                                        SrcRenameOk = file:rename(TmpSrc, ?ERLANG_GENERATED_SOURCE_DIR),
                                        case SrcRenameOk of
                                            {error, SrcError} ->
                                                io:fwrite("[ERROR] Failed to move generated Erlang binding files: ~p~n", [SrcError]);
                                            _ ->
                                                ok
                                        end,
                                        io:fwrite("[INFO] Moving priv directory: ~p => ~p~n", [TmpSrc, "priv"]),
                                        PrivRenameOk = file:rename(TmpPriv, "priv"),
                                        case PrivRenameOk of
                                            {error, PrivError} ->
                                                io:fwrite("[ERROR] Failed to move priv directory: ~p~n", [PrivError]);
                                            _ ->
                                                ok
                                        end,
                                        case {SrcRenameOk, PrivRenameOk} of
                                            {ok, ok} ->
                                                ok;
                                            _ ->
                                                failed
                                        end;
                                    Error ->
                                        io:fwrite("[ERROR] Failed to unarchive tarball file: ~s, error: ~p~n", [TarballFileFullPath, Error]),
                                        failed
                                end,
                            file:del_dir_r("tmp_priv"),
                            case Status of 
                                failed ->
                                    exit(failed);
                                _ ->
                                    ok
                            end
                    end;
                What ->
                    io:fwrite("[INFO] Cannot find precompiled binary: ~p~n", [What]),
                    exit(failed)
            end;
        true ->
            ok
    end.
