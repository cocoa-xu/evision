<img src="https://github.com/cocoa-xu/evision/raw/main/assets/repository-open-graph.png" alt="Logo"/>

[![Hex.pm](https://img.shields.io/hexpm/v/evision.svg?style=flat&color=blue)](https://hex.pm/packages/evision)

| OS               | Arch           | ABI  | Build Status | Has Precompiled Library |
|------------------|----------------|------|--------------|-------------------------|
| Linux            | x86_64         | gnu  |[![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-x86_64.yml) | Yes |
| Linux            | x86_64         | musl |[![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml) | Yes |
| Linux            | aarch64        | gnu  |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | aarch64        | musl |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml) | Yes |
| Linux            | armv6 (armhf)  | gnueabihf |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | armv7l (armhf) | gnueabihf |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | ppc64le        | gnu  |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | s390x          | gnu  |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | riscv64        | gnu  |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | riscv64        | musl |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml) | Yes |
| macOS 12 Monterey | x86_64        | darwin |[![CI](https://github.com/cocoa-xu/evision/actions/workflows/macos-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/macos-x86_64.yml) | Yes |
| macOS 14 Sonoma  | aarch64       | darwin |[![macos-precompile](https://github.com/cocoa-xu/evision/actions/workflows/macos-precompile.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/macos-precompile.yml) | Yes |
| Windows 2022     | x86_64         | msvc |[![CI](https://github.com/cocoa-xu/evision/actions/workflows/windows-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/windows-x86_64.yml) | Yes |
| Windows 2022     | aarch64        | msvc |[![CI](https://github.com/cocoa-xu/evision/actions/workflows/windows-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/windows-precompile.yml) | Yes |

## Installation

In order to use `evision`, you will need Elixir installed. Then create an Elixir project via the `mix` build tool:

```sh
$ mix new my_app
```

Then you can add `evision` as a dependency in your `mix.exs`.

```elixir
def deps do
  [
    {:evision, "~> 0.2"}
  ]
end
```

### Use Precompiled Library (Default)
The following environment variables can be set based on your needs.

(Note that precompiled binaries do not use FFmpeg. If you'd like to use FFmpeg, please compile from source (please see instructions in the next section) and set corresponding environment variables. We're considering this option at the moment.)

<details>

<summary>Advanced Options</summary>

#### TARGET_ABI
**Required if and only if the target is using musl libc.**

```shell
# (for nerves project, this environment variable is set by nerves)
export TARGET_ABI=musl
## (for armv7l which uses hard-float ABI (armhf))
export TARGET_ABI=musleabihf
```

This variable will only be checked when identifying the musl libc ABI so that the correct precompiled binaries can be downloaded. Therefore, 

1. You don't need to keep it in the runtime environment.
2. If you want to change it later, the directory `_build/${MIX_ENV}/lib/evision` needs to be deleted first.

The default value for the `TARGET_ABI` env var is obtained using the following elixir code

```elixir
target_abi = List.last(String.split(to_string(:erlang.system_info(:system_architecture)), "-"))
target_abi =
  case target_abi do
    "darwin" <> _ -> "darwin"
    "win32" ->
      {compiler_id, _} = :erlang.system_info(:c_compiler_used)
      case compiler_id do
        :msc -> "msvc"
        _ -> to_string(compiler_id)
      end
    _ -> target_abi
  end
```

#### EVISION_PREFER_PRECOMPILED
```shell
# optional. 
# set this to "false" if you prefer :evision to be compiled from source
# 
# default value is "true", and :evision will prefer to use precompiled binaries (if available)
export EVISION_PREFER_PRECOMPILED=false
```

This variable will only be checked whenever the `mix compile` task is invoked directly (`mix compile`) or indirectly (`mix test`). And in the Makefile we would skip everything if `_build/${MIX_ENV}/lib/evision/priv/evision.so` is presented. Therefore,

1. You don't need to keep it in the runtime environment.
2. If you want to change it later, the directory `_build/${MIX_ENV}/lib/evision` needs to be deleted first.

**If you found the precompiled binaries do not suit your needs (e.g., perhaps you need OpenCV to be compiled with FFmpeg to handle more video formats.), it's possible to override the behaviour by setting the environment variable `EVISION_PREFER_PRECOMPILED` to `false`, and then please delete `_build/${MIX_ENV}/lib/evision` and recompile evision**

**Also, for Linux users only, the precompiled binary is not compiled with GTK support, therefore functions like `Evision.HighGui.imshow/2` will not work. However, you can either use `Evision.Wx.imshow/2` (if Erlang on your system is compiled with `wxWidgets`), or set the environment variable `EVISION_PREFER_PRECOMPILED` to `false` so that OpenCV can detect available HighGui backend when compiling from source.**

```shell
export EVISION_PREFER_PRECOMPILED=false
```

For livebook users, 
```elixir
Mix.install([
  {:evision, "~> 0.2"}
], system_env: [
  {"EVISION_PREFER_PRECOMPILED", "false"}
])
```

#### EVISION_ENABLE_CONTRIB
Set environment variable `EVISION_ENABLE_CONTRIB` to `true` to enable modules from [opencv_contrib](https://github.com/opencv/opencv_contrib).

```bash
# enable opencv_contrib modules (default)
export EVISION_ENABLE_CONTRIB=true

# disable opencv_contrib modules
export EVISION_ENABLE_CONTRIB=false
```

This variable will only be checked whenever the `mix compile` task is invoked directly (`mix compile`) or indirectly (`mix test`). And in the Makefile we would skip everything if `_build/${MIX_ENV}/lib/evision/priv/evision.so` is presented. Therefore,

1. You don't need to keep it in the runtime environment.
2. If you want to change it later from `false` to `true`, you can delete the file `_build/${MIX_ENV}/lib/evision/priv/evision.so`, set `EVISION_ENABLE_CONTRIB` to `true`, and then execute `mix compile`.

Defaults to `true` because for precompiled binaries, including these "extra" modules only increases less than 20 MBs (tested on `aarch64-apple-darwin`) in size.

However, 20 MBs for Nerves users can be a huge deal (still depending on your device, for example, +20 MBs is often much more acceptable for RPIs as they are usually equipped with >= 8 GB microSD cards while being absolutely a luxury thing for some other embedded devices).

#### EVISION_ENABLE_CUDA
Set environment variable `EVISION_ENABLE_CONTRIB` to `true` to enable CUDA support from [opencv_contrib](https://github.com/opencv/opencv_contrib). Defaults to `false`.

Note that `EVISION_ENABLE_CONTRIB` will need to be `true` as well.

```bash
# enable CUDA support
export EVISION_ENABLE_CUDA=true
## set a CUDA version that matches your local CUDA driver
## (this environment variable is only required for users who'd like to use precompiled binaries)
## available ones are 
## 11, for CUDA 11.x, built with CUDA 11.8.0
## 12, for CUDA 12.x, built with CUDA 12.5.0
export EVISION_CUDA_VERSION=11

## set a CUDNN version that matches your local CUDNN shared library
## (this environment variable is only required for users who'd like to use precompiled binaries)
## available ones are 
## 8, for CUDA 11.x or 12.x, built with CUDNN 8.9.7
## 9, for CUDA 11.x or 12.x, built with CUDNN 9.2.0
export EVISION_CUDNN_VERSION=9

## opencv_contrib modules is enabled by default
export EVISION_ENABLE_CONTRIB=true

# disable CUDA support (default) 
export EVISION_ENABLE_CUDA=false
```

##### IMPORTANT NOTE FOR WINDOWS USERS
If `EVISION_ENABLE_CUDA` is `true`, please also set CUDA runtime dir otherwise `Evision` will fail to load.

###### cmd
```cmd
set EVISION_CUDA_RUNTIME_DIR=C:/PATH/TO/YOUR/CUDA/RUNTIME/BIN
```

Also, please don't quote even if there are spaces in the path

```cmd
set EVISION_CUDA_RUNTIME_DIR=C:/PATH WITH SPACE/TO/YOUR/CUDA/RUNTIME/BIN
```

###### Powershell
```pwsh
$Env:EVISION_CUDA_RUNTIME_DIR="C:/PATH/TO/YOUR/CUDA/RUNTIME/BIN"
$Env:EVISION_CUDA_RUNTIME_DIR="C:/PATH WITH SPACE/TO/YOUR/CUDA/RUNTIME/BIN"
```

#### EVISION_PRECOMPILED_CACHE_DIR
```shell
# optional.
## set the cache directory for the precompiled archive file
export EVISION_PRECOMPILED_CACHE_DIR="$(pwd)/.cache"
```

</details>


## Examples

Some [examples](https://github.com/cocoa-xu/evision/tree/main/examples) are available in the `examples` directory.

## Description

`evision` will pull OpenCV source code from GitHub, then parse and automatically generate corresponding OpenCV-Elixir bindings.

This project uses and modifies `gen2.py` and `hdr_parser.py` from the `python` module in the [OpenCV repo](https://github.com/opencv/opencv) so that they output header files that can be used in Elixir bindings. 

We hope this project can largely reduce the work of manually porting OpenCV functions/modules to Elixir.

Compatible OpenCV versions:
- 4.5.3
- 4.5.4
- 4.5.5
- 4.6.0
- 4.7.0
- 4.8.0
- 4.9.0
- 4.10.0

by compatible, it means these versions can compile successfully, and I tested a small range of functions. Tons of tests
should be written, and then we can have a list for tested OpenCV versions.

## Docs
Online docs for the latest released version is available on Hex.pm, [https://hexdocs.pm/evision/](https://hexdocs.pm/evision/).

## Useful links
- [Installation](https://github.com/cocoa-xu/evision?tab=readme-ov-file#installation)
- [Use Precompiled Library (Default)](https://github.com/cocoa-xu/evision?tab=readme-ov-file#use-precompiled-library-default)
- [Compile evision from source](https://github.com/cocoa-xu/evision/wiki/Compile-evision-from-source)
- [Nerves Support](https://github.com/cocoa-xu/evision/wiki/Nerves-Support)
- [Register Builtin Smart Cells](https://github.com/cocoa-xu/evision/wiki/Register-Builtin-Smart-Cells)
- [Integration with Nx](https://github.com/cocoa-xu/evision/wiki/Integration-with-Nx)
- [Access behaviour (Getting a sub-area of an image)](https://github.com/cocoa-xu/evision/wiki/Access-behaviour-(Getting-a-sub%E2%80%90area-of-an-image))

### Acknowledgements
- `gen2.py`, `hdr_parser.py`, and `c_src/erlcompat.hpp` were directly copied from the `python` module in the [OpenCV repo](https://github.com/opencv/opencv). Changes applied.
- `Makefile`, `CMakeLists.txt`, and `c_src/nif_utils.hpp` were also copied from the `torchx` module in the [elixir-nx repo](https://github.com/elixir-nx/nx). Minor changes applied.
