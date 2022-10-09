# evision [WIP]

| OS               | Arch           | ABI  | Build Status | Has Precompiled Library |
|------------------|----------------|------|--------------|-------------------------|
| Linux            | x86_64         | gnu  |[![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-x86_64.yml) | Yes |
| Linux            | x86_64         | musl |[![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml) | Yes |
| Linux            | arm64          | gnu  |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | arm64          | musl |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml) | Yes |
| Linux            | armv7l (armhf) | gnueabihf |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | ppc64le        | gnu  |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | s390x          | gnu  |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | riscv64        | gnu  |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-gnu.yml) | Yes |
| Linux            | riscv64        | musl |[![linux-precompile](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile-musl.yml) | Yes |
| macOS 11 Big Sur | x86_64         | darwin |[![CI](https://github.com/cocoa-xu/evision/actions/workflows/macos-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/macos-x86_64.yml) | Yes |
| macOS 11 Big Sur | arm64          | darwin |[![macos-precompile](https://github.com/cocoa-xu/evision/actions/workflows/macos-precompile.yml/badge.svg?branch=v0.1.1)](https://github.com/cocoa-xu/evision/actions/workflows/macos-precompile.yml) | Yes |
| Windows 2019     | x86_64         | msvc |[![CI](https://github.com/cocoa-xu/evision/actions/workflows/windows-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/windows-x86_64.yml) | Yes |

## Docs
Online docs for the latest released version is available on Hex.pm, [https://hexdocs.pm/evision/](https://hexdocs.pm/evision/).

Online docs for git HEAD is available here, [https://cocoa-xu.github.io/evision/](https://cocoa-xu.github.io/evision/).

## Nerves Support

[![Nerves](https://github-actions.40ants.com/cocoa-xu/evision/matrix.svg?only=nerves-build)](https://github.com/cocoa-xu/evision)

Prebuilt firmwares are available [here](https://github.com/cocoa-xu/evision/actions/workflows/nerves-build.yml?query=is%3Asuccess). 
Select the most recent run and scroll down to the `Artifacts` section, download the firmware file for your board and run

```bash
fwup /path/to/the/downloaded/firmware.fw
```

In the nerves build, `evision` is integrated as one of the dependencies of the [nerves_livebook](https://github.com/livebook-dev/nerves_livebook)
project. This means that you can use livebook (as well as other pre-pulled libraries) to explore and evaluate the `evision`
project. 

The default password of the livebook is `nerves` (as the time of writing, if it does not work, please check the nerves_livebook project). 

## Integration with Nx

`Evision.Nx` module converts `Evision.Mat` to `Nx.tensor`:

```elixir
iex> {:ok, mat} = Evision.imread("/path/to/image.png")
# Or you can use the !(bang) version, but if the image cannot be read by OpenCV for whatever reason
# the bang version will raise a RuntimeError exception
iex> mat = Evision.imread!("/path/to/image.png")
%Evision.Mat{
  channels: 3,
  dims: 2,
  type: {:u, 8},
  raw_type: 16,
  shape: {512, 512, 3},
  ref: #Reference<0.2992585850.4173463580.172624>
}

iex> t = Evision.Nx.to_nx(mat)
#Nx.Tensor<
  u8[512][512][3]
  Evision.Backend
  [
    [
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, 255],
      [255, 255, ...],
      ...
    ],
    ...
  ]
>
```

and vice-versa:

```elixir
iex> mat = Evision.imread!("/path/to/image.png")
iex> t = Evision.Nx.to_nx(mat)
# convert a tensor to a mat
iex> mat_from_tensor = Evision.Nx.to_mat!(t)
%Evision.Mat{
  channels: 1,
  dims: 3,
  type: {:u, 8},
  raw_type: 0,
  shape: {512, 512, 3},
  ref: #Reference<0.1086574232.1510342676.18186>
}

# Note that `Evision.Nx.to_mat` gives a tensor
# however, some OpenCV functions expect the mat
# to be a "valid 2D image"
# therefore, in such cases `Evision.Nx.to_mat_2d`
# should be used instead
#
# Noticing the changes in `channels`, `dims` and `raw_type`
iex> mat_from_tensor = Evision.Nx.to_mat_2d!(t)
%Evision.Mat{
  channels: 3,
  dims: 2,
  type: {:u, 8},
  raw_type: 16,
  shape: {512, 512, 3},
  ref: #Reference<0.1086574232.1510342676.18187>
}

# and it works for tensors with any shapes
iex> t = Nx.iota({2, 3, 2, 3, 2, 3}, type: :s32)
iex> mat = Evision.Nx.to_mat!(t)
%Evision.Mat{
  channels: 1,
  dims: 6,
  type: {:s, 32},
  raw_type: 4,
  shape: {2, 3, 2, 3, 2, 3},
  ref: #Reference<0.1086574232.1510342676.18188>
}
```

#### Unsupported Type Map
As OpenCV does not support the following types (yet, as of OpenCV 4.6.0)

- `{:s, 64}`
- `{:u, 32}`
- `{:u, 64}`

Although it's possible to *store* values with those types using custom types, the resulting Mat/tensor will be incompatible with most existing functions in OpenCV.

Moreover, it's somewhat inconvinient to explicitly specify the type each time using them. Therefore, Evision allows to set a map for those unsupported types. 

```elixir
config :evision, unsupported_type_map: %{
  {:s, 64} => {:f, 64},
  {:u, 64} => {:f, 64},
  {:u, 32} => {:f, 32}
}
```

The `key` of this `unsupported_type_map` is the unsupported type, and the value is the replacement type for it.

See [this reply](https://github.com/cocoa-xu/evision/issues/48#issuecomment-1266282345) for more details on this.

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

by compatible, it means these versions can compile successfully, and I tested a small range of functions. Tons of tests
should be written, and then we can have a list for tested OpenCV versions.

## Installation

In order to use `evision`, you will need Elixir installed. Then create an Elixir project via the `mix` build tool:

```sh
$ mix new my_app
```

Then you can add `evision` as dependency in your `mix.exs`.

```elixir
def deps do
  [
    {:evision, "~> 0.1.9"}
  ]
end
```

Please note that although `:evision` is available on hex.pm now, it's still in its early versions. And it will remain on v0.1.x for a while, and all v0.1.x versions should be treated as in very active development. Please read the `CHANGELOG.md` for all breaking changes even it's a "minor" update.

Therefore, it's recommended to use a specific version (i.e., include the minor version number in `deps`, `{:evision, "~> 0.1.8"}`, instead of `{:evision, "~> 0.1"}`) at the moment.

### Use Precompiled Library (Default)
The following environment variables can be set based on your needs.

(Note that precompiled binaries do not use FFmpeg. If you'd like to use FFmpeg, please compile from source (please see instructions in the next section) and set corresponding environment variables. We're considering this option at the moment.)

#### Important notes
When using `:evision` from git, version "0.1.1" to "0.1.7" are available.

Starting from `0.1.8` (included) and onwards, using `:evision` from git is no longer supported (unless set `EVISION_PREFER_PRECOMPILED` to `false`) because we started to use checksum file to verify the integrity of the downloaded tarball file, and the checksum file is only tracked in the hex.pm package.

```elixir
def deps do
  [
    {:evision, "~> 0.1.7", github: "cocoa-xu/evision", tag: "v0.1.7"}
  ]
end
```

It is recommended to use `:evision` from hex.pm. Currently "0.1.7" to "0.1.9" are available on hex.pm,
```elixir
def deps do
  [
    {:evision, "~> 0.1.9"}
  ]
end
```

#### TARGET_ABI
```shell
# required if and only if the build target is using musl libc.
#
# (for nerves project, this environment variable is set by nerves)
export TARGET_ABI=musl
## (for armv7l which uses hard-float ABI (armhf))
export TARGET_ABI=musleabihf
```

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

If you found the precompiled binaries does not suit your needs (e.g., perhaps you need OpenCV to be compiled with FFmpeg to handle more video formats.), it's possible to override the behaviour by setting the environment variable `EVISION_PREFER_PRECOMPILED` to `false`

```shell
export EVISION_PREFER_PRECOMPILED=false
```

For livebook users, 
```elixir
Mix.install([
  {:evision, "~> 0.1.9"}
], system_env: [
  {"EVISION_PREFER_PRECOMPILED", "false"}
])
```

#### EVISION_PRECOMPILED_CACHE_DIR
```shell
# optional.
## set the cache directory for the precompiled archive file
export EVISION_PRECOMPILED_CACHE_DIR="$(pwd)/.cache"
```

### Compile evision from source
#### Dependencies

- Python3 (Only during the compilation, to generate binding files)

  Tested Python verisons (on `ubuntu:20.04`, see [workflow file](https://github.com/cocoa-xu/evision/blob/main/.github/workflows/test-python-compatibility.yml)):
  - 3.6.15
  - 3.7.12
  - 3.8.12
  - 3.9.9
  - 3.10.1
- [CMake](https://cmake.org/) >= 3.3 (for this project)

  The minimal version required by OpenCV can vary between versions.

  OpenCV 4.5.5 requires at least CMake 3.5.1.
- Erlang development library/headers. Tested on OTP/25.

#### Build from source
To obtain and compile OpenCV's source code from official releases, the following environment variables can affect the build

```shell
# optional
## set OpenCV version
##   the corresponding license file should be available at https://github.com/opencv/opencv/blob/${OPENCV_VER}/LICENSE
export OPENCV_VER="4.6.0"

# optional
## Use Debug build
export CMAKE_BUILD_TYPE=Debug

# optional
## enable FFmpeg
##   this will allow cmake to auto-detect FFmpeg libraries installed on the host
##   on Windows, OpenCV will download prebuilt FFmpeg libraries
##   for more information, please visit https://github.com/opencv/opencv/tree/4.x/3rdparty/ffmpeg
export CMAKE_OPENCV_OPTIONS="-D WITH_FFMPEG=ON"
## or disable FFmpeg
export CMAKE_OPENCV_OPTIONS="-D WITH_FFMPEG=OFF"
```

Note 1: OpenCV can encode and decode some video formats (varies depending on your system). FFmpeg can be used to encode/decode more video formats. 

However, you should be aware of the license of the FFmpeg components you selected as they could be licensed by LGPL/GPL or other licenses.

Note 2: FFmpeg 5 is not supported by OpenCV yet (as of OpenCV 4.6.0).

```shell
sudo apt install -y libavcodec-dev libavformat-dev libavutil-dev libswscale-dev libavresample-dev ffmpeg
```

on macOS
```shell
brew install ffmpeg@4
brew link ffmpeg@4
```

##### Extra notes for building from source on Windows
Evision on Windows uses `nmake` to handle the `Makefile.win` at the moment. And we also need `powershell` for now. `nmake`
should be included in Microsoft Visual Studio, and `powershell` should be included in almost all recent versions (it was 
first released in 2007) of Windows.

If `ninja` can be found in `%PATH%`, then we will prefer using `ninja` to build everything as it allows parallel building.

Evision is NOT tested in MSYS2, Cygwin, or WSL/WSL2. 

#### Using source from a git repo (Optional)
It's also possible to obtain and compile OpenCV's source code from a custom git repo by setting the following environment variables (in addition to the ones above)

```shell
# required if and only if you prefer to compile OpenCV from a git repo
# set OPENCV_USE_GIT_HEAD to true to compile OpenCV from a git repo
# default value is false
export OPENCV_USE_GIT_HEAD=true

# required if and only if you prefer to compile OpenCV from a git repo
# this env var indicates which branch you prefer to use
# no default value
export OPENCV_USE_GIT_BRANCH=4.x

# optional.
# set this env var to specify which git repo to use
# default value is https://github.com/opencv/opencv.git, which is the offical git repo of OpenCV
export OPENCV_GIT_REPO="https://github.com/opencv/opencv.git"
```

When `OPENCV_USE_GIT_HEAD` is set to `true`, the following command will be used to fetch OpenCV's source code in `Makefile`:
```shell
git clone --branch=${OPENCV_USE_GIT_BRANCH} --depth=1 ${OPENCV_GIT_REPO} "${OPENCV_DIR}"
```

#### More Configuration (Optional)

`evision` will compile a subset of OpenCV functionality. You can configure the enabled modules in your `config` files:

```elixir
config :evision, enabled_modules: [
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
```

If a module is not specified in `:enabled_modules`, it may still be compiled if all requirements are present in your machine.
You can enforce only the `:enabled_modules` to be compiled by changing the compilation mode:

```elixir
config :evision, :compile_mode, :only_enabled_modules
```

You can also configure the list of image codecs used:

```elixir
config :evision, enabled_img_codecs: [
  :png,
  :jpeg,
  :tiff,
  :webp,
  :openjpeg,
  :jasper,
  :openexr
]
```

#### Notes

- How do I use my own OpenCV source code on my local disk?

    ```shell
    # To skip the download process, you can put the source zip file at `3rd_party/cache/opencv-${OPENCV_VER}.zip`.
    # Or you could supply OpenCV source code at `3rd_party/opencv/opencv-${OPENCV_VER}`.
    #
    # `3rd_party/opencv/opencv-${OPENCV_VER}` is the default value for `OPENCV_DIR`
    export OPENCV_DIR=/path/to/your/opencv/source/root
    ```

- How do I use my own OpenCV source code on my git repo?

    ```shell
    # use branch
    export OPENCV_USE_GIT_BRANCH="branch_name"
    export OPENCV_GIT_REPO="https://github.com/username/opencv.git"
  
    # use HEAD
    export OPENCV_USE_GIT_HEAD=true
    export OPENCV_GIT_REPO="https://github.com/username/opencv.git"
    ```

- How do I set the number of jobs for compiling?

    ```shell
    # use all logical cores, by default
    # `"-j#{System.schedulers_online()}"`. In `mix.exs`.
    export MAKE_BUILD_FLAGS="-j$(nproc)"
    
    # use 2 cores
    export MAKE_BUILD_FLAGS="-j2"
    ```

- How do I set up for cross-compile or specify the toolchain?

    ```shell
    export CMAKE_TOOLCHAIN_FILE="/path/to/toolchain.cmake"
    ```

- How do I make my own adjustments to the OpenCV CMake options?

    ```shell
    export CMAKE_OPENCV_OPTIONS="YOUR CMAKE OPTIONS FOR OPENCV"
    ```

- How do I generate binding code for erlang and Elixir at the same time?
    Yes, but currently it's only possible to do so when compiling evision using `mix`.

    ```shell
    # default value is `elixir` when compiling evision using `mix`
    # default value is `erlang` when compiling evision using `rebar`
    #
    # expected format is a comma-separated string
    export EVISION_GENERATE_LANG="erlang,elixir"
    ```

- Which ones of OpenCV options are supposed to be specified in `config/config.exs`?
  1. Enabled and disabled OpenCV modules
  2. Image codecs (if you enabled related OpenCV modules).

#### Debug related

Say you have the following MIX environment variables:

```shell
# set by MIX
MIX_ENV=dev
# set by evision or you
OPENCV_VER=4.6.0
# set by yourself if you're compiling evision to a nerves firmware
MIX_TARGET=rpi4
```

- How do I delete OpenCV related CMake build caches?

    ```shell
    # delete OpenCV related CMake build caches.
    rm -rf "_build/${MIX_ENV}/lib/evision/cmake_opencv_${OPENCV_VER}"
    ## for nerves
    rm -rf "_build/${MIX_TARGET}_${MIX_ENV}/lib/evision/cmake_opencv_${OPENCV_VER}"
    ```
  
- How do I remove downloaded OpenCV source zip file.

    ```shell
    rm -f "3rd_party/cache/opencv-${OPENCV_VER}"
    ```

- Can I manually edit the generated files and compile them?
  1. First, delete evision.so (so that `cmake` can rebuild it)  
  
      ```shell
      # 
      rm -f "_build/${MIX_ENV}/lib/evision/priv/evision.so"
      ## if you're building with nerves,
      ## use this path instead
      rm -rf "_build/${MIX_TARGET}_${MIX_ENV}/lib/evision/priv/evision.so"
      ```

  2. Secondly, comment out the following lines in the CMakeLists.txt  

    otherwise, your editing will be overwritten by the `py_src/gen2.py` (executing from the `CMakeLists.txt`)

    ```cmake
    if(WIN32)
        execute_process(COMMAND "rmdir ${GENERATED_ELIXIR_SRC_DIR} /s /q && rmdir ${GENERATED_ERLANG_SRC_DIR} /s /q  && mkdir ${GENERATED_ELIXIR_SRC_DIR} && mkdir ${GENERATED_ERLANG_SRC_DIR}")
        message("enabled modules: ${ENABLED_CV_MODULES}")
        execute_process(COMMAND python3.exe "${PY_SRC}\\gen2.py" "${C_SRC}" "${GENERATED_ELIXIR_SRC_DIR}" "${GENERATED_ERLANG_SRC_DIR}" "${C_SRC}\\headers.txt" "${EVISION_GENERATE_LANG}" "${ENABLED_CV_MODULES}" RESULT_VARIABLE STATUS)
    else()
        execute_process(COMMAND bash -c "rm -rf ${GENERATED_ELIXIR_SRC_DIR} && rm -rf ${GENERATED_ERLANG_SRC_DIR} && mkdir -p ${GENERATED_ELIXIR_SRC_DIR} && mkdir -p ${GENERATED_ERLANG_SRC_DIR}")
        message("enabled modules: ${ENABLED_CV_MODULES}")
        execute_process(COMMAND bash -c "python3 ${PY_SRC}/gen2.py ${C_SRC} ${GENERATED_ELIXIR_SRC_DIR} ${GENERATED_ERLANG_SRC_DIR} ${C_SRC}/headers.txt ${EVISION_GENERATE_LANG} ${ENABLED_CV_MODULES}" RESULT_VARIABLE STATUS)
    endif()
    if(STATUS STREQUAL "0")
        message(STATUS "Successfully generated binding code for: ${EVISION_GENERATE_LANG}")
    else()
        message(FATAL_ERROR "Failed to generate binding code for: ${EVISION_GENERATE_LANG}")
    endif()
    ```

  3. Then you can edit the source files and recompile `evision.so`.

    ```shell
    mix compile
    ```

#### Runtime related

- How do I enable debug logging for OpenCV (prints to stderr).

    ```shell
    export OPENCV_EVISION_DEBUG=1
    ```

### Acknowledgements
- `gen2.py`, `hdr_parser.py` and `c_src/erlcompat.hpp` were directly copied from the `python` module in the [OpenCV repo](https://github.com/opencv/opencv). Changes applied.
- `Makefile`, `CMakeLists.txt` and `c_src/nif_utils.hpp` were also copied from the `torchx` module in the [elixir-nx repo](https://github.com/elixir-nx/nx). Minor changes applied.
