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
Online docs is available here, [https://cocoa-xu.github.io/evision](https://cocoa-xu.github.io/evision/doc/Evision.html).

Will be available on hex.pm once we reach to the first release.

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

## Usage
In general, you can add `evision` to `deps` with the following settings.

```elixir
def deps do
  [
    {:evision, "~> 0.1", github: "cocoa-xu/evision", tag: "v0.1.6"}
  ]
end
```

Early versions (v0.1.x) of `evision` will be available on hex.pm soon.

### Use Precompiled Library
To use precompiled Evision library, the following environment variables should be set

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

```shell
# optional. 
# set this to "false" if you prefer :evision to be compiled from source
# 
# default value is "true", and :evision will prefer to use precompiled binaries (if available)
#   currently "0.1.1" to "0.1.6" are available
#   the version is implied by the tag in deps:
#     {:evision, "~> 0.1.6", github: "cocoa-xu/evision", tag: "v0.1.6"}
#   for other available versions, please check the GitHub release page
#   https://github.com/cocoa-xu/evision/releases
export EVISION_PREFER_PRECOMPILED=false

# optional.
## set the cache directory for the precompiled archive file
export EVISION_PRECOMPILED_CACHE_DIR="$(pwd)/.cache"
```

Note 1: Precompiled binaries does not use FFmpeg. If you'd like to use FFmpeg, please compile from source and set corresponding environment variables.

Note 2: by default, Evision will compile from source as it's WIP at the moment.

Note 3: a copy of OpenCV's license file can be found at `LICENSE-OpenCV`.

### Compile OpenCV from Sources
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

### Compile OpenCV from Git Repo
To obtain and compile OpenCV's source code from git, set the following environment variables

```shell
# required 
# set those variables if you'd like to compile OpenCV from git
##   the corresponding license file should be available at https://github.com/opencv/opencv/blob/${OPENCV_USE_GIT_BRANCH}/LICENSE
export OPENCV_USE_GIT_HEAD=true
export OPENCV_USE_GIT_BRANCH=4.x

# optional.
## set this if you want to use to your/other fork/mirrors
export OPENCV_GIT_REPO="https://github.com/opencv/opencv.git"

## enable FFmpeg
##   this will allow cmake to auto-detect FFmpeg libraries installed on the host
##   on Windows, OpenCV will download prebuilt FFmpeg libraries
##   for more information, please visit https://github.com/opencv/opencv/tree/4.x/3rdparty/ffmpeg
export CMAKE_OPENCV_OPTIONS="-D WITH_FFMPEG=ON"

## disable FFmpeg
export CMAKE_OPENCV_OPTIONS="-D WITH_FFMPEG=OFF"
```

### Available modules
Current available modules:
- calib3d
- core
- dnn
- features2d
- flann
- highgui
- imgcodecs
- imgproc
- ml
- photo
- stitching 
- ts 
- video 
- videoio

Note 1: to be able to encode/decode more video formats, you may compile OpenCV with FFmpeg enabled. 

However, you should be aware of the license of the FFmpeg components you selected as they could be licensed by LGPL/GPL or other licenses.

Note 2: FFmpeg 5 is not supported by OpenCV yet.

```shell
sudo apt install -y libavcodec-dev libavformat-dev libavutil-dev libswscale-dev libavresample-dev ffmpeg
```

on macOS
```shell
brew install ffmpeg@4
brew link ffmpeg@4
```

## Dependencies

### Required

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

#### Extra Notes for Windows
Evision on Windows uses `nmake` to handle the `Makefile.win` at the moment. And we also need `powershell` for now. `nmake`
should be included in Microsoft Visual Studio, and `powershell` should be included in almost all recent version (it was 
first released in 2007).

If `ninja` can be found in `%PATH%`, then we will prefer using `ninja` to build everything as it allows parallel building.

Evision is NOT tested in MSYS2, Cygwin, or WSL/WSL2. 

### Optional

To skip the download process, you can put the source zip file at `3rd_party/cache/opencv-${OPENCV_VER}.zip`.

Or you could supply OpenCV source code at `3rd_party/opencv/opencv-${OPENCV_VER}`.

## Installation

In order to use `evision`, you will need Elixir installed. Then create an Elixir project via the `mix` build tool:

```sh
$ mix new my_app
```

Then you can add `evision` as dependency in your `mix.exs`. At the moment you will have to use a Git dependency while we work on our first release:

```elixir
def deps do
  [
    {:evision, "~> 0.1.6", github: "cocoa-xu/evision", tag: "v0.1.6"}
  ]
end
```

### Configuration

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

#### Unsupported Type Map
As OpenCV does not support the following types

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

### Notes 

#### Compile-time related

- How do I specify which OpenCV version to compile?
    ```shell
    # e.g., use OpenCV 4.5.5
    # current, evision uses 4.6.0 by default 
    export OPENCV_VER=4.5.5
    ```

- How do I use my own OpenCV source code on my local disk?

    ```shell
    export OPENCV_DIR=/path/to/your/opencv/source/root
    ```

- How do I use my own OpenCV source code on my git?

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

      otherwise, your editing will be overwritten by the `py_src/gen2.py`
  
    ```cmake
    execute_process(COMMAND bash -c "rm -rf ${GENERATED_ELIXIR_SRC_DIR} && mkdir -p ${GENERATED_ELIXIR_SRC_DIR}")
    message("enabled modules: ${ENABLED_CV_MODULES}")
    execute_process(COMMAND bash -c "python3 ${PY_SRC}/gen2.py ${C_SRC} ${GENERATED_ELIXIR_SRC_DIR} ${C_SRC}/headers.txt ${ENABLED_CV_MODULES}" RESULT_VARIABLE STATUS)
    if(STATUS STREQUAL "0")
      message("Successfully generated Erlang/Elixir bindings")
    else()
      message(FATAL_ERROR "Failed to generate Erlang/Elixir bindings")
    endif()
    ```  

  3. Lastly, you can edit the source files and recompile the project.
    ```shell
    mix compile
    ```

#### Runtime related

- How do I enable debug logging for OpenCV (prints to stderr).
    ```shell
    export OPENCV_EVISION_DEBUG=1
    ```

### Namespace
`:evision` is just one possible OpenCV-Elixir bindings, and that means I didn't write ANY actual algorithms here. As for
the reason why I chose `OpenCV` as the namespace in previous commits, that's because I don't want to give a feeling to 
users that all these functions/algorithms come from `:evision`.

However, as there are more people get interested in this project, I have to think about this question carefully. And after
various considerations, I decided to use `Evision` as the root namespace for this project. The reasons are

1. The `app` name of this project is `:evision`, therefore, I should use `Evision` so that it's consistent. 
2. `:evision` is one possible OpenCV-Elixir bindings, so it is better to use `Evision` as the namespace.
3. Using `OpenCV` as the namespace could be misleading at times if something went wrong in `:evision`'s code.
4. This leaves the choice to users to choose whether if they would like to alias `Evision` as `OpenCV` or any other names.

Please note that although everything now will be under the `Evision` namespace, the actual algorithms/functions come from
the OpenCV project.

```elixir
alias Evision, as: Cv
```

### Current Status

Some tiny examples:

```elixir
alias Evision, as: Cv

## read image, process image and write image
gray_mat = Cv.imread!("/path/to/img.png", flags: Cv.cv_IMREAD_GRAYSCALE)
gray_blur_mat = Cv.blur!(gray_mat, [10,10], anchor: [1,1])
colour_mat = Cv.imread!("/path/to/img.png")
colour_blur_mat = Cv.blur!(colour_mat, [10,10], anchor: [1,1])
:ok = Cv.imwrite("/path/to/img-gray-and-blur.png", gray_blur_mat)
:ok = Cv.imwrite("/path/to/img-colour-and-blur.png", colour_blur_mat)

## capture from video file/camera
cap = Cv.VideoCapture.videoCapture!(0)
cap_mat = Cv.VideoCapture.read!(cap)
:ok = Cv.imwrite("/path/exists/capture-mat.png", cap_mat)
:error = Cv.imwrite("/path/not/exists/capture-mat.png", cap_mat)

## to_binary/from_binary
mat = Cv.imread!("/path/to/img.jpg")
type = Cv.Mat.type!(mat)
{rows, cols, channels} = Cv.Mat.shape!(mat)
binary_data = Cv.Mat.to_binary!(mat)
Cv.Mat.from_binary(binary_data, type, cols, rows, channels)
```

Some other [examples](https://github.com/cocoa-xu/evision/tree/main/examples).

### Acknowledgements
- `gen2.py`, `hdr_parser.py` and `c_src/erlcompat.hpp` were directly copied from the `python` module in the [OpenCV repo](https://github.com/opencv/opencv). Changes applied.
- `Makefile`, `CMakeLists.txt` and `c_src/nif_utils.hpp` were also copied from the `torchx` module in the [elixir-nx repo](https://github.com/elixir-nx/nx). Minor changes applied.
