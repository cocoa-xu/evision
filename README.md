# evision [WIP]

| OS               | arch    | Build Status | Has Precompiled Library |
|------------------|---------|--------------|-------------------------|
| Ubuntu 20.04     | x86_64  | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-x86_64.yml) | Yes |
| Ubuntu 20.04     | arm64   | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile.yml) | Yes |
| Ubuntu 20.04     | armv7   | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-precompile.yml) | Yes |
| macOS 11 Big Sur | x86_64  | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/macos-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/macos-x86_64.yml) | Yes |
| macOS 11 Big Sur | arm64   | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/macos-precompile.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/macos-precompile.yml) | Yes |
| Windows 2019     | x86_64  | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/windows-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/windows-x86_64.yml) | Not yet |

`ppc64le` and `s390x` emulators are too slow. Let's focus on x86_64, arm64 and armv7 for now. Furthermore, nerves workflow
will compile evision to arm64, armv7 and armv6, therefore, `linux-arm64.yml` and `linux-armv7.yml` will now be triggered 
manually only.

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
{:ok, mat} = Evision.imread("/path/to/image.png")
# Or you can use the !(bang) version, but if the image cannot be read by OpenCV for whatever reason
# the bang version will raise a RuntimeError exception
mat = Evision.imread!("/path/to/image.png")
t = Evision.Nx.to_nx(mat)
```

and vice-versa:

```elixir
{:ok, mat} = Evision.imread("/path/to/image.png")
t = Evision.Nx.to_nx(mat)
# convert a tensor to a mat
{:ok, mat_from_tensor} = Evision.Nx.to_mat(t)

# and it works for tensors with any shapes
t = Nx.iota({2, 3, 2, 3, 2, 3}, type: {:s, 32})
{:ok, mat} = Evision.Nx.to_mat(t)
```

## Description

`evision` will pull OpenCV source code from GitHub, then parse and automatically generate corresponding OpenCV-Elixir bindings.

This project uses and modifies `gen2.py` and `hdr_parser.py` from the `python` module in the [OpenCV repo](https://github.com/opencv/opencv) so that they output header files that can be used in Elixir bindings. 

We hope this project can largely reduce the work of manually porting OpenCV functions/modules to Elixir.

Compatible OpenCV versions:
- 4.5.3
- 4.5.4
- 4.5.5

by compatible, it means these versions can compile successfully, and I tested a small range of functions. Tons of tests
should be written, and then we can have a list for tested OpenCV versions.

### Use Precompiled Library
To use precompiled Evision library, the following environment variables should be set

```shell
# required if prefer to use precompiled library
export EVISION_PREFER_PRECOMPILED=true

# optional. 
## currently only "0.1.0-dev" is valid
export EVISION_PRECOMPILED_VERSION="0.1.0-dev"
## set the cache directory for the precompiled archive file
export EVISION_PRECOMPILED_CACHE_DIR="$(pwd)/.cache"
```

Note that using precompiled library requires installing FFmpeg libraries. See the end of [Available Modules](#available-modules) for more information.

### Compile OpenCV from Sources
To obtain and compile OpenCV's source code from official releases, the following environment variables can affect the build

```shell
# optional.
## set OpenCV version
export OPENCV_VER="4.5.5"
```

### Compile OpenCV from Git Repo
To obtain and compile OpenCV's source code from git, set the following environment variables

```shell
# required if compile OpenCV from git
export OPENCV_USE_GIT_HEAD=true
export OPENCV_USE_GIT_BRANCH=4.x

# optional.
## set this if you want to use to your/other fork/mirrors
export OPENCV_GIT_REPO="https://github.com/opencv/opencv.git"
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

Note 1: to open video files, FFmpeg related libraries should be installed, e.g., on Debian/Ubuntu

Note 2: FFmpeg 5 is not supported by OpenCV yet

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
- [CMake](https://cmake.org/) >= 3.1 (for this project)

  The minimal version required by OpenCV can vary between versions.

  OpenCV 4.5.5 requires at least CMake 3.5.1.
- Erlang development library/headers. Tested on OTP/24.

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
    {:evision, "~> 0.1.0-dev", github: "cocoa-xu/evision", branch: "main"}
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

### Notes 

#### Compile-time related

- How do I specify which OpenCV version to compile?
    ```shell
    # e.g., use OpenCV 4.5.4
    # current, evision uses 4.5.5 by default 
    export OPENCV_VER=4.5.4
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

- Which ones of OpenCV options are supposed to be specified in `config/config.exs`?
  1. Enabled and disabled OpenCV modules
  2. Image codecs (if you enabled related OpenCV modules).

#### Debug related

Say you have the following MIX environment variables:

```shell
# set by MIX
MIX_ENV=dev
# set by evision or you
OPENCV_VER=4.5.5
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
alias Evision, as: OpenCV
# Or
# alias Evision, as: CV
```

### Current Status

Some tiny examples:

```elixir
alias Evision, as: OpenCV

## read image, process image and write image
{:ok, gray_mat} = OpenCV.imread("/path/to/img.png", flags: OpenCV.cv_IMREAD_GRAYSCALE)
{:ok, gray_blur_mat} = OpenCV.blur(gray_mat, [10,10], anchor: [1,1])
{:ok, colour_mat} = OpenCV.imread("/path/to/img.png")
{:ok, colour_blur_mat} = OpenCV.blur(colour_mat, [10,10], anchor: [1,1])
:ok = OpenCV.imwrite("/path/to/img-gray-and-blur.png", gray_blur_mat)
:ok = OpenCV.imwrite("/path/to/img-colour-and-blur.png", colour_blur_mat)

## capture from video file/camera
{:ok, cap} = OpenCV.VideoCapture.videoCapture(0)
{:ok, cap_mat} = OpenCV.VideoCapture.read(cap)
:ok = OpenCV.imwrite("/path/exists/capture-mat.png", cap_mat)
:error = OpenCV.imwrite("/path/not/exists/capture-mat.png", cap_mat)

## to_binary/from_binary
{:ok, mat} = OpenCV.imread("/path/to/img.jpg")
{:ok, type} = OpenCV.Mat.type(mat)
{:ok, {rows, cols, channels}} = OpenCV.Mat.shape(mat)
{:ok, binary_data} = OpenCV.Mat.to_binary(mat)
OpenCV.Mat.from_binary(binary_data, type, cols, rows, channels)
```

Some other [examples](https://github.com/cocoa-xu/evision/tree/main/examples).

### Todo

- [x] Update `.py` files in `py_src` so that they output header files for Erlang bindings.
- [x] Automatically generate `erl_cv_nif.ex`.
- [x] Automatically generate `opencv_*.ex` files using Python.
- [x] Automatically convert enum constants in C++ to "constants" in Elixir
- [x] When a C++ function's return value's type is `bool`, map `true` to `:ok` and `false` to `:error`.

   ```elixir
   # not this
   {:ok, true} = OpenCV.imwrite("/path/to/save.png", mat, [])
   # but this
   :ok = OpenCV.imwrite("/path/to/save.png", mat, [])
   ```
- [x] Make optional parameters truly optional.

   ```elixir
   # not this
   {:ok, cap} = OpenCV.VideoCapture.videoCapture(0, [])
   # but this
   {:ok, cap} = OpenCV.VideoCapture.videoCapture(0)
   # this also changes the above example to
   :ok = OpenCV.imwrite("/path/to/save.png", mat)
   ```
- [x] Nerves support (rpi4 for now).
- [x] Add `OpenCV.Mat` module.

   ```elixir
   {:ok, type} = OpenCV.Mat.type(mat)
   {:ok, {:u, 8}}
   
   {:ok, {height, weight, channel}} = OpenCV.Mat.shape(mat)
   {:ok, {1080, 1920, 3}}
   {:ok, {height, weight}} = OpenCV.Mat.shape(gray_mat)
   {:ok, {1080, 1920}}
  
   {:ok, bin_data} = OpenCV.Mat.to_binary(mat)
   {:ok, << ... binary data ... >>}
   ```
- [x] Edit `config/config.exs` to enable/disable OpenCV modules and image coders.
- [x] Make function names more readable/friendly. (expect **breaking** changes after this)

   Will be using **smallCamelCase** for almost all OpenCV functions.

   The reasons why I chose **smallCamelCase** are that

   1. it transforms much fewer function names as a large portion of them in OpenCV are starting with lowercase letter.
   2. avoid converting abbreviations to lowercase spellings, which can be quite tricky sometimes.
   3. make the APIs look similar to their corresponding C++/Python ones.
   4. This could also help when the user wants to search for existing examples/usage online.

- [ ] Add more [examples](https://github.com/cocoa-xu/evision/tree/main/examples) (perhaps as livebook).
- [ ] Add support for `dnn`. Halfway done? Tons of functions haven't been tested yet. 
- [ ] Add support for `gapi`? Perhaps not. See [#22](https://github.com/cocoa-xu/evision/issues/22).
- [ ] Add tests.

### How does this work?

1. This project will first pull OpenCV source code from git (as git submodules).
2. Inside the OpenCV project, there is an `opencv-python` module, `3rd_party/opencv/modules/python`. If the
   `opencv-python` module is enabled,

   ```bash
   cmake ...
       -D PYTHON3_EXECUTABLE=$(PYTHON3_EXECUTABLE) \
       ...
   ```

   It will generate files for `opencv-python` bindings in cmake build dir, `cmake_build_dir/modules/python_bindings_generator`. 

   We are interested in the `headers.txt` file as it tells us which headers should we parse (this header list changes
   based on the enabled modules).

   We also need to check another file, `pyopencv_custom_headers.h`. This file includes pyopencv compatible headers from 
   modules that need special handlings to enable interactions with Python. We will talk about this later.
3. Originally, the `headers.txt` file will be passed to `3rd_party/opencv/modules/python/src2/gen2.py` and that Python script
   will then generate `pyopencv_*.h` in `cmake_build_dir/modules/python_bindings_generator`. Here we copy that Python
   script and modify it so that it outputs `c_src/evision_*.h` files which use `c_src/erlcompat.hpp` and `c_src/nif_utils.hpp`
   to make everything compatible with Erlang.
4. `c_src/opencv.cpp` includes almost all specialised and generic `evision_to` and `evision_from` functions. They are
   used for making conversions between Erlang and C++. Some conversion functions are defined in module custom headers.
5. This is where we need to make some changes to `pyopencv_custom_headers.h`. We first copy it to `c_src/evision_custom_headers.h`
   and copy every file it includes to `c_src/evision_custom_headers/`. Then we make corresponding changes to `c_src/evision_custom_headers/*.hpp`
   files so that these types can be converted from and to Erlang terms. The header include path in `c_src/evision_custom_headers.h`
   should be changed correspondingly.
6. However, it is hard to do step 5 automatically. We can try to create a PR which puts these changed files to the
   original OpenCV repo's `{module_name}/mics/erlang/` directory. Now we just manually save them in `c_src/evision_custom_headers`.
   Note that step 5 and 6 are done manually, calling `py_src/gen2.py` will not have effect on `c_src/evision_custom_headers.h`
   and `*.hpp` files in `c_src/evision_custom_headers`.
7. Another catch is that, while function overloading is easy in C++ and optional arguments is simple in Python, they are not
   quite friendly to Erlang/Elixir. There is basically no function overloading in Erlang/Elixir. Although Erlang/Elixir support
   optional argument (default argument), it also affects the function's arity and that can be very tricky to deal with. For
   example,

   ```elixir
   defmodule OpenCV.VideoCapture do
     def open(self, camera_index, opts \\ []), do: :nil
     def open(self, filename), do: :nil
     # ... other functions ...
   end
   ```

   In this case, `def open(self, camera_index, opts \\ []), do: :nil` will define `open/3` and `open/2` at the same time.
   This will cause conflicts with `def open(self, filename), do: :nil` which defines `open/2`.

   So we cannot use default arguments. Now, say we have

   ```elixir
   defmodule OpenCV.VideoCapture do
     def open(self, camera_index, opts), do: :nil
     def open(self, filename, opt), do: :nil
   end
   ```

   Function overloading in C++ is relatively simple as compiler does that for us. In this project, we have to do this ourselves in the Erlang/Elixir way.
   For the example above, we can use `guards`.

   ```elixir
   defmodule OpenCV.VideoCapture do
     def open(self, camera_index, opts) when is_integer(camera_index) do
       # do something
     end
     def open(self, filename, opt) when is_binary(filename) do
       # do something
     end
   end
   ```

   But there are some cases we cannot distinguish the argument type in Erlang/Elixir becauase they are resources 
   (instance of a certain C++ class).

   ```elixir
   defmodule OpenCV.SomeModule do
     # @param mat: Mat
     def func_name(mat) do
       # do something
     end
   
     # @param mat: UMat
     def func_name(mat) do
       # do something
     end
   end
   ```
   
   In such cases, we only keep one definition. The overloading will be done in `c_src/opencv.cpp` (by `evision_to`).
8. Enum handling. Originally, `PythonWrapperGenerator.add_const` in `py_src/gen2.py` will be used to handle those enum constants. They will be saved to a map with the enum's string representation as the key, and, of course, enum's value as the value. In Python, when a user uses the enum, say `cv2.COLOR_RGB2BGR`, it will perform a dynamic lookup which ends up calling corresponding `evision_[to|from]`. `evision_[to|from]` will take the responsibility to convert between the enum's string representation and its value.
   Although in Erlang/Elixir we do have the ability to both create atoms and do the similar lookups dynamically, the problem is that, if an enum is used as one of the arguments in a C++ function, it may be written as `void func(int enum)` instead of `void func(ENUM_TYPE_NAME enum)`. 
   However, to distinguish between overloaded functions, some types (int, bool, string, char, vector) will be used for guards. For example, `void func(int enum)` will be translated to `def func(enum) when is_integer(enum), do: :nil`. Adding these guardians help us to make some differences amongst overloaded functions in step 7. However, that prevents us froming passing an atom to `def func(enum) when is_integer(enum), do: :nil`. 
   Technically, we can add one more variant `def func(enum) when is_atom(enum), do: :nil` for this specific example, but there are tons of functions has one or more `int`s as their input arguments, which means the number of variants in Erlang will increase expoentially (for each `int` in a C++ function, it can be either a real `int` or an `enum`). 
   Another way is just allow it to be either an integer or an atom:
   
   ```elixir
   def func(enum) when is_integer(enum) or is_atom(enum) do
     :nil
   end
   ```
   
   But in this way, atoms are created on-the-fly, users cannot get code completion feature for enums from their IDE. But, finally, we have a good news that, in Erlang/Elixir, when a function has zero arguments, you can write its name without explictly calling it, i.e., 
   
   ```elixir
   defmodule M do
     def enum_name(), do: 1
   end
   
   1 = M.enum_name
   1 = M.enum_name()
   ```
   
   So, in this project, every enum is actually transformed to a function that has zero input arguments.

### How do I make it compatible with more OpenCV modules?

Because of the reason in step 6, when you enable more modules, if that module has specialised custom header for python
bindings, the custom headers will be added to `cmake_build_dir/modules/python_bindings_generator/pyopencv_custom_headers.h`.
Then you can manually copy corresponding specialised custom headers to `c_src/evision_custom_headers` and modify these
conversion functions in them.

### Acknowledgements
- `gen2.py`, `hdr_parser.py` and `c_src/erlcompat.hpp` were directly copied from the `python` module in the [OpenCV repo](https://github.com/opencv/opencv). Changes applied.
- `Makefile`, `CMakeLists.txt` and `c_src/nif_utils.hpp` were also copied from the `torchx` module in the [elixir-nx repo](https://github.com/elixir-nx/nx). Minor changes applied.
