# evision [WIP]

|: OS              |: arch   | Build Status |
|------------------|---------|--------------|
| Ubuntu 20.04     | arm64   | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-arm64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-arm64.yml) |
| Ubuntu 20.04     | armv7   | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-armv7.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-armv7.yml) |
| Ubuntu 20.04     | s390x   | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-s390x.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-s390x.yml) |
| Ubuntu 20.04     | ppc64le | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-ppc64le.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-ppc64le.yml) |
| Ubuntu 20.04     | x86_64  | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/linux-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/linux-x86_64.yml) |
| macOS 11 Big Sur | x86_64  | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/macos-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/macos-x86_64.yml) |

## Nerves Support

|: MIX_TARGET | Build Status |
|-------------|--------------|
| rpi4        | [![CI](https://github.com/cocoa-xu/evision/actions/workflows/nerves-rpi4.yml/badge.svg)](https://github.com/cocoa-xu/evision/actions/workflows/nerves-rpi4.yml) |

`evision` will pull OpenCV source code from GitHub, then parse and automatically generate corresponding OpenCV-Elixir bindings.

This project uses and modifies `gen2.py` and `hdr_parser.py` from the `python` module in the [OpenCV repo](https://github.com/opencv/opencv) so that they output header files that can be used in Elixir bindings. 

We hope this project can largely reduce the work of manually porting OpenCV functions/modules to Elixir.

Current available modules:
- calib3d
- core
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

## Dependencies

- Python3 (Only during the compliation, required to compile OpenCV)
- [CMake](https://cmake.org/)

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

Note use `MAKE_BUILD_FLAGS="-j$(nproc)"` environment variable to set number of jobs for compiling.

Use `TOOLCHAIN_FILE="/path/to/toolchain.cmake"` to set your own toolchain.

### Current Status
```elixir
{:ok, gray_mat} = OpenCV.imread("/path/to/img.png", flags: OpenCV.cv_imread_grayscale)
{:ok, gray_blur_mat} = OpenCV.blur(gray_mat, [10,10], anchor: [1,1])
{:ok, colour_mat} = OpenCV.imread("/path/to/img.png")
{:ok, colour_blur_mat} = OpenCV.blur(colour_mat, [10,10], anchor: [1,1])
:ok = OpenCV.imwrite("/path/to/img-gray-and-blur.png", gray_blur_mat)
:ok = OpenCV.imwrite("/path/to/img-colour-and-blur.png", colour_blur_mat)

{:ok, cap} = OpenCV.VideoCapture.videocapture(0)
{:ok, cap_mat} = OpenCV.VideoCapture.read(cap)
:ok = OpenCV.imwrite("/path/exists/capture-mat.png", cap_mat)
:error = OpenCV.imwrite("/path/not/exists/capture-mat.png", cap_mat)
```

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
   {:ok, cap} = OpenCV.VideoCapture.videocapture(0, [])
   # but this
   {:ok, cap} = OpenCV.VideoCapture.videocapture(0)
   # this also changes the above example to
   :ok = OpenCV.imwrite("/path/to/save.png", mat)
   ```
- [x] Nerves support (rpi4 for now).
- [ ] Add `OpenCV.Mat` module.
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
