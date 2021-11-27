# evision [WIP]
`evision` will search OpenCV libraries installed on your system, then parse and automatically generate corresponding OpenCV-Elixir bindings.

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

- Python3 (Only during the compliation)
- NumPy (Only during the compliation)
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

### Current Status
```elixir
{:ok, gray_mat} = OpenCV.imread("/path/to/img.png", flags: OpenCV.cv_imread_grayscale)
{:ok, gray_blur_mat} = OpenCV.blur(gray_mat, [10,10], anchor: [1,1])
{:ok, colour_mat} = OpenCV.imread("/path/to/img.png", flags: OpenCV.cv_imread_color)
{:ok, colour_blur_mat} = OpenCV.blur(colour_mat, [10,10], anchor: [1,1])
{:ok, true} = OpenCV.imwrite("/path/to/img-gray-and-blur.png", gray_blur_mat, [])
{:ok, true} = OpenCV.imwrite("/path/to/img-colour-and-blur.png", colour_blur_mat, [])

{:ok, cap} = OpenCV.VideoCapture.videocapture(0, [])
{:ok, {true, cap_mat}} = OpenCV.VideoCapture.read(cap, [])
{:ok, true} = OpenCV.imwrite("/path/to/capture-mat.png", cap_mat, [])
```

### Todo

- [x] Update `.py` files in `py_src` so that they output header files for Erlang bindings.
- [x] Automatically generate `erl_cv_nif.ex`.
- [x] Automatically generate `opencv_*.ex` files using Python.
- [x] Automatically convert enum constants in C++ to `@` constants in Elixir
- [ ] Add tests.

### How does this work?

1. This project will first pull OpenCV source code from git (as git submodules).
2. Inside the OpenCV project, there is an `opencv-python` module, `3rd_party/opencv/modules/python`. If the
   `opencv-python` module is enabled,

   ```bash
   cmake ...
       -D PYTHON3_LIBRARY=$(PYTHON3_LIBRARY) \
       -D PYTHON3_INCLUDE_DIR=$(PYTHON3_INCLUDE_DIR) \
       -D PYTHON3_EXECUTABLE=$(PYTHON3_EXECUTABLE) \
       ...
   ```

   It will generate files for opencv-python bindings in cmake build dir, `cmake_build_dir/modules/python_bindings_generator`. 

   We are interested in the `headers.txt` file as it tells us which headers should we parse (this header list changes
   depending on enabled modules).

   We also need to check another file, `pyopencv_custom_headers.h`. This file includes pyopencv compatible headers from 
   modules that need special handlings to enable interactions with Python. We will talk about this later.
3. Originally, the `headers.txt` will be passed to `3rd_party/opencv/modules/python/src2/gen2.py` and that Python script
   will then generate `pyopencv_*.h` in `cmake_build_dir/modules/python_bindings_generator`. Here we copy that Python
   script and modify it so that it outputs `c_src/evision_*.h` files which use `c_src/erlcompat.hpp` and `c_src/nif_utils.hpp`
   to make everything compatible with Erlang.
4. `c_src/opencv.cpp` includes almost all specialised and generic `evision_to` and `evision_from` functions. They are
   used for making conversions between Erlang and C++. Some conversion functions are defined in module custom headers.
5. This is where we need to make some changes to `pyopencv_custom_headers.h`. We first copy it to `c_src/evision_custom_headers.h`
   and copy every file it includes to `c_src/evision_custom_headers`. Then we make corresponding changes to `c_src/evision_custom_headers/*.hpp`
   files so that these types can be converted from and to Erlang terms. The header include path in `c_src/evision_custom_headers.h`
   should be changed correspondingly.
6. However, it is hard to do step 5 automatically. We can try to create a PR which puts these changed files to the
   original OpenCV repo's `{module_name}/mics/erlang`. Now we just manually save them in `c_src/evision_custom_headers`.
   Note that step 5 and 6 are done manually, calling `py_src/gen2.py` will not have effect on `c_src/evision_custom_headers.h`
   and `*.hpp` files in `c_src/evision_custom_headers`.
7. Another catch is that, while function overload is easy in C++ and optional argument is simple in Python, they are not
   quite friendly to Erlang/Elixir. There is no function overload in Erlang/Elixir. Although Erlang/Elixir support
   optional argument (default argument), it also affects the function arity and that can be very tricky to deal with. For
   example,

   ```elixir
   defmodule OpenCV.VideoCapture do
     def open(self, camera_index, opts \\ []), do: :nil
     def open(self, filename), do: :nil
     # ... other functions ...
   end
   ```

   In this case, `def open(self, camera_index, opts \\ []), do: :nil` will define `open/3` and `open/2` at the same time.
   This will cause conflict with `def open(self, filename), do: :nil` which define `open/2`.

   So we cannot use default argument. Now say we have

   ```elixir
   defmodule OpenCV.VideoCapture do
     def open(self, camera_index, opts), do: :nil
     def open(self, filename, opt), do: :nil
   end
   ```

   Function overload in C++ is relatively simple as compiler does that for us. In this project, we have to do that ourselves.
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
   
   In such cases, we only keep one definition. The overload will be done in `c_src/opencv.cpp` (by `evision_to`).

### How do I make it compatible with more OpenCV modules?

Because of the reason in step 6, when you enable more modules, if that module has specialised custom header for python
bindings, the custom headers will be added to `cmake_build_dir/modules/python_bindings_generator/pyopencv_custom_headers.h`.
Then you can manually copy corresponding specialised custom headers to `c_src/evision_custom_headers` and modify these
conversion functions in them.

### Acknowledgements
- `gen2.py`, `hdr_parser.py` and `c_src/erlcompat.hpp` were directly copied from the `python` module in the [OpenCV repo](https://github.com/opencv/opencv). Changes applied.
- `Makefile`, `CMakeLists.txt` and `c_src/nif_utils.hpp` were also copied from the `torchx` module in the [elixir-nx repo](https://github.com/elixir-nx/nx). Minor changes applied.
