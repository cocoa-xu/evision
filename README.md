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

- Python3
- NumPy
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

### Todo

- [x] Update `.py` files in `py_src` so that they output header files for Erlang bindings.
- [x] Automatically generate `erl_cv_nif.ex`.
- [ ] Automatically generate `opencv_*.ex` files using Python.

### Acknowledgements
- `gen2.py`, `hdr_parser.py` and `c_src/erlcompat.hpp` were directly copied from the `python` module in the [OpenCV repo](https://github.com/opencv/opencv). Changes applied.
- `Makefile`, `CMakeLists.txt` and `c_src/nif_utils.hpp` were also copied from the `torchx` module in the [elixir-nx repo](https://github.com/elixir-nx/nx). Minor changes applied.
