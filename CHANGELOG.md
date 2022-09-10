# Changelog

## v0.1.4 (2022-09-10)
- Default to `Evision.Backend` for `Evision.Nx.to_nx/2`.
- Fix class inheritance issue in `py_src/class_info.py`.
- Fix missing comma in example livebooks' `Mix.install`. @dbii
- Add decision tree and random forest example.

## v0.1.3 (2022-09-01)
- Fix issues in restoring files from precompiled package for macOS and Linux.
  - Paths are now quoted. 
  - using `cp -RPf` on macOS while `cp -a` on Linux.
- Fix `destroyAllWindows` in NIF.
  It was generated as 'erlang:destroyAllWindows/1' but it should be 'erlang:destroyAllWindows/0'.

## v0.1.2 (2022-08-26)
- Added x86_64 musl compilation CI test.
- Fixed transpose.
- Added a few precompilation musl targets:
  - x86_64-linux-musl
  - aarch64-linux-musl
  - armv7l-linux-musleabihf
  - riscv64-linux-musl

## v0.1.1 (2022-08-25)
- Use OpenCV 4.6.0 by default.
- Implemented a few nx callbacks (remaining ones will be implemented in the next release).
- Deprecated the use of the `EVISION_PRECOMPILED_VERSION` environment variable. The version information will be implied by the tag:

    ```elixir
    def deps do
    [
        {:evision, "~> 0.1.1", github: "cocoa-xu/evision", tag: "v0.1.1"}
    ]
    end
    ```
  
  The value of the environment variable `EVISION_PREFER_PRECOMPILED` decides whether the precompiled artefacts will be used or not.

  ~~From the next version (>=0.1.2), `evision` will set `EVISION_PREFER_PRECOMPILED` to `true` by default.~~

## v0.1.0 (2022-07-23)
First release.
