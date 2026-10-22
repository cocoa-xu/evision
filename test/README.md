## Tests

As we allow users to select a subset of OpenCV modules, all tests have corresponding tags and, by default, they will be
enabled by the availability of the module (i.e., will be tested if the corresponding module is compiled). 

However, some test data is relative large thus we don't store them in git (to avoid long cloning time and/or wasting the
disk space if you don't want to run tests). Therefore, we have a special tag named `:require_downloading`. If you'd like
to test the ones that require downloading test data, you can do

```shell
mix test --include require_downloading
```

Another special tag is `:require_ffmpeg`. OpenCV can compile and use video module when any of the following points is met
1. your system has native support for camera
2. your system has native support for some video files
3. ffmpeg related libraries are installed

Therefore, the availability of the video module does not imply that some tests can decode or encode the test video files
correctly, as the formats specified in these tests may not support by the native library provided by your system. 

Another thing is that, even if it is possible to test the camera, it would require setting up a virtual camera in the CI
environment, and that would introduce much work. So we won't consider writing test for cameras at the moment. 

Therefore, if you have ffmpeg installed and the OpenCV library is compiled with ffmpeg, then you can enable these tests
by

```shell
mix test --include require_ffmpeg
```


### Native boundary regressions

The `evision_*safety_test.exs` files call `:evision_nif` directly, bypassing the
Elixir wrappers. They cover malformed shapes, types and indices, integer
overflow, binary extents, and C++ exception containment. Run them together with
the normal Nx and Mat tests so stricter validation cannot silently reject valid
operations.

Use a source-built NIF, not a downloaded release. When changing C++ sources in
an existing build, remove only the old NIF before rebuilding: the Makefile can
otherwise reuse it even when a header has changed.

```sh
rm -f _build/test/lib/evision/priv/evision.so
MIX_ENV=test EVISION_PREFER_PRECOMPILED=false mix compile
MIX_ENV=test EVISION_PREFER_PRECOMPILED=false mix test --no-compile \
  test/evision_*safety_test.exs test/evision_backend_test.exs \
  test/evision_mat_test.exs test/evision_new_types_test.exs \
  test/evision_input_immutability_test.exs test/evision_backend_copy_test.exs
```

For native sanitizer runs, compile the NIF with
`-fsanitize=address,undefined -fno-omit-frame-pointer` and load the matching ASan
runtime before starting the BEAM executable. Use
`ASAN_OPTIONS=detect_leaks=0:abort_on_error=1` and
`UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1` so native failures fail the run.
Leak detection is disabled for the host VM; this does not disable bounds checks.
On macOS, launch `beam.smp` directly because protected shell launchers can strip
`DYLD_INSERT_LIBRARIES` before Erlang starts.

Tests tagged `:face`, `:xfeatures2d`, or `:stitching` run only when their OpenCV
modules are compiled, like the existing `:dnn`, `:ml`, `:photo`, and `:video` tags.

The copy regressions exercise contiguous blocks, positive and negative strides,
scalar inputs, aliased updates, and output lifetime after the input process exits.
Performance measurements and reproduction commands are in
[`benchmarks/native_boundaries/README.md`](../benchmarks/native_boundaries/README.md).

### downloading_list.txt

```shell
grep ' @download_file' *.exs > downloading_list.txt
```
