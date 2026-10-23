## Tests

### Native numeric conversions

The standalone C++ tests compile the same numeric conversion helpers used by
the NIF. They check all 65,536 encodings of both f16 and bf16 against an
independent arithmetic reference, including signed zero, subnormals, infinities,
NaNs, and aligned or unaligned native-order loads into f32 and f64.

On a little-endian development machine:

```shell
cmake -S test/native -B _build/native-casts -DCMAKE_BUILD_TYPE=Release -DEVISION_TEST_ENDIAN=little
cmake --build _build/native-casts --config Release
ctest --test-dir _build/native-casts -C Release --verbose
```

The `native-numeric-casts` workflow runs these tests on Linux, Windows, and
macOS, plus s390x Linux under QEMU user-mode emulation. The s390x test requires
big-endian byte order at runtime, so a host-native executable cannot silently
stand in for it. This checks the shared conversion code, not the complete
OpenCV/BEAM stack on s390x. The ExUnit tests in
`evision_native_conversion_safety_test.exs` cover NIF integration on the native
platforms.

### Elixir integration tests

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


### downloading_list.txt

```shell
grep ' @download_file' *.exs > downloading_list.txt
```
