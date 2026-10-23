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

### CPU dispatch

Dense Nx `select` uses a baseline implementation or an optional AArch64 NEON /
x86_64 AVX2 kernel. OpenCV checks CPU and OS feature support at runtime. AVX2 is
compiled in its own source file so its compiler flags do not affect the baseline.
Other architectures and unsupported toolchains retain the baseline implementation.

Set `EVISION_CPU_DISPATCH=baseline` before starting the VM to disable these
optional kernels, or change the mode for subsequent calls at runtime:

```elixir
Evision.Backend.set_cpu_dispatch(:baseline)
Evision.Backend.set_cpu_dispatch(:auto)
Evision.Backend.cpu_dispatch_info()
```

The setting is shared by all processes in the BEAM VM. Calls already in progress
may finish with their previous implementation. It does not bind CPU cores, change
OpenCV's own optimization settings, or eliminate compiler-generated baseline SIMD.
The reported FP16, DotProd, and BF16 capabilities are feature observations, not
claims that `select` uses those instructions. Universal macOS builds retain the
baseline implementation; per-architecture builds enable their supported kernels.

The standalone `cpu_dispatch_test` exercises all 256 mask patterns per block,
unaligned inputs, every short tail, input immutability, fallback selection, and
the baseline override. Guard-page cases place buffers against inaccessible pages
at either end and protect inputs as read-only. They cover empty buffers and long
tails as well as short SIMD boundaries, so padding cannot hide out-of-range reads.
Native CI includes macOS and Linux AArch64, x86_64 Linux, macOS and Windows, plus
the baseline under big-endian s390x emulation. Linux/macOS sanitizer jobs fail on
the first ASan or UBSan finding. NIF tests exercise both modes, compare Nx results
with BinaryBackend, and verify input/output ownership after creator exit and GC
with one or four OpenCV threads and unaligned binary-backed or OpenCV-owned inputs.

Compare complete Nx operations, including output materialization, with:

```shell
EVISION_CPU_DISPATCH=baseline mix evision.backend.bench --only select --output /tmp/select-baseline.json
EVISION_CPU_DISPATCH=auto mix evision.backend.bench --only select --output /tmp/select-auto.json
```

Each report records the selected implementation and available CPU features.

### Running Elixir tests

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
