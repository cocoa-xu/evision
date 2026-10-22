# Native boundary performance

Measured on 2026-10-23 after hardening the native input boundaries. The initial
safety change slowed large slices and reversals by about 10-12%. Keeping those
checks and changing the copying strategy recovered that cost and substantially
improved the complete Nx operations.

## Complete Nx calls

These measurements include `Nx.to_binary/1` materialization, with 512 x 512 f32
inputs. Slices and updates cover a 256 x 256 region starting at `[1, 1]`.
Each cell is the median of three independent VM medians, in microseconds.

| Operation | Before safety checks | Safety checks | Checks plus optimization | Speedup over safety version |
| --- | ---: | ---: | ---: | ---: |
| `Nx.slice/3` | 136.375 | 150.000 | 26.167 | 5.73x |
| `Nx.reverse/2`, last axis | 467.042 | 520.208 | 35.000 | 14.86x |
| `Nx.put_slice/3` | 178.166 | 177.291 | 61.875 | 2.87x |

The other 14 Nx cases cover arithmetic, reductions, cumulative sums, sorting,
indexing, windows, convolution, triangular solve, and concatenation. Their
optimized versus safety-version medians changed by -1.5% to +3.1%. This small
single-machine sample does not establish statistical equivalence or a speedup
for those operations. All per-run medians are retained in [summary.csv](summary.csv).

## Direct native calls

These measurements isolate synchronous NIF calls and output allocation. Output
binary export and correctness verification are outside the timed region. Inputs
are 1024 x 1024 f32 unless the case name states otherwise.

| Operation | Input storage | Safety, us | Optimized, us | Speedup |
| --- | --- | ---: | ---: | ---: |
| Slice | BEAM binary | 557.500 | 70.951 | 7.86x |
| Slice | OpenCV allocation | 556.333 | 25.602 | 21.73x |
| Reverse last axis | BEAM binary | 2093.500 | 148.167 | 14.13x |
| Reverse last axis | OpenCV allocation | 2091.000 | 88.882 | 23.53x |
| Put slice | BEAM binary | 582.800 | 77.526 | 7.52x |
| Put slice | OpenCV allocation | 580.333 | 64.775 | 8.96x |
| Indexed put | BEAM binary | 149.667 | 89.519 | 1.67x |
| Indexed put | OpenCV allocation | 147.500 | 62.578 | 2.36x |
| As shape | BEAM binary | 97.667 | 50.797 | 1.92x |
| As shape | OpenCV allocation | 96.226 | 52.944 | 1.82x |
| Exact batches | BEAM binary | 97.615 | 96.385 | 1.01x |
| Exact batches | OpenCV allocation | 95.391 | 43.297 | 2.20x |
| Repeated final batch | BEAM binary | 134.100 | 99.000 | 1.35x |
| Repeated final batch | OpenCV allocation | 132.565 | 90.886 | 1.46x |

Binary-backed inputs retain the existing defensive copy, so exact batching of
those inputs shows essentially no improvement. Large quotient, integer
reduction, and integer window cases remain close to the safety version, within
about 1%. Large s64 quotient still costs about 2% more than the pre-safety
baseline, including the required division edge-case checks.

Small calls are dominated by VM dispatch and allocation and show substantial
run-to-run variation. For example, binary-backed 8 x 8 `as_shape` measured
1.99, 3.20, and 4.59 us in the optimized full runs versus 1.93, 1.93, and 2.01 us
in the safety runs. Five additional isolated runs per version, with alternating
order, measured 2.26 us for safety and 2.07 us optimized. Both sets are retained;
there is no blanket claim that every small call became faster or that all
small-input regressions have been ruled out.

## Changes and correctness

- Coalesce contiguous trailing dimensions into one `memcpy` per block.
- Copy strided inner rows with fixed-width byte copies, allowing the compiler
  to vectorize reversal without a multidimensional carry per element.
- Share read-only OpenCV allocations only through the existing input converter.
  Binary-backed inputs keep their ownership protection.
- Reuse the converter's private copy for writable scatter and reshape outputs,
  removing a second deep copy. Aliased inputs remain immutable.
- Allocate the repeated final batch directly and fill every byte, removing an
  intermediate zero-filled buffer and another clone.

Rank, type, extent, stride, allocation-size, and index checks remain in place.
The final iteration still avoids advancing beyond the valid coordinate range.
Non-contiguous read-only inputs are made continuous before flat copying.

The complete available suite passed on both the optimized Release NIF and an
ASan/UBSan NIF: **264 passed, 37 excluded**, with no sanitizer findings. The
normal CMake build was rebuilt and tested separately. The five new copy tests
exercise ranks 1-4, scalar inputs, 1/2/4/8-byte elements, both storage owners,
positive/negative/zero strides, singleton axes with large strides, block
boundaries, aliased updates, immutability, and output lifetime after the input
process exits. Existing malformed-input regressions run in the same suite.

The excluded tests require unavailable optional modules, downloads, CUDA,
codecs, or OCR. Those paths were not validated. Sanitizer instrumentation covers
the NIF; the shared OpenCV libraries and BEAM were not rebuilt with sanitizers.
Leak checking is disabled for the host VM. See [test instructions](../../test/README.md).

## Method and artifacts

Hardware and software: Apple M6, 12 logical CPUs, 24 GiB RAM, macOS 27.0.1 arm64,
Apple clang 21.0.0, OpenCV 5.0.0, Erlang 29.1.2, Elixir 1.20.4, Nx 0.12.1.
OpenCV uses one thread; BEAM uses `+S 12:12 +SDcpu 12`. CPU affinity and frequency
were not pinned. These are warm repeated-call measurements on one machine,
not cold-cache, concurrent-throughput, or cross-platform guarantees.

Three NIF variants use the same generated bindings, compiler options, OpenCV
libraries, and Elixir benchmark harness:

| Label | Native source |
| --- | --- |
| `baseline` | `5b2a528`, before boundary hardening |
| `safe` | `f0c0595`, boundary hardening and its tests |
| `optimized` | `e9a49f9`, copy optimizations and regressions (source hashes recorded) |

The NIFs were compiled with `-std=c++17 -O3 -DNDEBUG` and identical linking
options. Sanitizers were disabled for timing. Native source snapshots were
created by copying the generated `c_src` tree, then replacing tracked sources
with `git archive <revision> c_src`. Each variant ran in its own fresh VM with
the NIF replaced only between processes. No builds or tests ran concurrently
with the retained measurements.

The direct suite contains 42 cases. Each uses 10 warmup calls, calibration,
then 25 timed batches targeting approximately 3 ms each, capped at 1000 calls.
Explicit GC occurs before each batch; automatic GC during the batch is included.
Every output byte and input immutability are checked outside timing. Three
independent runs per variant used rotated variant order. Preliminary runs are
excluded; retained labels are `r2`, `r3`, and `r4`.

The Nx suite uses 10 warmups and 25 single-operation samples, with explicit GC
outside each sample. The 14 control cases and three new copy cases ran as
separate groups, each three times per variant. Results are checked against
BinaryBackend or an independent row-splicing reference before timing. Input
preparation and backend transfer are not timed; output materialization is.

- [environment.json](environment.json): environment, exact compiler argument
  array, NIF SHA256 hashes, and optimized source hashes. `$REPO` marks the
  repository path in compiler arguments.
- [measurements.jsonl](measurements.jsonl): all 37 retained VM runs with raw
  samples, calibration batch sizes, verification status, and build identities.
- [summary.csv](summary.csv): medians of per-run medians, individual run medians,
  and ratios for all cases. Speedup is safety time divided by optimized time.

The Nx artifact's original `git_revision` describes the common Elixir harness;
the explicit `variant` and `nif_sha256` identify the measured native build.

## Reproduction

Build the NIF from source first, following the repository build instructions.
Use the same generated bindings, OpenCV build, compiler, optimization flags,
thread settings, and benchmark harness when comparing source revisions. The
`native_compiler_argv` array records the exact macOS build command used here;
substitute the repository path and each variant's source/output directory.
For other platforms use their normal linker options consistently for all builds.

With the desired NIF installed at `_build/test/lib/evision/priv/evision.so`, run:

```sh
export MIX_ENV=test
export EVISION_FETCH_PRECOMPILED=true
export ERL_FLAGS='+S 12:12 +SDcpu 12'

mix run --no-compile scripts/bench_native.exs -- \
  --label optimized --samples 25 --threads 1 \
  --output _build/native-performance/reproduced-native.json

mix run --no-compile \
  -e 'Evision.setNumThreads(1); Mix.Tasks.Evision.Backend.Bench.run(System.argv())' -- \
  --profile default --warmup 10 --iterations 25 --only copy \
  --output _build/native-performance/reproduced-nx-copy.json
```

Omit `--only copy` to run all 17 Nx cases. The native script accepts `--only`
as a case-name substring filter. Repeat each build in separate VMs with rotated
order. Keep the current benchmark harness when measuring older NIF builds;
the old revisions do not contain the added cases. `--no-compile` prevents Mix
from replacing the NIF under measurement. Never replace a NIF file while a
VM is using it; install each variant before launching that VM.
