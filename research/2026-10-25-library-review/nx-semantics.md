# Nx backend semantics audit

Date: 2026-10-25. Baseline: `c433cfe` on `research/library-improvements`.
Owner: `nx_semantics` research agent. All findings below predate PR #346.

## Outcome and implementation boundary

Two high-priority problems were confirmed and fixed in the working tree after root authorization:

1. Multiplying two non-scalar tensors raised instead of performing elementwise multiplication.
2. `Nx.dot` ignored explicit contracting axes and could silently return a different contraction.

The focused change is in `lib/evision/backend.ex`, with regression coverage in
`test/evision_backend_tensor_arithmetic_semantics_test.exs`. Integer add/subtract,
casting, and min/max/comparison promotion remain unchanged, with reproducible
follow-up findings below. No NIF was rebuilt, no shared BEAM was written by this
agent, and no commit or push was made.

The native multiplication path now accepts only f32/f64 operands, promotes both
to the Nx output type, aligns broadcasting, and calls typed elementwise
`Evision.Mat.multiply/3`. A later integration follow-up also admits integer scalars exactly representable
in the resolved floating output type; see [scalar multiplication](scalar-multiply.md).
Other combinations use `Nx.BinaryBackend`, then transfer the result back to Evision. This includes integer outputs, whose overflow must
not silently switch from an exception to OpenCV saturation.

The native dot path is restricted to nonbatched, two-dimensional f32/f64 inputs
with left contraction `[1]` and right contraction `[0]`. Mixed floats are promoted
before GEMM. All other axes, batches, ranks, or types use the reference backend
and return an Evision tensor. The fallback adds host-side copies and can be slow
for large integer or generalized contractions. It is an explicit correctness
boundary for future native implementations.

## Runtime verification

- Required status checks completed; the requested reset stop condition was not reached.
  The inspection CLI was closed after the audit checkpoint.
- Runtime dependency: Nx 0.12.1. The local native build reported OpenCV 5.0.0 and
  NEON select dispatch on this arm64 machine.
- Loaded backend BEAM:
  `_build/test/lib/evision/ebin/Elixir.Evision.Backend.beam`.
  Its compile source was this checkout's `lib/evision/backend.ex`.
  BEAM mtime was 2026-10-23 12:50:54 UTC, source mtime 12:46:25 UTC.
- BEAM debug definitions were inspected. They contained the exact matrix
  multiplication call at baseline line 339 and ignored dot axes at lines
  273-274. Initial failures were not caused by stale unrelated code.

## Reproduction helper

Initial probes ran against the existing build with:

```sh
MIX_ENV=test EVISION_FETCH_PRECOMPILED=true mix run --no-compile probe.exs
```

The following helper is sufficient to repeat the follow-up examples. Construct
inputs on BinaryBackend, then copy them to each target backend before the call.

```elixir
defmodule CompareBackends do
  def run(inputs, fun) do
    for backend <- [Nx.BinaryBackend, Evision.Backend] do
      try do
        result = fun.(Enum.map(inputs, &Nx.backend_copy(&1, backend)))
        {backend, Nx.type(result), Nx.shape(result), Nx.to_flat_list(result)}
      rescue
        error -> {backend, Exception.message(error)}
      end
    end
  end
end
```

All baseline line numbers below refer to `c433cfe`; working-tree lines move as
the approved fixes are integrated.

## 1. Non-scalar multiplication chooses matrix multiplication

Severity: high. Status: fixed in working tree.

Baseline locations: `lib/evision/backend.ex:331`, especially line 339;
`lib/evision_mat.ex:1110`; native argument conversion in
`c_src/modules/evision_backend/matrix_multiply.h:25`.

```elixir
a = Nx.tensor([[1, 2], [3, 4]], type: :f32)
CompareBackends.run([a], fn [x] -> Nx.multiply(x, x) end)
```

- Expected: f32 `{2, 2}`, flat values `[1.0, 4.0, 9.0, 16.0]`.
- Observed before fix: `ArgumentError`, from `mat_matrix_multiply` receiving
  `t: nil, l: 0` through `Evision.Mat.matrix_multiply/2`.
- The same failure was reproduced for u8, s8, u16, s16, s32, s64, u64, f32, f64.
- Fixing the nil argument alone would still compute matrix multiplication,
  which violates Nx's elementwise operation. Matrix, vector, and broadcasting
  cases require the elementwise route.

Provenance: matrix routing from `ff136a8c` (2022-06-08); the call at line 339
from `7715f2ba` (2022-10-11). The optional nil wrapper dates to `3ef4def0`
(2022-10-09). These are not select SIMD regressions.

The new tests cover matrix/vector multiplication, two-dimensional and
three-dimensional broadcast, scalar order, mixed floating promotion, NaN,
infinity, signed zero, integer wrapping and values beyond 2^53, half precision,
mixed integer/float operands, names, backend identity, and vectorized axes.

## 2. Dot ignores contracting axes

Severity: high, silent wrong values. Status: fixed in working tree.

Baseline location: `lib/evision/backend.ex:271`, ignoring `_left_axes` and
`_right_axes` at lines 273-274.

```elixir
a = Nx.tensor([[1, 2], [3, 4]], type: :f32)
CompareBackends.run([a], fn [x] -> Nx.dot(x, [0], x, [0]) end)
```

- Expected: `[10.0, 14.0, 14.0, 20.0]` with shape `{2, 2}`.
- Observed before fix: `[7.0, 10.0, 15.0, 22.0]`, ordinary matrix multiplication.
- `Nx.dot([1, 2], [], [3, 4], [])` with f32 tensors also raised an OpenCV GEMM
  assertion instead of returning outer product `[3.0, 4.0, 6.0, 8.0]`.

Provenance: ignored axes from `91cd599b` (2022-10-05), native call from
`7715f2ba` (2022-10-11).

The new tests cover left and right nonstandard axes, vector dot, outer product,
batch axes, multiple contracting axes in nontrivial order, integer accumulation,
half precision, all four f32/f64 dtype pairings on conventional GEMM, names,
vectorized axes, and Evision output backend identity.

## 3. Integer add/subtract saturate, and wide integer scalar arithmetic loses precision

Severity: medium to high depending on numerical workload. Status: documented
follow-up; multiplication is now covered by the reference fallback.

Locations: `lib/evision/backend.ex:314`, `:323`, and `float_scalar/1` at baseline
line 377; `c_src/modules/evision_backend/add.h:61`, `subtract.h:59`, and
`multiply.h:59` delegate to OpenCV arithmetic.

| Operation and explicit input type | BinaryBackend | Evision before scoped fix |
| --- | --- | --- |
| u8 `[255, 128] + [1, 128]` | `[0, 0]` | `[255, 255]` |
| u8 `[0, 1] - [1, 2]` | `[255, 255]` | `[0, 0]` |
| s16 `[32767, -32768] + [1, -1]` | `[-32768, 32767]` | `[32767, -32768]` |
| u8 `[128, 255] * scalar(2)` | `[0, 254]` | `[255, 255]` |
| s64 `[9007199254740993] + scalar(0)` | `[9007199254740993]` | `[9007199254740992]` |

Example: pass `Nx.tensor([255, 128], type: :u8)` and
`Nx.tensor([1, 128], type: :u8)` to `CompareBackends.run/2` with
`fn [a, b] -> Nx.add(a, b) end`.

The project's `c_src/modules/evision_backend/arithmetic.h` already explicitly
implements wrapping addition/multiplication for reduction/window kernels.
OpenCV arithmetic has different saturation behavior, with exceptions by depth;
do not assume every integer depth saturates identically. See the
[OpenCV arithmetic reference](https://docs.opencv.org/4.13.0/d2/de8/group__core__array.html).

Potential fix: add an Nx-specific exact integer path using the existing wrapping
helpers and native integer scalar broadcasting, or use a reference fallback
first. Keep image-oriented `Evision.Mat.add/subtract` semantics unchanged.
Validate every integer width, minima/maxima, mixed types, scalar directions,
broadcasting, and 64-bit values around 2^53 and the integer limits.

The OpenCV arithmetic delegation predates the recent PRs. The current scalar
f64 routing was introduced in `25140b96` (2026-06-07, PR #322), not PR #346.

## 4. Float-to-integer casts violate truncation and non-finite rules

Severity: high for numerical conversions. Status: documented follow-up.

Locations: `lib/evision/backend.ex:218`; `c_src/modules/evision_mat.h:119`
calls `img.convertTo(ret, type)`.

```elixir
a = Nx.tensor([-2.7, -1.5, 1.5, 2.7], type: :f32)
CompareBackends.run([a], fn [x] -> Nx.as_type(x, :s32) end)
```

- Expected: `[-2, -1, 1, 2]`.
- Observed: `[-3, -2, 2, 3]`.
- For f32 `[:infinity, :nan, :neg_infinity]` to s32, BinaryBackend returns
  `[2147483647, 0, -2147483648]`; this OpenCV 5.0.0 build returned `[-1, 0, 0]`.

The [Nx 0.12.1 public source](https://github.com/elixir-nx/nx/blob/v0.12.1/nx/lib/nx.ex)
documents truncation and fixed non-finite conversions. The installed source is
at `deps/nx/lib/nx.ex:2638`; BinaryBackend implements these branches at
`deps/nx/lib/nx/binary_backend.ex:2065`.

Additional parity observations: s32 `[-1, 255, 256, 257]` to u8 produces
`[0, 255, 255, 255]` instead of BinaryBackend `[255, 255, 0, 1]`; u16
`[32767, 32768, 65535]` to s16 produces `[32767, 32767, 32767]` instead of
`[32767, -32768, -1]`. These narrowing observations are **not independently
classified as public-contract violations** because Nx documents overflow and
underflow on precision reduction as platform/compiler dependent.

Potential fix: implement Nx-specific conversion with truncation and explicit
NaN/Inf handling. Define and test narrowing parity separately. Do not change
general Mat conversion globally without considering its OpenCV API contract.
Provenance: `convertTo` dates to `86fd3316` (2022-01-23).

## 5. Mixed signed/unsigned promotion corrupts comparisons and min/max

Severity: high, silent wrong predicates and dtype. Status: documented follow-up.

Baseline locations: `lib/evision/backend.ex:480` and `:488` select signed
`max(bit_width)` instead of the Nx merge type; `:425`/`:437` replace the
requested min/max output type; `:584`/`:586` reuse the helper for comparisons.

| Operation | BinaryBackend | Evision |
| --- | --- | --- |
| `max(u8 [200], s8 [-1])` | s16 `[200]` | s8 `[127]` |
| `min(u8 [200], s8 [-1])` | s16 `[-1]` | s8 `[-1]` |
| `equal(u8 [200], s8 [127])` | u8 `[0]` | u8 `[1]` |
| `greater(u8 [200], s8 [127])` | u8 `[1]` | u8 `[0]` |

Potential fix: use `out.type` for min/max and `Nx.Type.merge(left.type,
right.type)` for comparison operands, as `cmp_scalar/5` already does. Preserve
the Nx output template instead of replacing its type from the Mat. Validate all
signed/unsigned width pairings and scalar/non-scalar parity. This is separate
from a general conversion fix: the demonstrated u8-to-s16 promotion is lossless.

Provenance: the incorrect mixed promotion rule dates to `f496cb7e`
(2022-06-09). The current min/max output-template override dates to `25140b96`
(2026-06-07, PR #322). Neither originated in PR #346.

## Integration follow-up

Independent cross-review added weak-number and integer-scalar precision cases.
The fallback now preserves the original output template through the direct
BinaryBackend callback, and common exactly representable integer scalars retain
the native floating path. The focused arithmetic file now contains twelve tests.
See the research index for final complete-suite and sanitizer results.

## Initial validation and remaining work

The approved source was loaded only inside a short-lived probe VM:

```sh
MIX_ENV=test EVISION_FETCH_PRECOMPILED=true mix run --no-compile -e \
  'Code.compile_file("lib/evision/backend.ex"); ExUnit.start(); Code.require_file("test/evision_backend_tensor_arithmetic_semantics_test.exs")'
```

Result: **8 tests passed, 0 failures**, seed 321426. The expected warning about
redefining the backend module was emitted. A prior temporary regression suite
confirmed both original bugs fail against the unmodified BEAM and the candidate
routes pass. The permanent test file retains and expands those useful cases.

Read-only differential smoke probes also matched BinaryBackend for non-leading
gather axes `[1, 2]`, indexed-add over those axes, clamped strided slicing,
NaN min/max on f32 vectors, and s64 argmax around 2^53. This was a bounded audit,
not an exhaustive proof of indexing or floating-point coverage.

Root owns the shared build and full-suite integration. Root was notified of
stale comments in `test/evision_backend_test.exs:26` and `:511` which described
the former matrix-multiplication behavior. Future native fallback replacements
should keep these differential regression tests and demonstrate exact integer
semantics before performance work.

Temporary files under `_build/research-nx-semantics` were removed after their
conclusions and useful tests were recorded here. The status inspection CLI was
checked once more and closed successfully before worker completion.
