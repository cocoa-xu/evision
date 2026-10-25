# Exact integer scalars in floating multiplication

Follow-up to the multiply correctness fix and its weak-number promotion correction, 2026-10-25. The first fix conservatively routed every integer/float combination through `Nx.BinaryBackend`. That includes ordinary `Nx.multiply(float_tensor, 2)`, because Nx tensorizes the raw integer as an integer scalar before invoking the backend.

## Measured regression and implemented correction

An isolated comparison with `c433cfe` confirmed a substantial regression for this common case. For 1,048,576 binary-backed elements, f32 multiplication by raw `2` changed from 337 microseconds to 35,891 microseconds; f64 changed from 206 microseconds to 30,615 microseconds. Raw `-3` showed the same problem. The fallback computes the entire multiplication through BEAM's reference loop instead of the existing native scalar kernel.

The implemented correction allows native multiplication only when:

- One operand is an f32/f64 tensor.
- The other is a shape-`{}` signed or unsigned integer tensor.
- The integer survives an exact round trip through the already-resolved output float type.

Integer arrays, half-float arrays, and non-representable scalar integers retain the reference fallback. The fallback invokes `Nx.BinaryBackend.multiply(out, left, right)` directly, preserving Nx's output template and weak-number promotion. Operand order is preserved in both paths.

The gate reads the scalar using **public `Nx.to_number/1`**, whose integer implementation reads the scalar's binary with signed/unsigned bit syntax. It deliberately does not call the similarly named helper in `Evision.Backend`, which reads through `Mat.at` and can round a wide integer through a double before checking precision.

The representability check is:

```elixir
integer = Nx.to_number(scalar)
<<value::float-native-size(^bits)>> = <<(integer * 1.0)::float-native-size(bits)>>
exact? = trunc(value) == integer
```

All supported integer inputs fit within the finite range of f32/f64, so the round trip cannot produce infinity. Comparing the recovered integer, rather than using float/integer equality, rejects values such as `2^53 + 1` that round during conversion to a double. Exactly representable larger integers, such as powers of two, remain eligible. For admitted values, an f64 scalar Mat is constructed because OpenCV's scalar operand path expects that representation; the explicit output type preserves Nx's dtype.

The production helper includes comments explaining both the precision check and the measured reason for avoiding the general fallback on common scalar operations. No shared NIF or build artifact was changed by this workstream.

## Comparative timings

Hardware/runtime: Apple M6, macOS arm64, Elixir 1.20.4, OTP 29, Nx 0.12.1, existing OpenCV 5 NIF, 12 BEAM schedulers and 12 OpenCV threads. Root granted an isolated timing window; other local research work was static.

Three variants were compiled as distinct in-memory modules:

1. Original source from `git show c433cfe:lib/evision/backend.ex`.
2. The corrected reference-only fallback, including direct `Nx.BinaryBackend.multiply(out, ...)`.
3. The candidate with the exact integer-scalar gate.

No shared BEAM files were overwritten. Each input was constructed once with `Nx.BinaryBackend`, then copied to each variant, producing binary-backed Mats. Each timed invocation was the complete `Nx.multiply(tensor, raw_integer) |> Nx.to_binary()` call. Input preparation, backend transfer, correctness checks, and explicit garbage collection were outside timing.

Each pair used three warmups per variant and 11 paired samples, alternating order AB/BA. Samples were one complete invocation each. The calling process was garbage-collected before each pair. Tables show median microseconds. Different comparison pairs have separate medians; they should not be combined as one simultaneous three-way sample.

At 1,048,576 elements:

| Operation | Original / candidate pair, us | Reference-only / candidate pair, us | Reference-only recovery |
| --- | ---: | ---: | ---: |
| f32 times `2` | 335.25 / 329.25 | 36,402.08 / 333.12 | 109x |
| f32 times `-3` | 332.21 / 330.46 | 36,335.75 / 333.79 | 109x |
| f64 times `2` | 199.33 / 202.79 | 33,012.46 / 207.96 | 159x |
| f64 times `-3` | 198.83 / 200.38 | 28,774.42 / 209.29 | 137x |

The candidate restored approximately the original throughput on these large inputs. In the original/candidate pairs, minimum-maximum sample ranges were 323.58-346.58 / 321.25-335.21 microseconds for f32 times `2`, and 191.38-208.25 / 193.96-215.67 for f64 times `2`.

At 4,096 elements:

| Operation | Original / candidate pair, us | Reference-only / candidate pair, us |
| --- | ---: | ---: |
| f32 times `2` | 11.29 / 11.88 | 134.50 / 43.88 |
| f32 times `-3` | 9.83 / 15.75 | 133.17 / 48.08 |
| f64 times `2` | 16.50 / 15.88 | 128.08 / 15.25 |
| f64 times `-3` | 10.17 / 17.88 | 126.33 / 16.12 |

The short samples show substantial VM scheduling and pair-order noise, especially the f32 candidate when paired with the slower reference loop. The large-input result is much clearer. These measurements justify restoring a validated native scalar path; they do not establish a universal scalar-call latency or cross-platform ratio.

## Correctness evidence

Before timing, each full measured output was checked against an independent binary oracle. Inputs repeat `[1.0, -2.0, 0.25, -0.5]`; expected outputs repeat the four independently computed products, encoded at the target width. The original, reference-only, and candidate variants produced those exact bytes and left their input bytes unchanged.

Additional isolated checks covered:

- Both operand orders and binary-backed/cv-owned inputs.
- Integer values `0`, `1`, `-1`, `2`, and `-3`.
- Values immediately around `2^24` and `2^53`, including representable values beyond the first precision boundary.
- Signed 64-bit minimum and unsigned 64-bit maximum.
- Positive and negative zero, infinities, quiet NaNs with distinct payloads and signs, and positive/negative subnormals.
- A deterministic sample of 4,096 generated bit patterns per float width, excluding NaN/infinity encodings and, for f64, bounding exponents to avoid the reference backend's finite-overflow exception. Thirteen scalar values around the precision boundaries were checked in both orders. Every compared finite-result binary matched BinaryBackend exactly, including signed zeros and subnormals.

The old-native exceptional-byte comparison is particularly important: for both float widths, scalars `0`, `1`, `-1`, `2`, `-3`, `2^24`, and `2^53`, and both operand orders, the candidate matched `c433cfe` output bytes exactly. Thus the gate restores existing native NaN payload/sign behavior.

**BinaryBackend arithmetic is not a universal NaN byte oracle.** It canonicalizes input NaNs to a standard positive NaN, while the native multiplication retains the input NaN payload/sign on the tested platform. Numeric classifications agreed. Signed-zero, infinity, and subnormal bytes agreed where those outputs were not NaNs. The candidate is not claimed to be byte-identical to the temporary reference-only fallback for NaN inputs.

Another reference limitation appeared during the unrestricted f64 bit-pattern probe: a finite value near the maximum double multiplied by `2` raises `ArithmeticError` in Nx 0.12.1's BinaryBackend. The native operation returns IEEE infinity. The bounded finite sample avoids using that exception path as an arithmetic oracle. This is not a newly introduced gate mismatch.

Three focused tests were added to `test/evision_backend_tensor_arithmetic_semantics_test.exs`:

1. Exact integer-scalar boundaries and both operand orders, plus an integer-array counterexample that must remain on the fallback.
2. Raw integer scalars with signed zero and nonfinite float inputs.
3. Integer identity multiplication preserving native NaN payload and subnormal bytes.

Together with the prior tests, all **12 arithmetic regression tests passed** using process-local compilation of the current backend source and the existing NIF. Root owns full-suite and sanitizer validation after integration. No production benchmark was run with a partially rebuilt shared NIF.

## Reproduction details

The timing input and independent oracle for an element count `n`, float width `width`, and raw integer `scalar` are:

```elixir
raw = :binary.copy(
  <<1.0::float-native-size(width), -2.0::float-native-size(width),
    0.25::float-native-size(width), -0.5::float-native-size(width)>>,
  div(n, 4)
)

expected = :binary.copy(
  <<(1.0 * scalar)::float-native-size(width),
    (-2.0 * scalar)::float-native-size(width),
    (0.25 * scalar)::float-native-size(width),
    (-0.5 * scalar)::float-native-size(width)>>,
  div(n, 4)
)
```

Use `{n, scalar}` from `[4_096, 1_048_576]` crossed with `[2, -3]`, for f32 and f64. Rename every `Evision.Backend` occurrence in a source string to a unique module name before `Code.compile_string/1`. The reference-only variant is the candidate source with its two integer-scalar clauses and `multiply_integer_scalar/4` removed, and `multiply_reference/3` restored as the public fallback clause. This leaves its weak-number fix intact.

For each pair, validate both full output binaries and input immutability, warm each variant three times, then take 11 samples while alternating variant order. Call `:erlang.garbage_collect()` before each pair, and use `System.monotonic_time(:nanosecond)` directly around the complete Nx operation and binary materialization. Divide elapsed nanoseconds by 1,000 and report the middle sorted sample. This procedure matches the tables above.

Required live quota checkpoints were performed and the requested stop condition was not reached. Profile-specific snapshots are not stored here. Scratch cleanup and closure of the status-only CLI are completed at handoff; no branch changes, commits, pushes, or shared builds were performed by this workstream.
