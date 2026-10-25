# Integer promotion and scalar semantics follow-up

Date: 2026-10-25. This is a read-only follow-up to the first audit and batch
`ad7bb2b`. No min/max/comparison production change is implemented by this report.
The backend performance worker separately owns the weak-number multiplication
correction described below. Existing committed reports were not modified.

## Recommendation

The next bounded repair should cover integer pairs in `min`, `max`, and all six
comparison predicates. Use lossless native integer promotion and typed tensor
broadcasting where possible, with a reference fallback whenever u64 is paired
with a signed type. Preserve the callback's `out` template in every path.

Do not replace the entire promotion helper with an unconditional
`Nx.Type.merge/2` cast. That fixes narrower mixed integers but silently gives
wrong comparisons for u64/signed, mixed integer/float, and some f16/bf16 inputs.

Candidate simulations passed **1,280 mixed-integer cases**, comprising 960
native cases and 320 reference-fallback cases, plus **176 raw-number scalar
cases**. These results validate the proposed gates, not an implemented patch.

## Call graph and relevant contracts

`lib/evision/backend.ex` has exactly two consumers of `enforce_same_type/2`:

| Consumer | Public operations | Current behavior |
| --- | --- | --- |
| `min_max/5` | non-scalar `min/3`, `max/3` callbacks | Broadcasts both operands, applies the helper, replaces `out.type` with the resulting Mat type |
| `cmp/5` non-scalar clause | `equal`, `not_equal`, `greater`, `less`, `greater_equal`, `less_equal` | Broadcasts both operands, applies the helper, invokes `Evision.Mat.cmp/3` |

Scalar operands bypass that helper. `min_max_scalar/5` casts only the array to
`out.type` and passes the scalar as a BEAM number to generated `Evision.min/max`.
`cmp_scalar/5` already casts both operands to `Nx.Type.merge/2`, then lets
OpenCV interpret the one-element operand as a scalar. These paths must be
included in regression coverage.

Related helpers have broader consumers and should not be changed globally:

- `cast_mat/2`: conventional dot, scalar min/max, scalar comparisons, scalar and
  non-scalar select, and concatenate.
- `enforce_same_shape/3`: arithmetic alignment, non-scalar comparisons, and
  `same_shape_unless_scalar/3` used by shift and binary math.
- `to_number/1`: clip bounds, scalar min/max, slice start clamping, and window
  scatter initialization.

The pinned Nx source establishes three different concepts:

1. `deps/nx/lib/nx/type.ex:467` merges unsigned/signed types as signed
   `max(min(unsigned_bits * 2, 64), signed_bits)`. Thus u8+s8 becomes s16,
   u16+s16 becomes s32, u32+s32 becomes s64, and **u64+signed becomes s64,
   not a floating type**.
2. `deps/nx/lib/nx.ex:5546` resolves arithmetic output type before converting raw
   numbers to tensors. `out.type` can therefore differ from the merge of the
   already tensorized callback arguments.
3. `deps/nx/lib/nx.ex:5570` sends comparison callbacks a u8 output template and
   the original operand types. `deps/nx/lib/nx/binary_backend.ex:671` reads both
   original types, compares their values, then encodes the result. It does not
   first coerce both operands to the merge type. The existing scalar-comparison
   comment suggesting that Nx always compares in the merged type is inaccurate
   for BinaryBackend.

## Exact counterexamples

For a minimal comparison, construct the indicated inputs on BinaryBackend,
compute `apply(Nx, operation, [left, right])`, then copy each tensor with
`Nx.backend_copy(tensor, Evision.Backend)` and repeat. Compare type, shape, and
`Nx.to_flat_list/1`. A scalar below means a shape-`{}` tensor unless marked raw.

| Inputs and operation | BinaryBackend | Existing Evision | Merge/out.type plus full broadcast candidate |
| --- | --- | --- | --- |
| max(u8 `[255]`, s8 `[127]`) | s16 `[255]` | s8 `[127]` | s16 `[255]` |
| equal(u16 `[65535]`, s16 `[32767]`) | u8 `[0]` | u8 `[1]` | u8 `[0]` |
| max(u32 `[4294967295]`, s32 `[2147483647]`) | s64 `[4294967295]` | s32 `[2147483647]` | s64 `[4294967295]` |
| min(u64 `[18446744073709551615]`, s64 `[1]`) | s64 `[1]` | s64 `[-1]` | s64 `[-1]`, still wrong |
| max(the same u64/s64 inputs) | s64 `[-1]` | s64 `[1]` | s64 `[1]`, still wrong |
| greater(the same u64/s64 inputs) | u8 `[1]` | u8 `[0]` | u8 `[0]`, still wrong |

The u64 maximum is larger than every s64 value. BinaryBackend selects in that
original domain and only then encodes the selected value as s64. Converting the
u64 operand to s64 before selecting changes ordering. A floating intermediate
would also lose exactness and is not a solution.

Scalar handling has an independent precision problem:

```elixir
a = Nx.tensor(9_007_199_254_740_993, type: :s64)
b = Nx.tensor([9_007_199_254_740_992, 9_007_199_254_740_993], type: :s64)
Nx.equal(a, b)
```

BinaryBackend returns `[0, 1]`; the current Evision scalar path returns `[1, 0]`.
Materializing the scalar as a matching typed tensor before native comparison
returns `[0, 1]`. Native min/max with this scalar currently raises
`ArgumentError`; typed full broadcast succeeds and preserves both 64-bit values.
The same broadcast approach passed u64 extrema without passing through a double.

Lossy noninteger promotion is a separate follow-up:

| Comparison | BinaryBackend | Existing Evision | Unconditional merge candidate |
| --- | --- | --- | --- |
| equal(s32 `[16777217]`, f32 `[16777216.0]`) | `[0]` | `[1]` | `[1]` |
| equal(s64 `[9007199254740993]`, f64 `[9007199254740992.0]`) | `[0]` | `[1]` | `[1]` |
| equal(f16 `[Inf, 0]`, bf16 `[1.0e30, 1.0e-9]`) | `[0, 0]` | missing helper clause | `[1, 1]` |

For future comparison coverage, f64 can represent all 32-bit integers and f32
values exactly; it cannot represent the full 64-bit integer domain. f32 can
represent both f16 and bf16 values exactly, while f16 cannot represent bf16's
range. These facts can define future work types, but are outside the proposed
mixed-integer repair. Using the arithmetic output type as a comparison work
type is not generally valid.

## Proposed implementation gates

Apply the integer gate before the existing scalar clauses so both scalar and
non-scalar inputs take the same correctness path.

1. Identify integer pairs by both kinds being `:s` or `:u`.
2. If one operand is u64 and the other is signed, use the reference callback
   directly, preserving the original output template:

   ```elixir
   apply(Nx.BinaryBackend, operation, [
     out,
     Nx.backend_copy(left, Nx.BinaryBackend),
     Nx.backend_copy(right, Nx.BinaryBackend)
   ])
   |> Nx.backend_transfer(Evision.Backend)
   ```

3. For other integer pairs, use `out.type` as min/max work type and
   `Nx.Type.merge(left.type, right.type)` as predicate work type. The latter is
   an exact common integer representation in this guarded domain. A predicate's
   u8 `out.type` is only its result type.
4. Cast and broadcast both operands into equal-size typed Mats, including
   shape-`{}` scalars. For scalar outputs, the Mat can have shape `{1}` while
   `to_nx(mat, out)` retains the scalar Nx template. Do not send 64-bit values
   through a BEAM-number or OpenCV scalar overload.
5. Invoke native Mat/Mat min/max or comparison, reject native errors, and return
   `to_nx(mat, out)`. Do not overwrite the requested output type from the Mat.
6. Leave unrelated float/half handling unchanged in this bounded patch. A later
   cleanup can remove or generalize the remaining legacy helper after its
   precision domain has explicit coverage.

The simple integer implementation materializes a scalar broadcast. This costs
an allocation compared with the current scalar fast path, but avoids the
confirmed 64-bit precision loss. A later native typed-scalar kernel can remove
that allocation while preserving integer reads. Mixed u64/signed fallback
also introduces host copies and reference-backend execution.

## Weak-number output types and the parallel multiplication correction

Raw numbers and explicitly typed scalar tensors are not interchangeable:

| Expression | Raw-number output | Same value passed as explicit s32 scalar |
| --- | --- | --- |
| max(u8 `[200]`, `300`) | u16 `[300]` | s32 `[300]` |
| min(s8 `[-100]`, `200`) | s16 `[-100]` | s32 `[-100]` |

The original `out` template is authoritative. A public `Nx.min/max` call inside
a fallback would recompute promotion from the tensorized scalar and can change
the dtype. For u64 arrays and a positive raw integer, such a fallback could
even change the expected u64 output to s64.

Cross-review by the backend performance worker confirmed the same issue in the
new multiplication fallback. On `71cc936`, u8 `[128, 255]` multiplied by raw `2`
returned s32 `[256, 510]` instead of u8 `[0, 254]`; s8 `[-128, 127]` returned
s32 `[-256, 254]` instead of s8 `[0, -2]`; u32 max multiplied by raw `2`
returned s64 `[8589934590]` instead of u32 `[4294967294]`. Half precision arrays
multiplied by raw `0.5` incorrectly became f32.

That worker's authorized correction calls `Nx.BinaryBackend.multiply(out,
copied_left, copied_right)` before transferring back. It reported a failing
regression before the correction and **9 passing focused tests** afterward,
including 15 raw scalar cases in both operand orders. Root owns integration
and full-suite validation. Dot was inspected separately: its public frontend
resolves type after tensorization, so the same weak-number issue was not found.

## Regression matrix and evidence

Exploratory simulation used the current no-compile build and direct calls to
existing Mat operations, without redefining production modules or writing
shared BEAM/NIF artifacts:

```sh
MIX_ENV=test EVISION_FETCH_PRECOMPILED=true mix run --no-compile probe.exs
```

The 1,280-case integer matrix covered all 16 unsigned/signed width pairings
across 8/16/32/64 bits, both operand orders, eight operations (min/max and six
predicates), and five layouts: vector/vector, vector/scalar, scalar/vector,
scalar/scalar, and two-dimensional broadcast. Values included zero, one,
unsigned maximum, negative one, and signed maximum. The u64/signed gate used
the reference route; all others used native merged/out-type Mats with broadcast.
There were zero mismatches: 960 native and 320 fallback cases.

Another 176 cases exercised 11 raw-number inputs, both orders, and the same
eight operations. The proposed native path received the actual reference
`out` template; the reference path called the BinaryBackend callback directly.
Values covered u8 narrow preservation/widening, negative unsigned promotion,
u16/u32 limits, u64 limits with positive and negative raw scalars, and s64
values around 2^53. There were zero mismatches.

A minimal permanent suite should retain these behavior groups:

- Narrow mixed-sign widths: u8/s8, u16/s16, u32/s32, plus unequal widths and
  both operand orders; assert dtype as well as values.
- Mixed u64/signed: maxima against `-1`, `1`, and signed maximum; verify min,
  max, and all predicates select before output encoding.
- Typed 64-bit scalar cases: adjacent values around 2^53 and u64 extrema;
  compare scalar/scalar, scalar/vector, and vector/scalar.
- Raw-number cases: retained narrow dtype, required widening, negative
  unsigned promotion, and positive scalar with a u64 tensor.
- Broadcasting and metadata: two-dimensional broadcast, scalar output,
  names, vectorized axes, and Evision output backend identity.
- Keep mixed integer/float and half-type counterexamples as an explicit next
  scope; do not claim they are repaired by the integer change.

Temporary probe scripts were removed after recording this evidence. Required
status checks completed; the reset stop condition was not reached. The
inspection CLI was closed successfully. No account or quota snapshot is
stored in this project report.
