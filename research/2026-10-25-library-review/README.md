# Library improvement research

Started 2026-10-25 from `c433cfe` on `research/library-improvements`.

## Objective

Find and validate substantial correctness and performance improvements beyond the select dispatch work in PR #346. Correctness takes priority. Performance claims require measurements and explicit workload assumptions. Counterintuitive implementation tradeoffs should be documented beside the code when adopted.

## Workstreams

| Owner | Scope | Report |
| --- | --- | --- |
| `nx_semantics` | Nx differential behavior, types, shapes, indices, numerical edges | [Nx semantics](nx-semantics.md) |
| `native_safety` | Native extents, ownership, strides, concurrency and boundary validation | [Native safety](native-safety.md) |
| `backend_performance` | Materialization, broadcasting, reductions and matrix multiplication | [Backend performance](backend-performance.md) |
| Root | Integration, independent benchmark references and prioritization | This index and [benchmark oracle](benchmark-oracle.md) |

The three research agents use `gpt-6-astra` with `xhigh` reasoning. Initial work is an evidence-gathering pass. Coordinate edits and shared NIF rebuilds through root; each agent owns only its report and isolated temporary probes.

## Baseline

- PR #346 remains open and separate from this research branch.
- Local macOS arm64 suite at the starting commit: 277 passed, 37 default exclusions, in auto and baseline modes.
- CI at the starting commit passes all executed checks, including Linux/macOS/Windows native dispatch, Linux/macOS ASan/UBSan, and big-endian s390x under QEMU.
- Existing select memory and branch-policy reviews are recorded in PR #346. Avoid repeating those experiments without new evidence.

## Checkpoint policy

Agents verify their intended profile and inspect live CLI `/status` before substantial work and at research checkpoints. The requested stop condition is weekly remaining allowance returning to 100%, not context remaining or quota consumed. On that condition, stop new work and record completed findings, tests, pending operations and next steps here. Profile identity and current quota snapshots belong in the respective profile context, not in project research reports.

## Findings and next actions

The first audit pass produced these scoped changes:

| Change | Evidence and scope |
| --- | --- |
| Elementwise multiplication | Non-scalars previously called matrix multiplication and raised. Floating operands now use typed elementwise multiplication; integer and unproven combinations use an exact reference fallback. |
| Dot axes and batching | Explicit axes were ignored. Keep the conventional two-dimensional floating GEMM path; generalized contractions and other types use a correctness fallback. |
| Moments results | The native converter returned the empty initial map rather than the map containing all 24 calculated values. |
| Channel reinterpretation | Replace an outdated depth whitelist with the actual single-channel condition, covering all 12 supported scalar types and ROI restoration. |
| Redundant casts | Preserve the existing Mat resource for an internal same-type conversion. Existing writable and alignment-protecting converter behavior remains in effect. |
| Benchmark oracle | Always construct and compare reference results through BinaryBackend, even when the caller configured another default. |

The same-type cast change was measured with complete eager Nx calls and output
materialization. At 1,048,576 elements on Apple M6, cv-owned input medians changed
from 174.29 to 119.21 us for sqrt (1.46x), 151.92 to 89.38 us for rsqrt (1.70x),
195.44 to 141.48 us for integer shift (1.38x), and 214.46 to 121.42 us for
put_slice including its source slice (1.77x). Binary-backed sqrt was nearly
unchanged (1.02x). See the performance report for methodology, both storage
types, smaller sizes, sample ranges and the complete reproduction harness.

## Integration validation

- Twenty new ExUnit tests cover the selected defects and cast-elision risks.
  Multiply also joins the pre-existing broadcast shape matrix.
- The rebuilt normal NIF passes the complete local suite in auto and baseline
  modes: 297 passed, 37 default exclusions in each mode.
- A separately built NIF and CPU-kernel library instrumented with ASan/UBSan
  passes the same complete suite in both modes: 297 passed, 37 exclusions each.
  Both sanitizers stop on the first finding. Leak detection is disabled, and
  the existing BEAM and OpenCV binaries are not instrumented.
- On macOS, the ASan runtime is preloaded into `beam.smp` directly before the
  NIF is loaded. The normal NIF is restored afterward and its SHA-256 verified.
- The benchmark detection control rejected a known wrong integer-add result
  under both BinaryBackend and Evision caller defaults after the oracle fix.
- The newly added u32 multiplication case passes the focused eight-test suite.
- Windows CI now explicitly runs all five new regression files alongside the
  existing conversion and dispatch tests. Remote CI results are recorded in
  the follow-up pull request.

## Prioritized follow-up findings

These remain existing library limitations; the first pass does not establish
full Nx conformance:

1. Mixed signed/unsigned promotion can silently corrupt min/max and comparisons.
2. Float-to-integer conversion rounds instead of truncating and mishandles
   non-finite values relative to Nx's documented conversion contract.
3. Integer add/subtract can saturate, and converting large integer scalars to
   f64 can lose precision above 2^53.
4. Half-float aggregate max/min paths need explicit supported-depth handling.
5. Contiguous broadcast suffixes and scans along non-trailing axes may avoid
   copies or index work, but these opportunities are not yet measured.

The workstream reports distinguish public reproductions from dormant native
converter defects and unmeasured performance hypotheses. The generalized
multiply/dot reference paths deliberately prioritize correct results and may
be slow for large integer or generalized contractions. Future native kernels
must reproduce those semantics before replacing the fallbacks.
