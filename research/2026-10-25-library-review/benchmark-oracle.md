# Independent benchmark references

Baseline: `c433cfe`. The benchmark task named its input `binary_input` but created
it through the caller's default Nx backend. Configuring Evision as that default
therefore made both the implementation and its expected result use Evision.
The verification could report success for a shared incorrect result.

## Reproduction and detection control

A temporary, process-local copy of the task exposed `run_case/5` for a custom
case without changing the installed module or shared build. The case added u8
`[255]` and `[1]`, where the existing Evision add path returns `[255]` and
BinaryBackend returns `[0]`.

| Caller default | Before | After |
| --- | --- | --- |
| Nx.BinaryBackend | Verification rejects the incorrect result | Verification rejects it |
| Evision.Backend | Verification incorrectly reports `passed` | Verification rejects it |

The custom case is a detection control for the benchmark's reference selection.
The underlying integer addition defect is separately recorded in
[Nx semantics](nx-semantics.md) and is not fixed by this harness change.

## Change and regression

Input construction and expected-result computation now run under
`Nx.with_default_backend(Nx.BinaryBackend, ...)`. Verification also establishes
that default because `Nx.all_close` creates scalar tolerance tensors. The
caller's process default is restored, and the report records
`reference_backend: Nx.BinaryBackend`.

`test/evision_backend_benchmark_test.exs` invokes the public task with a caller
backend that deliberately rejects input creation. Before the fix it fails in
that backend's `iota/3`; after the fix the smoke benchmark passes against the
independent reference and the caller's original backend remains selected.

Temporary probe source and output are removed after integration validation.

## Historical comparison caveat

With an Evision caller default, the old task could create OpenCV-owned inputs
and then clone them with a same-backend copy. The corrected task creates its
inputs on BinaryBackend before copying them to Evision. This can change the
input ownership and subsequent safety-copy costs, independently of any kernel
change. Compare historical reports only after controlling input preparation
and storage. The separate cast-elision experiment explicitly holds storage
constant and reports binary-backed and OpenCV-owned cases separately.
