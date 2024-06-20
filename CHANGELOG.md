# Changelog

## v0.2.4 (2024-06-20)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.4) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.4)

### Changes
- Allow users to use named arguments.

## v0.2.3 (2024-06-17)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.3) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.3)

### Added
- Experimental support for use shared CUDA memory via `Evision.CUDA.GpuMat.from_pointer/{3,4}`.

## v0.2.2 (2024-06-15)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.2) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.2)

### Added
- Experimental support for CUDA shared memory via `Evision.CUDA.GpuMat.to_pointer/{1,2}`.
- Experimental support for Gleam. See [gleam_evision_demo](https://github.com/cocoa-xu/gleam_evision_demo) for more information.

### Changes
- General improvements to the typespecs. Enumerators now also have their own modules.
- Experimental support for using Nx.tensor directly without calling helper functions like `Evision.Mat.from_nx/1`, `Evision.Mat.from_nx_2d/1` and `Evision.Mat.last_dim_as_channel/1` .
- Support `/AOS` (arithmetic op src) marks in OpenCV source code.

  This should make evision's behaviour in line with OpenCV (C++ and Python).

  For example, in `Evision.add/2`, the expected behaviour should be different when src1/src2 are single number and they are tuple/array.

  `add(src, X)` where `X` is a number (or `Nx.tensor(X)`), it means `add(src, {X,X,X,X})`

  while `add(src, {X})` (or `add(src, Nx.tensor([X]))`) means `add(src, {X,0,0,0})`.
- Allow a single number to be passed as `Evision.Mat.maybe_mat_in()` and `Evision.scalar()`.
- Allow a n-tuple (containing n numbers) to be passed as `Evision.Mat.maybe_mat_in()` and `Evision.scalar()`.
- `Evision.Mat` now implements `Nx.LazyContainer` protocol.

### Fixes
- Fixed Erlang bindings when converting from Elixir structs to Erlang records and vice versa.

## v0.2.1 (2024-06-04)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.1) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.1)

### Fixes
- Fixed function specs where type hints for sub-classes were using the type of their parent-classes.

## v0.2.0 (2024-06-03)
[Browse the Repository](https://github.com/cocoa-xu/evision/tree/v0.2.0) | [Released Assets](https://github.com/cocoa-xu/evision/releases/tag/v0.2.0)

### Changes
- Uses OpenCV 4.10.0. Some APIs may have changed, please see OpenCV's release note for more information.

Change logs for `v0.1.x` can be found in the [CHANGELOG.v0.1.md](https://github.com/cocoa-xu/evision/blob/main/CHANGELOG.v0.1.md)
