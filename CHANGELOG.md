# Changelog

## Unreleased (v1.0.0)

evision now targets **OpenCV 5.0.0**, released as the first major version.
OpenCV 5.0 is a breaking upgrade (C++17, the legacy C API removed, modules
reorganised, new core element types, and a rewritten DNN engine); see OpenCV's
release notes for the upstream details.

### Added

- **`cv::Mat` as a complete Nx backend ([#48](https://github.com/cocoa-xu/evision/issues/48)).**
  OpenCV 5.0's new native depths are mapped to Nx dtypes: `CV_64S` to `{:s, 64}`,
  `CV_32U` to `{:u, 32}`, `CV_64U` to `{:u, 64}`, and `CV_16BF` to `{:bf, 16}`.
  64-bit values now round-trip through `cv::Mat` losslessly (the old s64-to-s32
  downcast that truncated values above 2^31 is gone), and `Evision.Mat.at/2`
  returns full-width 64-bit values.
- Haar/HOG parity: `Evision.CascadeClassifier` and `Evision.HOGDescriptor` build
  again via the contrib `xobjdetect` module, where OpenCV 5.0 moved them.

### Changed

- Uses OpenCV 5.0.0.
- Module reorganisation follows OpenCV 5.0. Most classes keep their `Evision.*`
  names, but a few feature detectors moved to the contrib `xfeatures2d` module
  and are now under `Evision.XFeatures2D.*`: `AKAZE`, `KAZE`,
  `AgastFeatureDetector`, and `BRISK`.
- Multi-channel `raw_type` codes changed. OpenCV 5.0 bumped `CV_CN_SHIFT` from 3
  to 5, so a multi-channel `Mat`'s integer type code differs (for example
  `cv_8UC3` is now 64, was 16). `Evision.Constant.cv_8UC3/0` and friends compute
  the correct 5.0 values; code that hardcoded these numbers must be updated.
- `Evision.VideoWriter.write/2` now returns a boolean. OpenCV 5.0 changed
  `cv::VideoWriter::write` to return a success flag instead of `void`, so the
  call no longer returns the writer; write to the same writer handle in a loop.

### Removed

- OpenCV 4.x support and the multi-version selection mechanism. evision targets
  OpenCV 5.0.x only.
- The DNN Darknet and Caffe importers (removed upstream in 5.0). Use
  `Evision.DNN.readNetFromONNX/1` or ONNX-converted models instead.
- The DNN Halide backend (removed upstream in 5.0).

### Fixed

- Building from source no longer re-runs the OpenCV install on every
  `mix compile`; the cmake-config gate now matches OpenCV 5.0's install path.

Change logs for `v0.2.x` are in
[CHANGELOG.v0.2.md](https://github.com/cocoa-xu/evision/blob/main/CHANGELOG.v0.2.md);
`v0.1.x` is in
[CHANGELOG.v0.1.md](https://github.com/cocoa-xu/evision/blob/main/CHANGELOG.v0.1.md).
