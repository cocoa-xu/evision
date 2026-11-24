#!/usr/bin/env bash
# Pack the compiled evision, its runtime deps and the FFmpeg video tests, so the
# smoke test containers can run them against their own FFmpeg with ERL_LIBS.
#
# usage: bundle.sh OUT_TARBALL
set -euo pipefail

out="$(pwd)/$1"
lib="_build/${MIX_ENV}/lib"
stage="$(mktemp -d)"
trap 'rm -rf "${stage}"' EXIT

mkdir -p "${stage}/smoke/lib/evision" "${stage}/smoke/test/testdata"
cp -a "${lib}/evision/ebin" "${lib}/evision/priv" "${stage}/smoke/lib/evision/"
rm -rf "${stage}/smoke/lib/evision/priv/include"
for app in nx complex telemetry; do
  mkdir -p "${stage}/smoke/lib/${app}"
  cp -a "${lib}/${app}/ebin" "${stage}/smoke/lib/${app}/"
done
cp test/videocapture_test.exs test/videowriter_test.exs "${stage}/smoke/test/"
cp test/testdata/videocapture_test.mp4 "${stage}/smoke/test/testdata/"
cp .github/scripts/ffmpeg-smoke/run_tests.exs .github/scripts/ffmpeg-smoke/no_ffmpeg.exs "${stage}/smoke/"
tar -czf "${out}" -C "${stage}" smoke
ls -l "${out}"
