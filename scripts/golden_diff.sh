#!/usr/bin/env bash
# Regression harness for the py_src refactor.
#
# Replays gen2.py with the exact 4.11.0 build config that produced the
# committed lib-4.11.0/ and src-4.11.0/ baselines, then diffs the result.
#
# Generates into .golden/ (staging) rather than in-tree lib/generated and
# src/generated so the working tree stays clean during refactor work.
#
# First-run bootstraps the baselines from the staging output if they are
# empty (Erlang baseline was missing because the user originally only
# generated Elixir).
#
# Empty diff output = byte-equivalent = refactor step is safe.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

# Drop leaked pkg-config from a gstreamer-using sibling repo.
unset PKG_CONFIG_PATH

# Canonical 4.11.0 modules list, copied from
# _build/test/lib/evision/cmake_evision/CMakeCache.txt at the time the
# Elixir baseline was generated.
MODULES="calib3d,core,dnn,features2d,flann,highgui,imgcodecs,imgproc,ml,photo,stitching,ts,video,videoio,aruco,barcode,bgsegm,bioinspired,dnn_superres,face,hfs,img_hash,line_descriptor,mcc,plot,quality,rapid,reg,rgbd,saliency,shape,stereo,structured_light,surface_matching,text,tracking,wechat_qrcode,xfeatures2d,ximgproc,xphoto"

LANG_FLAG="${EVISION_LANG:-elixir,erlang}"
STAGING="${EVISION_GOLDEN_STAGING:-.golden}"

# Staging c_src mirrors in-tree c_src (modules/ feeds the // @evision scan).
# Using rsync so subsequent runs only sync diffs.
mkdir -p "$STAGING/c_src" "$STAGING/lib/generated" "$STAGING/src/generated" "$STAGING/gleam_src"
rsync -a --delete c_src/ "$STAGING/c_src/"

echo ">> running gen2.py (lang=$LANG_FLAG) -> $STAGING/"
python3 py_src/gen2.py \
  --c_src="$STAGING/c_src" \
  --elixir_gen="$STAGING/lib/generated" \
  --erlang_gen="$STAGING/src/generated" \
  --gleam_gen="$STAGING/gleam_src" \
  --headers=c_src/headers-contrib.txt \
  --lang="$LANG_FLAG" \
  --modules="$MODULES"

bootstrap_dir() {
  local src=$1 dst=$2
  if [ ! -d "$dst" ] || [ -z "$(ls -A "$dst" 2>/dev/null || true)" ]; then
    echo ">> bootstrap $dst <- $src"
    mkdir -p "$dst"
    cp -a "$src"/. "$dst"/
  fi
}
bootstrap_dir "$STAGING/lib/generated" lib-4.11.0/generated
bootstrap_dir "$STAGING/src/generated" src-4.11.0/generated

elixir_diff=$(diff -rq "$STAGING/lib/generated" lib-4.11.0/generated 2>&1 || true)
erlang_diff=$(diff -rq "$STAGING/src/generated" src-4.11.0/generated 2>&1 || true)

echo "=== Elixir diff (current vs lib-4.11.0/generated) ==="
if [ -z "$elixir_diff" ]; then
  echo "  (byte-equivalent)"
else
  echo "$elixir_diff"
fi

echo "=== Erlang diff (current vs src-4.11.0/generated) ==="
if [ -z "$erlang_diff" ]; then
  echo "  (byte-equivalent)"
else
  echo "$erlang_diff"
fi

if [ -n "$elixir_diff" ] || [ -n "$erlang_diff" ]; then
  exit 1
fi
