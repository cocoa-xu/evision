#!/usr/bin/env bash
# Run after a build: the Hex package must hold every tracked file except those
# listed in not_shipped, and nothing git doesn't track except the checksum files
# written at publish time. Build outputs next to the sources must stay out.
set -euo pipefail

not_shipped='^(\.github/|\.githooks/|\.gitconfig$|\.gitignore$|\.gitlab-ci\.yml$|assets/|benchmarks/|config/|examples/|test/|py_src/tests/|Cheatsheet\.cheatmd$|do_release\.sh$|mix\.lock$|rebar\.lock$)'
publish_time='^checksum\.(exs|erl)$'

work="$(mktemp -d)"
placeholders=""
trap 'rm -rf "${work}"; [ -z "${placeholders}" ] || rm -f ${placeholders}' EXIT

# hex.build stops when a listed file is missing, and these only exist at publish time.
for f in checksum.exs checksum.erl; do
  if [ ! -e "${f}" ]; then
    : > "${f}"
    placeholders="${placeholders} ${f}"
  fi
done

mix hex.build --unpack -o "${work}/package" > /dev/null
(cd "${work}/package" && find . -type f ! -name hex_metadata.config | sed 's|^\./||') | LC_ALL=C sort > "${work}/packaged"
git ls-files | LC_ALL=C sort > "${work}/tracked"

missing="$(grep -Ev "${not_shipped}" "${work}/tracked" | LC_ALL=C comm -23 - "${work}/packaged")"
extra="$(grep -Ev "${publish_time}" "${work}/packaged" | LC_ALL=C comm -23 - "${work}/tracked")"

if [ -n "${missing}" ]; then
  echo "Tracked but not in the Hex package (add it to package files in mix.exs, or to not_shipped in $0):"
  echo "${missing}"
fi
if [ -n "${extra}" ]; then
  echo "In the Hex package but not tracked (add it to exclude_patterns in mix.exs):"
  echo "${extra}"
fi
[ -z "${missing}${extra}" ] || exit 1
echo "The Hex package holds the $(wc -l < "${work}/packaged" | tr -d ' ') shipped files and nothing else."
