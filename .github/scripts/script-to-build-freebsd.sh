#!/bin/bash

set -xe

rm -rf lib/generated && rm -rf src/generated
echo "PKG_NAME: ${PKG_NAME}"
mkdir -p ${PKG_NAME}
export PRIV_DIR="${ROOT_DIR}/_build/prod/lib/evision/priv"

mix compile

mv "${PRIV_DIR}/include" /tmp/include
cp -a "${PRIV_DIR}" "${PKG_NAME}"
cp -a lib/generated "${PKG_NAME}/elixir_generated"
cp -a src/generated "${PKG_NAME}/erlang_generated"
tar -czf "${PKG_NAME}.tar.gz" "${PKG_NAME}"
rm -rf "${PKG_NAME}"
ls -lah "${PKG_NAME}.tar.gz"
mv "${PKG_NAME}.tar.gz" "artifacts"
cd artifacts
sha256sum "${PKG_NAME}.tar.gz" | tee "${PKG_NAME}.tar.gz.sha256"
mv "${PKG_NAME}.tar.gz.sha256" ${ROOT_DIR}
mv "${PKG_NAME}.tar.gz" ${ROOT_DIR}
cd ${ROOT_DIR}
