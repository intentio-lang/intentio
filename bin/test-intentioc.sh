#!/usr/bin/env bash

SCRIPT_PATH=$(realpath $0)
BIN_DIR=$(dirname $SCRIPT_PATH)
PROJ_DIR=$(dirname $BIN_DIR)

# set -x

stack exec intentioc -- \
  --modulepath-override "${PROJ_DIR}/std:." \
  -I "${PROJ_DIR}/runtime/include" \
  -L "${PROJ_DIR}/cmake-build-vsc/runtime" \
  $@
