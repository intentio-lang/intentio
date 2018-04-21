#!/usr/bin/env python3

# Based on Rust Compiler x.py
# https://github.com/rust-lang/rust/blob/04e4d426a169a26d498bf22d2c2d01bc7b14fbcd/x.py

# This file is only a "symlink" to build.py, all logic should go there.

import os
import sys

intentio_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(os.path.join(intentio_dir, "src", "build-tool"))

import build
build.main()
