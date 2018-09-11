# Just is awesome: https://github.com/casey/just

# Build everything
build : build-c build-haskell

# Build C code
build-c :
  cd cmake-build-vsc && cmake --build .

# Build Haskell code
build-haskell :
  stack build

# Test everything
test : test-c test-haskell

# Test C code
test-c :
  @echo "No tests for C code yet!"

# Test Haskell code
test-haskell :
  stack test

# Watch for changes in Haskell code
watch-haskell CMD='test' :
  stack {{ CMD }} --file-watch

# Watch for changes in C code
watch-c CMD='cmake --build .' :
  #!/usr/bin/env bash
  while :; do
    rg --files | entr -d bash -c 'cd cmake-build-vsc && {{ CMD }}'
  done

# Run all linters
lint : hlint

# Run HLint over Haskell codebase
hlint :
	hlint -gj --cross

# Format both Haskell and C code
fmt : brittany clang-format

# Format Haskell code
brittany :
	rg --files | rg .hs$ | xargs brittany --write-mode inplace --output-on-errors

# Format C code
clang-format :
  rg --files | rg '(h|hpp|c|cpp)$' | xargs clang-format -i

# (Re)create CMake development directory
init-cmake :
  rm -rf cmake-build-vsc
  mkdir -p cmake-build-vsc
  cd cmake-build-vsc && cmake ..

# Clean all build artifacts
clean : clean-stack clean-cmake clean-intentio

# Clean Haskell Stack work directory
clean-stack :
  rm -rf .stack-work

# Clean development CMake work directory
clean-cmake :
  rm -rf cmake-build-vsc

# Clean Intentio work directory
clean-intentio :
  rm -rf .intentio-work
