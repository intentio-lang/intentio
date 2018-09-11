cmake_minimum_required(VERSION 3.10)
project(
  intentio
  VERSION 0.0.0
  LANGUAGES C
)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)

##############################################################################
## Compiler settings

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pedantic -Wall -Wextra -Wno-uninitialized")

##############################################################################
## Subprojects

add_subdirectory(runtime)
