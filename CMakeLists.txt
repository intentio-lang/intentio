cmake_minimum_required(VERSION 3.10)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules")

project(
  intentio
  VERSION 0.0.0
  LANGUAGES C
)

enable_testing()
include(CTest)

include(AddCMockaTest)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)

##############################################################################
## Packages

find_package(CMocka REQUIRED)

##############################################################################
## Compiler settings

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pedantic -Wall -Wextra -Wno-uninitialized")

##############################################################################
## Subprojects

add_subdirectory(runtime)
