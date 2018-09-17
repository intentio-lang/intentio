add_library(
    intentiort STATIC

    include/intentio.h

    src/alltypes.c src/alltypes.h
    src/char.c src/char.h
    src/float.c src/float.h
    src/int.c src/int.h
    src/memory.c src/memory.h
    src/none.c src/none.h
    src/ops.c src/ops.h
    src/regex.c src/regex.h
    src/rt_meta.c src/rt_meta.h
    src/str.c src/str.h
    src/term.c src/term.h
    src/type.c src/type.h
    src/util.c src/util.h
)

configure_file(
  src/config.h.in
  ${PROJECT_BINARY_DIR}/runtime/include_priv/_intentio_config.h
)

target_include_directories(intentiort PUBLIC include)
target_include_directories(intentiort
                           PRIVATE ${PROJECT_BINARY_DIR}/runtime/include_priv)

add_subdirectory(test)
