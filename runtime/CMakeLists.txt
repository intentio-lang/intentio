add_library(
    intentiort STATIC

    # Intentio Runtime API headers
    include/intentio.h

    # Implementations of Runtime API routines
    api/none.c
    api/num.c
    api/ops.c
    api/regex.c
    api/rt_meta.c
    api/str.c

    # Private code
    core/attrs.h
    core/memory.c core/memory.h
    core/term.c core/term.h
    core/type.c core/type.h
)

configure_file(
  core/config.h.in
  ${PROJECT_BINARY_DIR}/runtime/include_priv/_intentio_config.h
)

target_include_directories(intentiort PUBLIC include)
target_include_directories(intentiort
                           PRIVATE ${PROJECT_BINARY_DIR}/runtime/include_priv)

add_subdirectory(test)
