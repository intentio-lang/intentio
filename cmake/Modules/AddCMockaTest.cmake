function(ADD_CMOCKA_TEST test_name)
  cmake_parse_arguments(ADD_CMOCKA_TEST "" "" "WRAP" ${ARGN})

  foreach(func IN LISTS ADD_CMOCKA_TEST_WRAP)
    list(APPEND WRAP_ARGS "-Wl,--wrap=${func}")
  endforeach(func)

  if (BUILD_TESTING)
    set(${test_name}_TARGET_NAME test-${test_name})
    add_executable(${${test_name}_TARGET_NAME} ${ADD_CMOCKA_TEST_UNPARSED_ARGUMENTS})
    target_include_directories(${${test_name}_TARGET_NAME} PRIVATE ${CMOCKA_INCLUDE_DIR})
    target_link_libraries(${${test_name}_TARGET_NAME} PRIVATE ${CMOCKA_LIBRARY} ${WRAP_ARGS})
    add_test(
      NAME ${test_name}
      COMMAND $<TARGET_FILE:${${test_name}_TARGET_NAME}>
      WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
    )
  endif (BUILD_TESTING)
endfunction(ADD_CMOCKA_TEST)
