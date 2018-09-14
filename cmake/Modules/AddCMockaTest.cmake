function(add_cmocka_test test_name)
  if (BUILD_TESTING)
    set(${test_name}_TARGET_NAME test_${test_name})
    add_executable(${${test_name}_TARGET_NAME} ${ARGN})
    target_include_directories(${${test_name}_TARGET_NAME} PRIVATE ${CMOCKA_INCLUDE_DIR})
    target_link_libraries(${${test_name}_TARGET_NAME} PRIVATE ${CMOCKA_LIBRARY})
    add_test(
      NAME ${test_name}
      COMMAND $<TARGET_FILE:${${test_name}_TARGET_NAME}>
      WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
    )
  endif (BUILD_TESTING)
endfunction(add_cmocka_test)
