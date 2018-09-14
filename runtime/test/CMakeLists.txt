function(add_intentiort_test test_name)
  add_cmocka_test(${test_name} ${ARGN})

  if(BUILD_TESTING)
    target_include_directories(test_${test_name} PRIVATE ${CMAKE_SOURCE_DIR})
    target_link_libraries(test_${test_name} PRIVATE intentiort)
  endif(BUILD_TESTING)
endfunction(add_intentiort_test)


add_intentiort_test(rt_hello hello.c)
