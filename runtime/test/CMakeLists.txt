function(ADD_INTENTIORT_TEST test_file)
  GET_FILENAME_COMPONENT(test_name ${test_file} NAME_WE)
  SET(test_name runtime-${test_name})
  add_cmocka_test(${test_name} ${test_file} ${ARGN})

  if(BUILD_TESTING)
    target_include_directories(test-${test_name} PRIVATE ${CMAKE_SOURCE_DIR})
    target_link_libraries(test-${test_name} PRIVATE intentiort)
  endif(BUILD_TESTING)
endfunction(ADD_INTENTIORT_TEST)


add_intentiort_test(int.c)
add_intentiort_test(none.c)
add_intentiort_test(str.c)
add_intentiort_test(term.c WRAP ieo_term_deleter)
add_intentiort_test(type.c)
add_intentiort_test(typeconv.c)
