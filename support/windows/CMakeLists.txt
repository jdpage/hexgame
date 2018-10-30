add_library (ntjobs SHARED
  tcl/ntjobs.c)
target_compile_definitions (ntjobs PRIVATE
  USE_TCL_STUBS
  USE_TCLOO_STUBS)
target_link_libraries (ntjobs PRIVATE ${TCL_STUB_LIBRARY})
