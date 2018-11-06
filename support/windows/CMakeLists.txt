add_library (ntapi SHARED
  tcl/nt.c)
target_compile_definitions (ntapi PRIVATE
  USE_TCL_STUBS
  USE_TCLOO_STUBS
  USE_TK_STUBS)
target_link_libraries (ntapi PRIVATE
  ${TCL_STUB_LIBRARY}
  ${TK_STUB_LIBRARY})
