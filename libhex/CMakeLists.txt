add_library (hex SHARED
  src/board.c
  src/tcl.c
  src/winner.c)

target_compile_definitions (hex PRIVATE
  USE_TCL_STUBS
  USE_TCLOO_STUBS)

target_link_libraries (hex ${TCL_STUB_LIBRARY})

target_include_directories (hex PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
  PRIVATE src)
