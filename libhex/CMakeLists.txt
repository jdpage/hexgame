add_library (hex SHARED
  src/board.c
  src/serialize.c
  src/tcl.c
  src/winner.c)

target_compile_definitions (hex PRIVATE
  USE_TCL_STUBS
  USE_TCLOO_STUBS)

target_link_libraries (hex PRIVATE ${TCL_STUB_LIBRARY})

target_include_directories (hex PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
  PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  ${TCL_INCLUDE_PATH})

install (TARGETS hex
  LIBRARY DESTINATION ${HEX_DATADIR}/hextk/lib/hex
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

if (NOT WIN32)
  install (FILES pkgIndex.tcl DESTINATION ${HEX_DATADIR}/hextk/lib/hex)
else ()
  install (FILES pkgIndex.tcl DESTINATION ${CMAKE_INSTALL_BINDIR})
endif ()
