# Object library for common files
add_library (hex_objects OBJECT
  src/board.c
  src/serialize.c
  src/winner.c)
target_include_directories (hex_objects
  PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  )
if (NOT WIN32)
  set_property(TARGET hex_objects PROPERTY POSITION_INDEPENDENT_CODE ON)
endif ()

# Static library (no Tcl)
add_library (hex_static STATIC $<TARGET_OBJECTS:hex_objects>)
target_include_directories (hex_static
  PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
  )

# Dynamic library (with Tcl)
add_library (hex SHARED
  $<TARGET_OBJECTS:hex_objects>
  src/tcl.c)
target_compile_definitions (hex PRIVATE
  USE_TCL_STUBS
  USE_TCLOO_STUBS)
target_link_libraries (hex PRIVATE ${TCL_STUB_LIBRARY})
target_include_directories (hex
  PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
  PRIVATE
  ${TCL_INCLUDE_PATH}
  )

if (NOT WIN32)
  # Don't install on Win32, since we build a starkit instead
  install (TARGETS hex
    LIBRARY DESTINATION ${HEX_DATADIR}/hextk/lib/hex
    RUNTIME DESTINATION ${HEX_DATADIR}/hextk/lib/hex)

  install (FILES pkgIndex.tcl DESTINATION ${HEX_DATADIR}/hextk/lib/hex)
endif ()
