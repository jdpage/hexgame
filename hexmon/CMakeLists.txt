add_library (hexmonlib INTERFACE)
target_include_directories (hexmonlib INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>)

add_executable (hexmon
  src/hexmon.c)
target_link_libraries (hexmon ${CMAKE_DL_LIBS} hexmonlib)
target_include_directories (hexmon PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)
install (TARGETS hexmon
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
