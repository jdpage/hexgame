add_library (hexmonlib INTERFACE)
target_include_directories (hexmonlib INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>)

if (WIN32)
  set (PLATFORM_SRC src/windows.c)
else ()
  set (PLATFORM_SRC src/posix.c)
endif ()

add_executable (hexmon
  ${PLATFORM_SRC}
  src/hexmon.c
  src/parser.c)
target_link_libraries (hexmon ${CMAKE_DL_LIBS} hexmonlib)
target_include_directories (hexmon PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)
install (TARGETS hexmon
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
