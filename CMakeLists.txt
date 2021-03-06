cmake_minimum_required (VERSION 3.11)
project (hexgame VERSION 0.1 LANGUAGES C)

set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")

include (GNUInstallDirs)
find_package (Chicken REQUIRED)
find_package (TclStub REQUIRED)

set (HEX_DATADIR "${CMAKE_INSTALL_DATADIR}/${CMAKE_PROJECT_NAME}")
if (WIN32)
  set (HEX_BINDIR ".")
  set (HEX_AI_DIR "./ai")
else ()
  set (HEX_BINDIR "${CMAKE_INSTALL_BINDIR}")
  set (HEX_AI_DIR "${CMAKE_INSTALL_DATADIR}/${CMAKE_PROJECT_NAME}/ai")
endif ()

set (CMAKE_C_STANDARD 99)
set (CMAKE_C_EXTENSIONS ON)

if (NOT MSVC)
  add_compile_options (
    "-Wall"
    "-Wextra"
    "-Wnull-dereference"
    "-Wuninitialized"
    "-Wstrict-overflow"
    "-Wfloat-equal"
    "-Wshadow"
    "-Wconversion"
    "-Wdate-time"
    "-Wsign-conversion"
    "-Wpacked"
    "-Wredundant-decls"
    "$<$<CONFIG:DEBUG>:-Werror>")
else ()
  # Provides shims for building Chicken under MSVC
  include_directories ("support/windows/include")
endif ()

add_subdirectory (libhex)
add_subdirectory (hexmon)
add_subdirectory (support)
add_subdirectory (ai)
add_subdirectory (hextk)

set (CPACK_GENERATOR "ZIP")
include (CPack)
