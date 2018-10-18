cmake_minimum_required (VERSION 3.11)
project (hexgame)

set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")

include (GNUInstallDirs)
find_package (Chicken REQUIRED)
find_package (TclStub REQUIRED)

set (HEX_AI_DIR "${CMAKE_INSTALL_DATADIR}/${CMAKE_PROJECT_NAME}/ai")

set (CMAKE_C_STANDARD 99)
set (CMAKE_C_EXTENSIONS ON)
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

add_subdirectory (libhex)
add_subdirectory (hexmon)
add_subdirectory (support)
add_subdirectory (ai)
add_subdirectory (hextk)
