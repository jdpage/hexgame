cmake_minimum_required (VERSION 3.11)
project (hexgame)

include (GNUInstallDirs)
find_package (TclStub REQUIRED)

set (HEX_AI_DIR "${CMAKE_INSTALL_DATADIR}/${CMAKE_PROJECT_NAME}/ai")

add_subdirectory (libhex)
add_subdirectory (hexmon)
add_subdirectory (ai)
add_subdirectory (hextk)
