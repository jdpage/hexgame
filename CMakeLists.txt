cmake_minimum_required (VERSION 3.11)
project (hexgame)

find_package (TclStub REQUIRED)

add_subdirectory (libhex)
add_subdirectory (hexmon)
add_subdirectory (ai)
