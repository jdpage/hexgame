# Copyright 2018 Jonathan David Page <jonathan@sleepingcyb.org>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

set (_CHICKEN_HOME "")
if (CHICKEN_HOME AND IS_DIRECTORY "${CHICKEN_HOME}")
  set (_CHICKEN_HOME "${CHICKEN_HOME}")
  set (_CHICKEN_HOME_EXPLICIT 1)
else ()
  set (_ENV_CHICKEN_HOME "")
  if (DEFINED ENV{CHICKEN_HOME})
    file(TO_CMAKE_PATH "$ENV{CHICKEN_HOME}" _ENV_CHICKEN_HOME)
  endif ()
  if (_ENV_CHICKEN_HOME AND IS_DIRECTORY "${_ENV_CHICKEN_HOME}")
    set (_CHICKEN_HOME "${_ENV_CHICKEN_HOME}")
    set (_CHICKEN_HOME_EXPLICIT 1)
  endif ()
  unset (_ENV_CHICKEN_HOME)
endif ()

set (_CHICKEN_HINTS)
if (_CHICKEN_HOME)
  list (APPEND _CHICKEN_HINTS "${_CHICKEN_HOME}/bin")
endif ()

set (_CHICKEN_PATHS
  /usr/bin
  /usr/local/bin
  )
find_program (Chicken_CSC_EXECUTABLE
  NAMES csc
  HINTS ${_CHICKEN_HINTS}
  PATHS ${_CHICKEN_PATHS}
)

if (Chicken_CSC_EXECUTABLE)
  execute_process (COMMAND ${Chicken_CSC_EXECUTABLE} -release
    RESULT_VARIABLE res
    OUTPUT_VARIABLE var
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  if (res)
    if (${Chicken_FIND_REQUIRED})
      message (FATAL_ERROR "Error executing csc -release")
    else ()
      message (STATUS "Warning, could not run csc -release")
    endif ()
  else ()
    # Extract version components
    set (_chicken_version_regex [[(([0-9]+)(\.([0-9]+)(\.([0-9]+))?)?)]])
    if (var MATCHES ${_chicken_version_regex})
      set (Chicken_VERSION_STRING "${CMAKE_MATCH_1}")
      set (Chicken_VERSION_MAJOR "${CMAKE_MATCH_2}")
      if (CMAKE_MATCH_4)
        set (Chicken_VERSION_MINOR "${CMAKE_MATCH_4}")
      else ()
        set (Chicken_VERSION_MINOR 0)
      endif ()
      if (CMAKE_MATCH_6)
        set (Chicken_VERSION_PATCH "${CMAKE_MATCH_6}")
      else ()
        set (Chicken_VERSION_PATCH 0)
      endif ()
    else ()
      if (NOT Chicken_FIND_QUIETLY)
        message (WARNING "Chicken version not recognized:${ver}\nPlease report.")
      endif ()
      set (Chicken_VERSION_STRING "")
      set (Chicken_VERSION_MAJOR "")
      set (Chicken_VERSION_MINOR "")
      set (Chicken_VERSION_PATCH "")
    endif ()
    set (Chicken_VERSION "${Chicken_VERSION_MAJOR}")
    if (NOT "x${Chicken_VERSION}" STREQUAL "x")
      foreach (c MINOR PATCH)
        if (NOT "x${Chicken_VERSION_${c}}" STREQUAL "x")
          string (APPEND Chicken_VERSION ".${Chicken_VERSION_${c}}")
        else ()
          break ()
        endif ()
      endforeach ()
    endif ()
  endif ()
  message (STATUS "Found Chicken csc: ${Chicken_CSC_EXECUTABLE} (version ${Chicken_VERSION})")
endif ()


find_program (Chicken_CSI_EXECUTABLE
  NAMES csi
  HINTS ${_CHICKEN_HINTS}
  PATHS ${_CHICKEN_PATHS}
  )
message (STATUS "Found Chicken csi: ${Chicken_CSI_EXECUTABLE}")

# if (NOT TARGET Chicken::runtime)
#   add_library (Chicken::runtime INTERFACE IMPORTED)
#   target_compile_options (Chicken::runtime INTERFACE ${Chicken_CFLAGS})
#   target_link_libraries (Chicken::runtime INTERFACE ${Chicken_LIBS})
#   target_link_libraries (Chicken::runtime INTERFACE ${Chicken_LDFLAGS})
# endif ()

file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/chicken_imports")

function (add_chicken_library name type)
  set (CHICKEN_TARGET_PARAM_OPTIONS
    EMBEDDED)
  set (CHICKEN_TARGET_PARAM_ONE_VALUE_KEYWORDS)
  set (CHICKEN_TARGET_PARAM_MULTI_VALUE_KEYWORDS
    EMIT_IMPORT_LIBRARIES
    IMPORTS
    )
  cmake_parse_arguments(
    CHICKEN_TARGET_ARG
    "${CHICKEN_TARGET_PARAM_OPTIONS}"
    "${CHICKEN_TARGET_PARAM_ONE_VALUE_KEYWORDS}"
    "${CHICKEN_TARGET_PARAM_MULTI_VALUE_KEYWORDS}"
    ${ARGN})
  set (CHICKEN_SOURCES ${CHICKEN_TARGET_ARG_UNPARSED_ARGUMENTS})

  set (myflags ${CHICKENFLAGS})
  list (APPEND myflags -s)
  set (import_outputs)
  set (import_depends)

  if (DEFINED CHICKEN_TARGET_ARG_EMIT_IMPORT_LIBRARIES)
    foreach (lib ${CHICKEN_TARGET_ARG_EMIT_IMPORT_LIBRARIES})
      list (APPEND myflags -j "${lib}")
      list (APPEND import_outputs "${CMAKE_BINARY_DIR}/chicken_imports/${lib}.import.scm")
    endforeach ()
  endif ()

  if (DEFINED CHICKEN_TARGET_ARG_IMPORTS)
    foreach (import ${CHICKEN_TARGET_ARG_IMPORTS})
      list (APPEND myflags -extend "${CMAKE_BINARY_DIR}/chicken_imports/${import}.import.scm")
      list (APPEND myflags -types "${CMAKE_BINARY_DIR}/chicken_imports/${import}.types")
      list (APPEND import_depends "${CMAKE_BINARY_DIR}/chicken_imports/${import}.import.scm")
    endforeach ()
  endif ()

  if (${CHICKEN_TARGET_ARG_EMBEDDED})
    list (APPEND myflags -e)
  endif ()

  # Figure out CFLAGS, LIBS, LDFLAGS
  execute_process (COMMAND ${Chicken_CSC_EXECUTABLE} ${myflags} -cflags
    OUTPUT_VARIABLE _Chicken_CFLAGS)
  string (STRIP ${_Chicken_CFLAGS} _Chicken_CFLAGS)
  string (REPLACE " " ";" Chicken_CFLAGS ${_Chicken_CFLAGS})
  unset (_Chicken_CFLAGS)

  execute_process (COMMAND ${Chicken_CSC_EXECUTABLE} ${myflags} -libs
    OUTPUT_VARIABLE _Chicken_LIBS)
  string (STRIP ${_Chicken_LIBS} _Chicken_LIBS)
  string (REPLACE " " ";" Chicken_LIBS ${_Chicken_LIBS})
  unset (_Chicken_LIBS)

  execute_process (COMMAND ${Chicken_CSC_EXECUTABLE} ${myflags} -ldflags
    OUTPUT_VARIABLE _Chicken_LDFLAGS)
  string (STRIP ${_Chicken_LDFLAGS} _Chicken_LDFLAGS)
  string (REPLACE "\\" "/" _Chicken_LDFLAGS ${_Chicken_LDFLAGS})  # fix for MSYS
  string (REPLACE " " ";" Chicken_LDFLAGS ${_Chicken_LDFLAGS})
  unset (_Chicken_LDFLAGS)

  # Add custom commands to generate C source files
  set (CHICKEN_GENERATED_SOURCES)
  foreach (src ${CHICKEN_SOURCES})
    get_filename_component (stem "${src}" NAME_WE)
    set (input "${CMAKE_CURRENT_SOURCE_DIR}/${src}")
    set (output "${CMAKE_CURRENT_BINARY_DIR}/${src}.c")
    list (APPEND CHICKEN_GENERATED_SOURCES ${output})

    add_custom_command (
      OUTPUT "${output}" ${import_outputs} "${CMAKE_BINARY_DIR}/chicken_imports/${stem}.types"
      COMMAND ${Chicken_CSC_EXECUTABLE} ${myflags} -t "${input}" -o "${output}" -emit-type-file "${stem}.types"
      MAIN_DEPENDENCY "${src}"
      DEPENDS ${import_depends}
      WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/chicken_imports")
  endforeach ()

  set (CHICKEN_IMPORT_LIBRARIES)
  if (DEFINED CHICKEN_TARGET_ARG_EMIT_IMPORT_LIBRARIES)
    foreach (lib ${CHICKEN_TARGET_ARG_EMIT_IMPORT_LIBRARIES})
      list (APPEND CHICKEN_IMPORT_LIBRARIES -extend "${CMAKE_CURRENT_BINARY_DIR}/${lib}.import.scm")
    endforeach ()
  endif ()

  # Create a regular C language target
  add_library (${name} ${type} ${CHICKEN_GENERATED_SOURCES})
  target_compile_options (${name} PRIVATE ${Chicken_CFLAGS})
  target_link_libraries (${name} PUBLIC ${Chicken_LIBS})
  target_link_libraries (${name} PRIVATE ${Chicken_LDFLAGS})
  set_target_properties (${name} PROPERTIES
    CHICKEN_IMPORT_LIBRARIES "${CHICKEN_IMPORT_LIBRARIES}")

  # Disable problematic warnings that the user might have enabled
  if (NOT MSVC)
    target_compile_options (${name} PRIVATE
      -Wno-conversion
      -Wno-float-equal
      -Wno-null-dereference
      -Wno-redundant-decls
      -Wno-sign-compare
      -Wno-sign-conversion
      -Wno-shadow
      -Wno-unused-but-set-variable
      -Wno-unused-function
      -Wno-unused-label
      -Wno-unused-parameter
      -Wno-unused-variable
      )
  endif ()

  # Add extra flags required for Windows. Basically, lie and say we're using
  # MinGW, then disable a bunch of warnings.
  if (MSVC)
    target_compile_definitions (${name} PRIVATE
      __MINGW32__
      __MINGW64__
      )
    target_compile_options (${name} PRIVATE
      /wd4101  # unreferenced local variables
      /wd4244  # downcast possible loss of data
      )
  endif ()
endfunction()
