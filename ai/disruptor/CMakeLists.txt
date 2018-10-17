get_target_property (hex_inc hex INTERFACE_INCLUDE_DIRECTORIES)
get_target_property (hexmon_inc hexmonlib INTERFACE_INCLUDE_DIRECTORIES)

add_custom_command (
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/disruptor.o
  COMMAND csc -embedded -c -s disruptor.scm -o ${CMAKE_CURRENT_BINARY_DIR}/disruptor.o -I${hexmon_inc}
  MAIN_DEPENDENCY disruptor.scm
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS hexmonlib)

add_custom_command (
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/hex.o
  COMMAND csc -embedded -c -s hex.scm -o ${CMAKE_CURRENT_BINARY_DIR}/hex.o -I${hex_inc}
  MAIN_DEPENDENCY hex.scm
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS hex)

add_custom_command (
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/disruptor.so
  COMMAND csc -embedded -s disruptor.o hex.o -o disruptor.so -L$<TARGET_LINKER_FILE_DIR:hex> -lhex
  DEPENDS disruptor.o hex.o
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_custom_target (disruptor ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/disruptor.so)
install (FILES ${CMAKE_CURRENT_BINARY_DIR}/disruptor.so DESTINATION ${HEX_AI_DIR})
