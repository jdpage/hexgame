set (CHICKENFLAGS ${CHICKENFLAGS} -e -s)

add_chicken_library (disruptor MODULE
  disruptor.scm
  IMPORTS hex tictoc pathtools)
target_link_libraries (disruptor PRIVATE
  scheme_hex
  scheme_tictoc
  scheme_pathtools
  hexmonlib)
install (TARGETS disruptor
  LIBRARY DESTINATION "${HEX_AI_DIR}")
