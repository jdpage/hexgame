set (CHICKENFLAGS ${CHICKENFLAGS} -e -s)

add_chicken_library (disruptor MODULE
  disruptor.scm)
target_link_libraries (disruptor PRIVATE
  scheme_hex
  scheme_tictoc
  hexmonlib)
install (TARGETS disruptor
  LIBRARY DESTINATION "${HEX_AI_DIR}")
