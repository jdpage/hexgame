set (CHICKENFLAGS ${CHICKENFLAGS} -e -s)

add_chicken_library (pessimist MODULE
  pessimist.scm
  IMPORTS hex pathtools tictoc)
target_link_libraries (pessimist PUBLIC
  scheme_hex
  scheme_tictoc
  scheme_pathtools
  hexmonlib)
install (TARGETS pessimist
  LIBRARY DESTINATION "${HEX_AI_DIR}")
