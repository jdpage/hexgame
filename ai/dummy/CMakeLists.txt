add_library (dummy MODULE dummy.c)
target_link_libraries (dummy hex hexmonlib)
install (TARGETS dummy
  LIBRARY DESTINATION ${HEX_AI_DIR})
