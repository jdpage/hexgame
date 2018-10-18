add_chicken_library (scheme_hex STATIC
  hex.scm
  EMBEDDED)
target_link_libraries (scheme_hex PUBLIC hex)

add_chicken_library (scheme_costheap STATIC
  costheap.scm
  EMIT_IMPORT_LIBRARIES costheap
  EMBEDDED)

add_chicken_library (scheme_tictoc STATIC
  tictoc.scm
  EMIT_IMPORT_LIBRARIES tictoc
  EMBEDDED)
