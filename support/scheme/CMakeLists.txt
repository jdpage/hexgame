add_chicken_library (scheme_hex STATIC
  hex.scm
  EMIT_IMPORT_LIBRARIES hex
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

add_chicken_library (scheme_pathtools STATIC
  pathtools.scm
  IMPORTS hex tictoc
  EMIT_IMPORT_LIBRARIES pathtools
  EMBEDDED)
target_link_libraries (scheme_pathtools INTERFACE
  scheme_hex
  scheme_tictoc)
