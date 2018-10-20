add_chicken_library (scheme_hex STATIC
  hex.scm
  EMIT_IMPORT_LIBRARIES hex
  EMBEDDED)
target_link_libraries (scheme_hex PUBLIC hex)

add_chicken_library (scheme_tictoc STATIC
  tictoc.scm
  EMIT_IMPORT_LIBRARIES tictoc
  EMBEDDED)

add_chicken_library (scheme_costheap STATIC
  costheap.scm
  IMPORTS tictoc
  EMIT_IMPORT_LIBRARIES costheap
  EMBEDDED)
target_link_libraries (scheme_costheap INTERFACE
  scheme_tictoc)

add_chicken_library (scheme_pathtools STATIC
  pathtools.scm
  IMPORTS hex tictoc costheap
  EMIT_IMPORT_LIBRARIES pathtools
  EMBEDDED)
target_link_libraries (scheme_pathtools INTERFACE
  scheme_hex
  scheme_tictoc
  scheme_costheap)
