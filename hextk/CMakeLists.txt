set (HEXTK_LAUNCHER ${CMAKE_CURRENT_BINARY_DIR}/hextk)
file (WRITE ${HEXTK_LAUNCHER} "#!${TK_WISH}\n")
file (APPEND ${HEXTK_LAUNCHER} "set sep $::tcl_platform(pathSeparator)\n")
file (APPEND ${HEXTK_LAUNCHER} "set env(PATH) \"$env(PATH)$sep[file dirname [info script]]\"\n")
file (APPEND ${HEXTK_LAUNCHER} "set sharepath [file join [file dirname [info script]] ")
file (APPEND ${HEXTK_LAUNCHER} "{../${CMAKE_INSTALL_DATADIR}/${CMAKE_PROJECT_NAME}}]\n")
if (NOT WIN32)
  file (APPEND ${HEXTK_LAUNCHER} "lappend auto_path $sharepath\n")
else ()
  file (APPEND ${HEXTK_LAUNCHER} "lappend auto_path [file dirname [info script]]\n")
endif ()
file (APPEND ${HEXTK_LAUNCHER} "source [file join $sharepath hextk.tcl]\n")

install (PROGRAMS
  ${HEXTK_LAUNCHER}
  DESTINATION ${CMAKE_INSTALL_BINDIR})
install (FILES
  hextk.tcl
  game.tcl
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${CMAKE_PROJECT_NAME})
