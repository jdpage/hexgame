if (WIN32)

  # On Windows, we build a Starpack, and install that.

  # Tcl 8.6.8, KitCreator 0.11.0, Platform win64, Unthreaded, Packages: mk4tcl, tk
  # TODO: Check with rkeene if this is kosher
  set (TCLKIT "${CMAKE_CURRENT_BINARY_DIR}/tools/tclkit.exe")
  file (DOWNLOAD
    http://kitcreator.rkeene.org/kits/aa67f5e0a4ae35b119a79639b841034aef9a5034/tclkit.exe
    ${TCLKIT}
    EXPECTED_HASH SHA256=4555fadada21a55aa172834f80f55caa7fc644cc56eb7eef0c2d710fe1f2dcb6
    SHOW_PROGRESS
    )

  # SDX for 2011-03-17. This is latest at time of writing AFAICT.
  set (SDXKIT "${CMAKE_CURRENT_BINARY_DIR}/tools/sdx.kit")
  file (DOWNLOAD
    https://chiselapp.com/user/aspect/repository/sdx/uv/sdx-20110317.kit
    ${SDXKIT}
    EXPECTED_HASH SHA256=04dc85d4503c037f26c2b8719e45e4e20385674fbc7404481f3c171fce0a26c3
    SHOW_PROGRESS
    )

  # Copy directory structure into a vfs folder
  set (HEXTK_SOURCES
    main.tcl
    lib/app-hextk/game.tcl
    lib/app-hextk/hextk.tcl
    lib/app-hextk/pkgIndex.tcl)

  # Copy Tcl sources
  set (VFS_DIR "${CMAKE_CURRENT_BINARY_DIR}/vfs")
  add_custom_target (hextk_starkit_sources DEPENDS ${HEXTK_SOURCES})
  add_custom_command (TARGET hextk_starkit_sources PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory "${VFS_DIR}"
    COMMAND ${CMAKE_COMMAND} -E copy main.tcl "${VFS_DIR}"
    COMMAND ${CMAKE_COMMAND} -E copy_directory lib "${VFS_DIR}/lib"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

  # Copy libhex
  add_custom_target (hextk_starkit_hex DEPENDS hex)
  add_custom_command (TARGET hextk_starkit_hex PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory "${VFS_DIR}/lib/hex"
    COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:hex>" "${VFS_DIR}/lib/hex/$<TARGET_FILE_NAME:hex>"
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_SOURCE_DIR}/libhex/pkgIndex.tcl" "${VFS_DIR}/lib/hex/pkgIndex.tcl"
    COMMENT "Copying libhex into VFS")

  # Copy ntjobs
  add_custom_target (hextk_starkit_ntjobs DEPENDS ntjobs)
  add_custom_command (TARGET hextk_starkit_ntjobs PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory "${VFS_DIR}/lib/ntjobs"
    COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:ntjobs>" "${VFS_DIR}/lib/ntjobs/$<TARGET_FILE_NAME:ntjobs>"
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_SOURCE_DIR}/support/windows/tcl/pkgIndex.tcl" "${VFS_DIR}/lib/ntjobs/pkgIndex.tcl"
    COMMENT "Copying ntjobs into VFS")

  # Build starkit. We assume that you are using ActiveTcl, not MinGW Tcl.
  add_custom_target (hextk_starkit ALL
    DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/hextk.exe")
  add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/hextk.exe"
    COMMAND ${TCL_TCLSH} ${SDXKIT} wrap hextk.exe -vfs vfs -runtime ${TCLKIT}
    DEPENDS hextk_starkit_sources hextk_starkit_hex hex hextk_starkit_ntjobs ntjobs ${HEXTK_SOURCES}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

  install (PROGRAMS
    "${CMAKE_CURRENT_BINARY_DIR}/hextk.exe"
    DESTINATION ${HEX_BINDIR})

else ()

  # On non-Windows platforms, just do a direct copy
  # TODO: Make something nice for macOS 
  set (HEXTK_LAUNCHER ${CMAKE_CURRENT_BINARY_DIR}/hextk)
  file (WRITE ${HEXTK_LAUNCHER} "#!/bin/sh\n")
  file (APPEND ${HEXTK_LAUNCHER} "export PATH=\"${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}:$PATH\"\n")
  file (APPEND ${HEXTK_LAUNCHER} "exec ${TK_WISH} \"${CMAKE_INSTALL_PREFIX}/${HEX_DATADIR}/hextk/main.tcl\" $@\n")

  install (PROGRAMS
    ${HEXTK_LAUNCHER}
    DESTINATION ${CMAKE_INSTALL_BINDIR})
  install (DIRECTORY lib/app-hextk
    DESTINATION ${HEX_DATADIR}/hextk/lib)
  install (FILES main.tcl
    DESTINATION ${HEX_DATADIR}/hextk)

endif ()
