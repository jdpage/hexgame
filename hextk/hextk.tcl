# hextk GUI
# Copyright (C) 2018  Jonathan David Page <jonathan@sleepingcyb.org>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

package require Tk 8.6
package require hex


array set gameconfig {}
set gameconfig(blue) human
set gameconfig(red) human
set gameconfig(first) blue
set gameconfig(size) 11

array set game {}

array set stcos {}
array set stsin {}
set deg2rad [expr {acos(-1) / 180.0}]
for {set a 0} {$a <= 360} {incr a 30} {
    set ang [expr {$a * $deg2rad}]
    set stcos($a) [expr {cos($ang)}]
    set stsin($a) [expr {sin($ang)}]
}

array set color {}
set color(empty) white
set color(red) red
set color(blue) blue
set color(hover_red) pink
set color(hover_blue) {light blue}
set color(hover_done) {light grey}


proc find_ais {share} {
    # AIs are stored in the ai directory. Right now we only support shared
    # object AIs; in the future, maybe we'll provide loaders for other
    # languages?

    set aipath [file join $share ai]
    set ais [dict create human {}]
    foreach ai [glob -directory $aipath "*[info sharedlibextension]"] {
        set name [file rootname [file tail $ai]]
        if {[string compare -length 3 lib $name] == 0} {
            set name [string range $name 3 end]
        }

        dict set ais $name [file normalize $ai]
    }

    return $ais
}


proc build_config {win} {
    wm title $win "Hex Setup"
    bind $win <Destroy> { exit }

    set c [ttk::frame $win.f]
    pack $c -fill both -expand 1

    foreach {name text} {blue "Blue:" red "Red:"} {
        ttk::labelframe $c.$name -text $text
        pack $c.$name -side top -fill both -expand 1 -padx 10 -pady 10

        ttk::combobox $c.$name.sel \
            -state readonly \
            -values [dict keys $::game(ais)] \
            -textvariable gameconfig($name)
        pack $c.$name.sel -side top -fill x -expand 1 -padx 10 -pady 10

        ttk::radiobutton $c.$name.first \
            -text "First turn" \
            -value $name \
            -variable gameconfig(first)
        pack $c.$name.first -side top -fill x -expand 1 -padx 10 -pady 10
    }

    # TODO this is kind of ugly next to the TTK widgets, but ttk::scale doesn't
    # support the -resolution option.
    tk::scale $c.size \
        -from 7 -to 19 \
        -orient horizontal \
        -variable gameconfig(size) \
        -resolution 1
    pack $c.size -side top -fill x -padx 10 -pady 10

    ttk::button $c.play -text Play -command { play_game }
    pack $c.play -side bottom -fill x -padx 10 -pady 10
}


proc build_gameview {win} {
    wm title $win "Hex Game"

    # TODO: prompt for exit and shut down children
    bind $win <Destroy> { exit }

    canvas $win.view
    pack $win.view -side top
}


proc draw_hex {c x y r tag} {
    global stcos
    global stsin
    global color

    for {set a 30} {$a < 360} {incr a 60} {
        lappend points [expr {$x+$r*$stcos($a)}] [expr {$y+$r*$stsin($a)}]
    }

    $c create polygon $points -fill $color(empty) -tag $tag -width 0
    $c bind $tag <Enter> [list apply {{c tag} {
        $c itemconfig $tag -fill $::color(hover_$::game(current))
    }} $c $tag]
    $c bind $tag <Leave> [list $c itemconfig $tag -fill $color(empty)]
}


proc hex_x {r c s} {
    return [expr {$s * ($c*2 + $r + 3)}]
}


proc hex_y {r c s} {
    global stcos

    return [expr {$s * 2 * ($r*$stcos(30) + 1)}]
}


proc draw_board {cn s board} {
    global stcos
    global color

    set w [expr {$s * 2}]
    set d [$board size]
    set d1 [expr {$d - 1}]
    set width [expr {($d*3 + 3) * $s}]
    set height [expr {$s * ($d * 2 + 0.5) * $stcos(30) + $s * 2}]

    $cn configure -width $width -height $height

    # red player edges
    $cn create polygon [list \
                            [hex_x -1 -1 $s] [hex_y -1 -1 $s] \
                            [hex_x 0 0 $s] [hex_y 0 0 $s] \
                            [hex_x 0 $d1 $s] [hex_y 0 $d1 $s] \
                            [hex_x -1 $d $s] [hex_y -1 $d $s]] \
        -fill $color(red) -width 0
    $cn create polygon [list \
                            [hex_x $d $d $s] [hex_y $d $d $s] \
                            [hex_x $d1 $d1 $s] [hex_y $d1 $d1 $s] \
                            [hex_x $d1 0 $s] [hex_y $d1 0 $s] \
                            [hex_x $d -1 $s] [hex_y $d -1 $s]] \
        -fill $color(red) -width 0

    # blue player edges
    $cn create polygon [list \
                            [hex_x $d1 0 $s] [hex_y $d1 0 $s] \
                            [hex_x 0 0 $s] [hex_y 0 0 $s] \
                            [hex_x -1 -1 $s] [hex_y -1 -1 $s] \
                            [hex_x $d -1 $s] [hex_y $d -1 $s]] \
        -fill $color(blue) -width 0
    $cn create polygon [list \
                            [hex_x $d $d $s] [hex_y $d $d $s] \
                            [hex_x -1 $d $s] [hex_y -1 $d $s] \
                            [hex_x 0 $d1 $s] [hex_y 0 $d1 $s] \
                            [hex_x $d1 $d1 $s] [hex_y $d1 $d1 $s]] \
        -fill $color(blue) -width 0

    foreach {r c} [$board coords] {
        draw_hex $cn [hex_x $r $c $s] [hex_y $r $c $s] $s "$r,$c"
        $cn bind "$r,$c" <1> [list human_ready [list $r $c]]
    }
}


proc mark_space {r c col} {
    global color

    set cn .game.view
    set t "$r,$c"
    $cn itemconfigure "$r,$c" -fill $color($col)
    $cn bind $t <Enter> {}
    $cn bind $t <Leave> {}
    $cn bind $t <1> {}
}


proc start_ai {ai color} {
    global game

    set path [dict get $game(ais) $ai]
    if {$path eq ""} {
        set game(ai$color) ""
    } else {
        set game(ai$color) [open |[list hexmon [$game(board) size] $color $path 2>@stderr] r+]
        fconfigure $game(ai$color) -buffering line -blocking 0
        fileevent $game(ai$color) readable [list ai_ready $color]
    }
}


proc stop_ai {color} {
    global game

    if {$game(ai$color) eq ""} {
        return
    }

    puts $game(ai$color) "quit [$game(board) dump]"
}


proc stop_ais {} {
    foreach color {blue red} {
        stop_ai $color
    }
}


proc ai_ready {color} {
    global game

    if {[eof $game(ai$color)]} {
        if {$game(current) eq "done"} {
            close $game(ai$color)
            return
        }

        error "ai $color dead"
    }

    set line [gets $game(ai$color)]
    if {$line ne ""} {
        receive_move $line
    }
}


proc human_ready {m} {
    global game

    # make sure we're supposed to be looking at moves from humans
    if {$game(current) ne "done" && $game(ai$game(current)) eq ""} {
        receive_move $m
    }
}


proc request_move {color} {
    global game

    # if we have an AI, tell it to move. Otherwise, we just wait for the user to
    # move.
    if {$game(ai$color) ne ""} {
        puts $game(ai$color) "move [$game(board) dump]"
    }
}


proc receive_move {m} {
    global game
    global color

    # check to see if the move is legal
    if {[$game(board) get $m] ne ""} {
        # TODO nicer error handling
        error "illegal move"
    }

    # update the game state
    $game(board) set $m $game(current)
    mark_space {*}$m $game(current)

    # check for a win
    set winner [$game(board) winner]
    if {$winner ne ""} {
        .game.view configure -bg $color($winner)
        set game(current) done
        stop_ais
        return
    }

    if {$game(current) eq "red"} {
        set game(current) blue
    } else {
        set game(current) red
    }

    request_move $game(current)
}


proc play_game {} {
    global gameconfig
    global game

    wm withdraw .config
    wm deiconify .game

    set game(current) $gameconfig(first)
    set game(board) [hex::board new $gameconfig(size)]
    draw_board .game.view 40 $game(board)

    foreach c {blue red} {
        start_ai $gameconfig($c) $c
    }

    request_move $game(current)
}


proc main {} {
    set share [file dirname [info script]]
    set ::game(ais) [find_ais $share]

    if {[lindex $::tcl_platform(os) 0] eq "Windows"} {
        ttk::style theme use xpnative
    } else {
        # TODO calculate this properly
        # seems to behave super weird under Linux
        tk scaling 1.0
        ttk::style theme use clam

        set sep $::tcl_platform(pathSeparator)
        set ldpath [split [array get ::env LD_LIBRARY_PATH] $sep]
        lappend ldpath $share
        set ::env(LD_LIBRARY_PATH) [join $ldpath $sep]
    }

    build_config [toplevel .config -class "Hex Game"]
    build_gameview [toplevel .game -class "Hex Game"]
    wm withdraw .
    wm withdraw .game
}


main
