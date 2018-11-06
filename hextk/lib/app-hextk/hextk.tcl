package provide app-hextk 1.0

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
source [file join [file dirname [info script]] "game.tcl"]

array set ais {}
set ais(human) {}

array set stcos {}
array set stsin {}
set deg2rad [expr {acos(-1) / 180.0}]
for {set a 0} {$a <= 360} {incr a 30} {
    set ang [expr {$a * $deg2rad}]
    set stcos($a) [expr {cos($ang)}]
    set stsin($a) [expr {sin($ang)}]
}

set tilesize 20

array set colors {}
set colors(empty) white
set colors(background) {light grey}
set colors(red) red
set colors(blue) blue
set colors(hover_red) pink
set colors(hover_blue) {light blue}
set colors(hover_done) {light grey}

proc lhas {lst item} {
    return [expr {[lsearch -exact $lst $item] != -1}]
}

proc find_ais {share} {
    # AIs are stored in the ai directory. Right now we only support shared
    # object AIs; in the future, maybe we'll provide loaders for other
    # languages?

    global ais

    set aipath [file join $share ai]
    try {
        foreach ai [glob -directory $aipath "*[info sharedlibextension]"] {
            set name [file rootname [file tail $ai]]
            if {[string compare -length 3 lib $name] == 0} {
                set name [string range $name 3 end]
            }

            set ais($name) [file normalize $ai]
        }
    } trap {TCL OPERATION GLOB NOMATCH} {} {
        # No ais, I guess
    }
}

oo::class create Toplevel {
    mixin CommandInvoking
    variable Hull

    constructor {path args} {
        set tlargs [my ConfigureCommands 0 {*}$args]
        set Hull [toplevel $path {*}$tlargs]
    }

    destructor {
        destroy $Hull
    }

    method configure {args} {
        $Hull configure {*}[my ConfigureCommands 0 {*}$args]
    }

    method show {} {
        if {[wm state $Hull] eq "withdrawn"} {
            wm deiconify $Hull
        }
    }

    method hide {} {
        if {[wm state $Hull] ne "withdrawn"} {
            wm withdraw $Hull
        }
    }

    method Hull {} { return $Hull }
}

oo::class create configwindow {
    superclass Toplevel
    variable Config

    constructor {path args} {
        global ais

        my AddCommands play
        next $path {*}$args
        my hide

        array set Config {}
        set Config(blue) human
        set Config(red) human
        set Config(first) blue
        set Config(size) 11

        set w [my Hull]
        wm title $w "Hex Setup"
        bind $w <Destroy> { exit }

        set c [ttk::frame $w.f]
        pack $c -fill both -expand 1

        foreach {name text} {blue "Blue:" red "Red:"} {
            ttk::labelframe $c.$name -text $text
            pack $c.$name -side top -fill both -expand 1 -padx 10 -pady 10

            ttk::combobox $c.$name.sel \
                -state readonly \
                -values [array names ais] \
                -textvariable [my Config $name]
            pack $c.$name.sel -side top -fill x -expand 1 -padx 10 -pady 10

            ttk::radiobutton $c.$name.first \
                -text "First turn" \
                -value $name \
                -variable [my Config first]
            pack $c.$name.first -side top -fill x -expand 1 -padx 10 -pady 10
        }

        # TODO this is kind of ugly next to the TTK widgets, but ttk::scale
        # doesn't support the -resolution option.
        tk::scale $c.size \
            -from 7 -to 19 \
            -orient horizontal \
            -variable [my Config size] \
            -resolution 1
        pack $c.size -side top -fill x -padx 10 -pady 10

        ttk::button $c.play -text Play \
            -command [list [self namespace]::my InvokeCommand play]
        pack $c.play -side bottom -fill x -padx 10 -pady 10
    }

    method get {key} { set [my Config $key] }

    method Config {key} { return [self namespace]::Config($key) }
}

oo::class create gamewindow {
    superclass Toplevel
    variable View Config Cons Session

    constructor {path} {
        global tilesize

        next $path -class "Hex Game"
        my hide

        set w [my Hull]
        wm title $w "Hex Game"
        bind $w <Destroy> [list [self namespace]::my OnQuit]

        set View [gameboard new $w.view $tilesize]
        pack $w.view -side top

        set Config [configwindow new $w.config -class "Hex Game" \
                        -playcommand [list [self] startgame] \
                       ]

        set Session ""

        foreach c {blue red} {
            set Cons($c) [aiconsole new $w.$c $c -class "Hex Game"]
        }

        set mbar [menu $w.mbar]
        $w configure -menu $mbar

        set mgame [menu $mbar.game -tearoff 0]
        $mgame add command -label "New..." -underline 0 \
            -command [list [self] configuregame]
        $mgame add command -label "Restart" -underline 0 \
            -command [list [self] startgame]
        $mgame add command -label "Quit" -underline 0 \
            -command [list [self namespace]::my OnQuit]

        set mview [menu $mbar.view -tearoff 0]
        $mview add command -label "AI Blue Console" -underline 3 \
            -command [list $Cons(blue) show]
        $mview add command -label "AI Red Console" -underline 3 \
            -command [list $Cons(red) show]

        $mbar add cascade -label "Game" -underline 0 -menu $mgame
        $mbar add cascade -label "View" -underline 0 -menu $mview
    }

    method endgame {} {
        if {$Session ne ""} {
            $Session destroy
            set Session ""
        }
    }

    method startgame {} {
        global ais
        $Config hide

        my endgame
        set Session [hexsession new \
                         [$Config get size] \
                         [$Config get first] \
                         $ais([$Config get blue]) {} \
                         $ais([$Config get red]) {} \
                        ]

        foreach c {blue red} {
            [$Session $c] configure \
                -outcommand [list $Cons($c) add] \
                -errcommand [list $Cons($c) adderror]
        }

        $View attach $Session
        $Session start
        my show
    }

    method configuregame {} {
        my hide
        my endgame
        $Config show
    }

    method OnQuit {} {
        # TODO: prompt for exit
        my endgame
        exit
    }
}

oo::class create aiconsole {
    superclass Toplevel
    variable w

    constructor {path color args} {
        next $path {*}$args
        my hide

        set w [my Hull]
        wm title $w "AI $color Console"

        ttk::scrollbar $w.scroll -orient vertical -command [list $w.log yview]
        pack $w.scroll -side right -fill y

        text $w.log -state disabled -yscrollcommand [list $w.scroll set]
        pack $w.log -side right -fill both -expand 1

        $w.log tag configure err -foreground red

        # wm transient $w [winfo parent $w]
        wm group $w [winfo parent $w]
        wm protocol $w WM_DELETE_WINDOW [list [self] hide]
    }

    forward add my Post {}
    forward adderror my Post err

    method Post {tags msg} {
        $w.log configure -state normal
        $w.log insert end "$msg\n" $tags
        $w.log configure -state disabled
        $w.log see end

        my show
    }
}

oo::class create gameboard {
    variable Hull Scale Session

    constructor {path scale args} {
        set Hull [canvas $path]
        set Scale $scale
        set Session ""
        my Rebuild
    }

    method attach {session} {
        set Session $session
        $Session configure \
            -acceptcommand [list [self namespace]::my MarkSpace] \
            -wincommand [list [self namespace]::my OnWin]
        my Rebuild
    }

    method refresh {m {entering 0} {setcursor 1}} {
        global colors

        if {$Session eq ""} { return }

        set cursor {}
        if {$m ne ""} {
            set t [join $m ,]
            set tags [$Hull gettags $t]

            set turnof [$Session turnof]
            if {![lhas $tags taken]} {
                if {$entering} {
                    $Hull itemconfigure $t -fill $colors(hover_$turnof)
                    if {[$Session postable $turnof]} {
                        set cursor hand2
                    } elseif {$turnof ne "done"} {
                        set cursor watch
                    }
                } else {
                    $Hull itemconfigure $t -fill $colors(empty)
                }
            }
        }

        if {$setcursor} {
            $Hull configure -cursor $cursor
        }
    }

    method DrawHex {r c tags} {
        global stcos
        global stsin
        global colors

        set x [my X $r $c]
        set y [my Y $r $c]

        for {set a 30} {$a < 360} {incr a 60} {
            lappend points [expr {$x+$Scale*$stcos($a)}] [expr {$y+$Scale*$stsin($a)}]
        }

        set id [$Hull create polygon $points -tags $tags -width 0]
        my refresh $id 0 0
    }

    method X {r c} { return [expr {$Scale * ($c*2 + $r + 3)}] }
    method Y {r c} {
        global stcos
        return [expr {$Scale * 2 * ($r*$stcos(30) + 1)}]
    }

    method DrawEdge {color rcs} {
        global colors

        $Hull create polygon [join [lmap {r c} $rcs {
            list [my X $r $c] [my Y $r c]
        }]] -fill $colors($color) -width 0 -tags taken
    }

    method Rebuild {} {
        global stcos
        global colors

        if {$Session eq ""} { return }

        set w [expr {$Scale * 2}]
        set d [$Session size]
        set d1 [expr {$d - 1}]
        set width [expr {($d*3 + 3) * $Scale}]
        set height [expr {$Scale * ($d * 2 + 0.5) * $stcos(30) + $Scale * 2}]

        $Hull delete all
        $Hull configure -width $width -height $height -bg $colors(background)

        # red player edges
        my DrawEdge red [list -1 -1 0 0 0 $d1 -1 $d]
        my DrawEdge red [list $d $d $d1 $d1 $d1 0 $d -1]

        # blue player edges
        my DrawEdge blue [list $d1 0 0 0 -1 -1 $d -1]
        my DrawEdge blue [list $d $d -1 $d 0 $d1 $d1 $d1]

        # hexes
        for {set c 0} {$c < [$Session size]} {incr c} {
            for {set r 0} {$r < [$Session size]} {incr r} {
                set tag "$r,$c"
                my DrawHex $r $c [list $tag]
                $Hull bind $tag <Enter> [list [self] refresh [list $r $c] 1]
                $Hull bind $tag <Leave> [list [self] refresh [list $r $c] 0]
                $Hull bind $tag <1> [list [self namespace]::my OnClick [list $r $c]]
            }
        }
    }

    method MarkSpace {color rc} {
        global colors

        set t [join $rc ,]
        $Hull addtag taken withtag $t
        $Hull itemconfigure $t -fill $colors($color)
        $Hull bind $t <Enter> {}
        $Hull bind $t <Leave> {}
        $Hull bind $t <1> {}
        my refresh current 1
    }

    method OnWin {color} {
        global colors

        if {$color ne ""} {
            $Hull configure -background $colors($color)
        }
    }

    method OnClick {m} {
        if {$Session eq ""} { return }

        set turnof [$Session turnof]
        if {[$Session postable $turnof]} {
            [$Session $turnof] post $m
        }
    }
}

proc main {} {
    global tilesize

    set share [file dirname $::starkit::topdir]
    find_ais $share

    # If we're on Windows, we're going to call update to force Tk to map the
    # main window so that we can get its DPI. This causes the main window to
    # flash onscreen unless we withdraw it; apparently it still gets an HWND,
    # even withdrawn, which is all we need.
    wm withdraw .

    set platform [lindex $::tcl_platform(os) 0]
    if {$platform eq "Windows"} {
        package require ntapi

        # Tclkit isn't DPI-aware, and using mt to add a manifest breaks it. So
        # we need to detect if we're in a high-DPI environment, and adjust our
        # scaling. In order to get the DPI, we have to force Tk to finish
        # mapping the main window using update. Additionally, if we're under
        # ActiveTcl, we might actually be set--in this case, nt::getdpiforwindow
        # will return the same values before and after nt::setprocessdpiaware,
        # in which case the scaling won't be altered.
        update
        set basescale [tk scaling]
        set basedpi [nt::getdpiforwindow .]
        if {[nt::setprocessdpiaware]} {
            set realdpi [nt::getdpiforwindow .]
            tk scaling [expr {$realdpi / $basedpi * $basescale}]
        }

        # Canvas doesn't do scaling, so do it manually by adjusting the tile size.
        set tilesize [expr {$tilesize * [tk scaling]}]

    } elseif {$platform eq "Linux"} {
        ttk::style theme use clam

        # TODO work out what is going on here. For some reason, the tk scaling
        # setting seems to go the wrong way (!) under GNOME3/Wayland, and the
        # value that it's populated with is hilariously large.

        set tilesize [expr {$tilesize * 2}]
        tk scaling 1.0
    }

    [gamewindow new .game] configuregame
}

main
