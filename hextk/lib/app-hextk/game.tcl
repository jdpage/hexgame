# Hex game implementation
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

package require hex

if {[lindex $::tcl_platform(os) 0] eq "Windows"} {
    package require ntjobs
}

proc nextplayer {color} {
    if {$color eq "blue"} {
        return red
    } else {
        return blue
    }
}

oo::class create CommandInvoking {
    variable commands

    forward configure my ConfigureCommands 1

    method AddCommands {args} {
        foreach name $args {
            set opt [join [list "-" $name "command"] ""]
            set commands($opt) {}
        }
    }

    method ConfigureCommands {strict args} {
        set extras {}
        foreach {opt val} $args {
            if {[info exists commands($opt)]} {
                set commands($opt) [string trim $val]
            } else {
                if {$strict} {
                    error "unknown option \"$opt\""
                } else {
                    lappend extras $opt $val
                }
            }
        }
        return $extras
    }

    method InvokeCommand {name args} {
        set opt [join [list "-" $name "command"] ""]
        if {[info exists commands($opt)]} {
            if {$commands($opt) ne ""} {
                after idle [list after 0 $commands($opt) $args]
            }
        } else {
            error "unknown command option $opt"
        }
    }
}

# Defines win/loss/allowed-move logic for a game of Hex
oo::class create hexgame {
    superclass hex::board
    mixin CommandInvoking
    variable turnof

    constructor {size first args} {
        next $size
        my AddCommands win accept
        set turnof $first
        my configure {*}$args
    }

    unexport clear scan set

    method move {color m} {
        if {$turnof ne $color} {
            error "Out-of-turn move $color"
        }

        if {[my get $m] ne ""} {
            error "Illegal move $m to $color"
        }

        my set $m $color

        set winner [my winner]
        if {$winner ne ""} {
            set turnof done
            my InvokeCommand win $winner
        } else {
            set turnof [nextplayer $turnof]
        }

        my InvokeCommand accept $color $m
    }

    method turnof {} { return $turnof }
}

oo::class create autohup {
    constructor {args} {
        if {[lindex $::tcl_platform(os) 0] eq "Windows"} {
            puts "Creating NT job"
            nt::job create job
            job limits -killonjobclose 1
            job assign {*}$args
        }
    }

    destructor {
        if {[lindex $::tcl_platform(os) 0] eq "Windows"} {
            puts "Destroying NT job"
            job destroy
        }
    }
}

# Class for AI players. Manages a hexmon instance.
oo::class create aiplayer {
    mixin CommandInvoking
    variable comm err die dead

    constructor {color size path args} {
        set die 0
        set dead 0

        my AddCommands move quit out err
        set opts [my ConfigureCommands 0 {*}$args]

        foreach {err w} [chan pipe] {}
        set comm [open |[list hexmon $size $color $path {*}$opts 2>@$w] r+]
        autohup create job [pid $comm]
        close $w

        fconfigure $comm -buffering line -blocking 0
        fileevent $comm readable [list [self namespace]::my OnComm]

        fconfigure $err -buffering line -blocking 0
        fileevent $err readable [list [self namespace]::my OnErr]
    }

    destructor {
        # This terminates the AI process directly. Generally, prefer the quit
        # method, which asks the AI process to terminate nicely.

        if {!$die && $comm} {
            fileevent $comm readable {}
            fconfigure $comm -blocking 1
            close $comm
        }

        job destroy
    }

    method turn {board} {
        if {$comm ne ""} {
            puts $comm "move [$board dump]"
        } else {
            error "AI [self] dead"
        }
    }

    method postable {} { return 0 }

    method quit {board} {
        if {$comm ne ""} {
            set die 1
            puts $comm "quit [$board dump]"
            vwait [self namespace]::dead
            my InvokeCommand out "terminated"
            tailcall my destroy
        } else {
            error "AI [self] dead"
        }
    }

    method OnComm {} {
        if {[eof $comm]} {
            if {$die} {
                close $comm
                set comm ""
                if {$err eq ""} { set dead 1 }
            } else {
                error "AI [self] dead"
            }
        } else {
            set line [gets $comm]
            if {$line ne ""} {
                my InvokeCommand move $line
            }
        }
    }

    method OnErr {} {
        if {[eof $err]} {
            close $err
            set err ""
            if {$comm eq ""} { set dead 1 }
        } else {
            set line [gets $err]
            if {$line ne ""} {
                my InvokeCommand err $line
            }
        }
    }
}

# Class for human players.
oo::class create humanplayer {
    mixin CommandInvoking
    variable playing

    constructor {color size args} {
        set playing 0
        my AddCommands move quit out err
        my configure {*}$args
    }

    method turn {board} {
        set playing 1
    }

    method quit {board} {
        my InvokeCommand quit
        my destroy
    }

    method postable {} { return $playing }

    method post {m} {
        if {$playing} {
            set playing 0
            my InvokeCommand move $m
        } else {
            error "Human [self] does not have turn"
        }
    }
}

# A Hex gameplay session. blue and red should be either the path to an AI, or
# an empty string to indicate a human player.
oo::class create hexsession {
    mixin CommandInvoking

    constructor {size first bluepath blueopts redpath redopts args} {
        my AddCommands accept win
        my configure {*}$args

        hexgame create game $size $first \
            -acceptcommand [list [self namespace]::my OnAccept] \
            -wincommand [list [self namespace]::my OnWin]

        my CreatePlayer blue $bluepath $blueopts
        my CreatePlayer red $redpath $redopts
    }

    destructor {
        catch {blue destroy}
        catch {red destroy}
        game destroy
    }

    method red {} { return [self namespace]::red }
    method blue {} { return [self namespace]::blue }
    forward turnof game turnof
    forward dump game dump
    forward getcolor game get
    forward size game size

    method start {} {
        [my turnof] turn [self]
    }

    method end {{winner {}}} {
        foreach c {blue red} { $c quit [self] }
        my InvokeCommand win $winner
    }

    method postable {color} {
        set postable 0
        catch { set postable [$color postable] }
        return $postable
    }

    method CreatePlayer {color path opts} {
        if {$path eq ""} {
            humanplayer create $color $color [game size] \
                -movecommand [list [self namespace]::my OnMove $color]
        } else {
            aiplayer create $color $color [game size] $path \
                -movecommand [list [self namespace]::my OnMove $color] \
                {*}$opts
        }
    }

    forward OnMove game move
    forward OnWin my end
    method OnAccept {color move} {
        my InvokeCommand accept $color $move
        if {[game turnof] ne "done"} {
            [game turnof] turn [self]
        }
    }
}
