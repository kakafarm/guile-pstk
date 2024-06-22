package require Tk
if {[package version tile] != ""} {
    package require tile
}

namespace eval AutoName {
    # This closure is used to return names that are never the same.
    # Names are either the argument provided to autoName and the
    # counter, or the default ## and the counter.
    #
    # Examples:
    # % # When the argument is "moo":
    # % ::AutoName::autoName moo #
    # moo1
    # % # When there are no provided arguments, the default "##" one is used:
    # % ::AutoName::autoName
    # ##2
    variable c 0
    proc autoName {{providedName \#\#}} {
        variable c
        append providedName [incr c]
    }
    namespace export *
}

namespace import AutoName::*

proc callToScm {callKey args} {
    global scmVar
    if { [catch {
            set resultKey [autoName]
            puts "(call $callKey \"$resultKey\" $args)"
            flush stdout
            vwait scmVar($resultKey)
            set result $scmVar($resultKey)
            unset scmVar($resultKey)
            set result
        } ]
    } { exit 1 }
}

proc tclListToScmList {l} {
    switch [llength $l] {
        0 {
            return ()
        }
        1 {
            if {[string range $l 0 0] eq "\#"} {
                return $l
            }
            if {[regexp {^[0-9]+$} $l]} {
                return $l
            }
            if {[regexp {^[.[:alpha:]][^ ,\"\'\[\]\\;]*$} $l]} {
                return $l
            }
            set result \"
            append result\
                [string map [list \" \\\" \\ \\\\] $l]
            append result \"

        }
        default {
            set result {}
            foreach el $l {
                append result " " [tclListToScmList $el]
            }
            set result [string range $result 1 end]
            return "($result)"
        }
    }
}

proc evalCmdFromScm {cmd {properly 0}} {
    if {[catch {
        set result [uplevel \#0 $cmd]
    } err]} {
        puts "(error \"[string map [list \\ \\\\ \" \\\"] $err]\")"
    } elseif $properly {
        puts "(return [tclListToScmList $result])"
    } else {
        puts "(return \"[string map [list \\ \\\\ \" \\\"] $result]\")"
    }
    flush stdout
}
