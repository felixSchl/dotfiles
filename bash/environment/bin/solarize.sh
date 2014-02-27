#!/bin/zsh
#
# solarize.sh
#
# DESCRIPTION:
# Sets solarized theme using cofig tools appropriate for the current OS.
#
# USAGE:
# solarize [light|dark]
#

mode=$1
bin=~/environment/bin

case `uname` in
    Linux)
        $bin/linux_solarized_$mode.sh
        ;;
    CYGWIN_NT-6.1-WOW64)
        $bin/mintty_solarized_$mode.sh
        ;;
esac
