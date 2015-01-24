#!/bin/sh

set -e

test `basename $PWD` != "c_src" && cd c_src

case "$1" in
    clean)
        make clean
        ;;

    *)
        make all && cp cookie_bouncer.so ../priv/
        ;;
esac
