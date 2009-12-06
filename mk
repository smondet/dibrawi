#! /bin/bash

build ()
{
    export BRACETAX_LIB_PATH=$PWD/../bracetax/_build/src/lib/
    export SEBIB_LIB_PATH=$PWD/../sebib/_build/lib/
    omake
    rm -f dbw && ln -s _build/app/dbw
    rm -f dbwpp && ln -s _build/app/dbwpp
}

echo_help ()
{
    echo "\
$0 <cmd>
b: Build all (default action)
c: Clean
h: This help"
}

if [ $# -eq 0 ]; then
    build "opt"
    exit $?
fi

for todo in $* ; do
    case "$todo" in
        "b" ) build ;;
        #"bg" ) build "debug" ;;
        #"o" ) build "opt" ;;
        "c" ) omake clean ;;
        "h" ) echo_help ;;
        * ) echo "see \`mk h\`";;
    esac
done


