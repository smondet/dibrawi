#! /bin/bash

BRACETAX_LIB_PATH=../bracetax/_build/src/lib/


build ()
{
    local TAGOPT="-tags pkg_unix"
    local I_OPT="-I src/app -I src/lib"
    local BRTXOPT="-cflags -I -cflags ../$BRACETAX_LIB_PATH \
                   -lflags -I -lflags ../$BRACETAX_LIB_PATH"
    local FLAGS="-cflags -dtypes -lib ocamlbracetax"
    local ALL_FLAGS="$I_OPT $TAGOPT $BRTXOPT $FLAGS"
    ocamlfind batteries/ocamlbuild $ALL_FLAGS src/app/test_dbw$1.byte libdibrawi.cma
    rm -f test_dbw && ln -s test_dbw$1.byte test_dbw
}

echo_help ()
{
    echo "\
$0 <cmd>
b: Build all (default action)
bg: Build all with debug symbols
c: Clean
h: This help"
}

if [ $# -eq 0 ]; then
    build
    exit $?
fi

for todo in $* ; do
    case "$todo" in
        "b" ) build ;;
        "bg" ) build ".d" ;;
        "c" ) ocamlbuild -clean ;;
        "h" ) echo_help ;;
        * ) echo "see \`mk h\`";;
    esac
done


