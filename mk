#! /bin/bash

build ()
{
    local TAGOPT="-tags pkg_unix"
    local I_OPT="-I src/app -I src/lib"
    local FLAGS="-cflags -dtypes"
    ocamlfind batteries/ocamlbuild $I_OPT $TAGOPT $FLAGS src/app/html_menu$1.byte libdibrawi.cma
    rm -f html_menu && ln -s html_menu$1.byte html_menu
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


