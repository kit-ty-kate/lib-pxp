#! /bin/sh

# This version of setup.sh assumes O'Caml 3.03.

#set -x

# --- defaults ---

prefix=/usr/local
bindir=bin
mandir=man
manext=1
batch=0

ocamlrename=ocamlrun

# --- messages ---

usage () {
    cat <<EOF
usage: ./setup.sh [ options ]

Sets up the application and installs it. 

Options:
  -prefix p:
      Sets the common prefix of the "bin" and "man" directories
      (default: $prefix)

  -bindir d:
      Sets the location of the directory containing executables
      (default: PREFIX/$bindir)

  -mandir d:
      Sets the location of the directory containing manpage sections
      (default: PREFIX/$mandir)

  -manext s:
      Sets the extension of the manual page section
      (default: $manext)

  -batch:
      Don't ask the user interactively and use only the values from
      the command line

  -help:
      Prints this help message
EOF
}

# --- argument parser ---

parseargs () {
    while [ $# -gt 0 ]; do
	case "$1" in
	    -prefix|--prefix)
		prefix="$2"
		shift 
		shift ;;
	    -bindir|--bindir)
		bindir="$2"
		shift
		shift ;;
	    -mandir|--mandir)
		manddir="$2"
		shift
		shift ;;
	    -manext|--manext)
		manext="$2"
		shift
		shift ;;
	    -help|--help)
		usage
		exit ;;
	    *)
		echo "Don't know what to to with $1!"
		usage 
		exit 1 ;;
	esac
    done
}

# --- argument confirmation ---

boolconfirm () {
    # $1: argument name (e.g. prefix)
    # $2: explanations
    test $batch -eq 0 || return 0          # means yes
    echo
    echo "$2"
    eval "v=\$$1"

    answer=""
    while [ -z "$answer" ]; do
	printf "[%s] (yes/no)? " $v
	read a
	if [ -z "$a" ]; then a=$v; fi
	case "$a" in
	    yes|y|YES|Y)
		answer=1 ;;
	    no|n|NO|N)
		answer=0 ;;
	    *)
		echo "Please answer yes or no!" ;;
	esac
    done
    
    test "$answer" -gt 0      # and return the exit code of this test
}


confirm () {
    # $1: argument name (e.g. prefix)
    # $2: explanations
    test $batch -eq 0 || return 0          # no modification
    echo
    echo "$2"
    eval "v=\$$1"

    ok=0
    while [ $ok -eq 0 ]; do
	case "$v" in
	    /*)
		printf "[%s] " $v ;;
	    *)
		printf "[%s/%s] " $prefix $v ;;
	esac
	read a
	case "$a" in
	    /*) ok=1 ;;
	    *)  
		if [ -n "$a" ]; then
		    echo "Please enter an absolute path!"
		    ok=0
		else
		    ok=1
		fi ;;
	esac
    done

    if [ -n "$a" ]; then
	eval "$1='$a'"
    fi
}

# --- argument rewriting ---

fixpath () {
    # $1: name of the argument (e.g. "prefix")
    eval "v=\$$1"
    case "$v" in
	/*) : ;;
	*)
	    v="$prefix/$v" 
	    eval "$1='$v'"
	    ;;
    esac
}

# --- run ---

# Split $PATH into words:
oldifs="$IFS"
IFS="   :"
spacepath=`echo $PATH`
IFS="$oldifs"


in_path () {
    # Does $1 exist in $PATH?
    for d in $spacepath; do
        if test -x "$d/$1"; then
            return 0
        fi
    done
    return 1
}


get_path () {
    for d in $spacepath; do
        if test -x "$d/$1"; then
            echo "$d/$1"
            return
        fi
    done
}


run () {
    # $@: command to execute
    echo "$@"
    eval "$@" || { r=$?; echo "*** Installation failed"; exit $r; }
}

# --- main ---

parseargs "$@"

if [ $batch = 0 ]; then
    clear 2>/dev/null
    cat <<EOF
                        INSTALLATION OF PXPVALIDATE

EOF
fi

# CHECKS:
if in_path $ocamlrename; then
    ocamlre=`get_path $ocamlrename`
    echo "ocamlrun found: $ocamlre"
else
    echo "Sorry, you need an O'Caml installation to execute this application."
    echo "(Or the command 'ocamlrun' is not in your PATH.)"
    echo "Get version 3.03-alpha at:"
    echo "http://caml.inria.fr"
    exit
fi

# CHECK THE VERSION OF OCAMLRE:
if $ocamlre -p | grep -s dynlink_open_lib >/dev/null; then
    echo "Your version of ocamlrun supports dynamic loading"
else
    echo "Sorry, you need an O'Caml version that supports dynamic loading of"
    echo "libraries."
    exit
fi

# CHECK WHETHER PXPVALIDATE CAN BE STARTED:
if $ocamlre ./pxpvalidate.byte >/dev/null 2>/dev/null; then
    echo "Your version of ocamlrun can execute pxpvalidate"
else
    echo "Sorry, your version of O'Caml is incompatible with the bytecode"
    echo "format used here."
    exit
fi

confirm "prefix" "Please enter the directory where to install the software:"

confirm "bindir" "Enter the directory where to install binaries:"

confirm "mandir" "Enter the directory where to install manual pages:"

fixpath bindir
fixpath mandir

if [ $batch = 0 ]; then
    cat <<EOF

	SUMMARY:

	Prefix:       $prefix
	Binaries:     $bindir
	Manual pages: $mandir
EOF
fi

continue="yes"

if boolconfirm "continue" "Continue with the installation?"; then
    if [ $batch -eq 0 ]; then
	echo "-------------------------------------------------------------"
	echo "Installation script:"
    fi
    run mkdir -p $bindir
    run mkdir -p $mandir/man$manext
    run echo "'#! $ocamlre'" ">$bindir/pxpvalidate"
    run cat pxpvalidate.byte ">>$bindir/pxpvalidate"
    run chmod 755 $bindir/pxpvalidate
    run cp pxpvalidate.1 $mandir/man$manext/pxpvalidate.1
    echo "Installation complete!"
else
    echo "Installation canceled!"
fi
