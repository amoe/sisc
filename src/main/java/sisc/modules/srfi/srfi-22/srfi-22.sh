#!/bin/bash
SCHEMEENV=`basename $0`
SOURCEFILE=$1

if [ -z "$SOURCEFILE" ]; then
    echo "$SCHEMEENV: cannot be used interactively." >&2
    echo "Please provide a Scheme source file on the command line." >&2
    exit 64    # EX_USAGE
fi

sisc -x -e "(srfi-22-prepare '$SCHEMEENV \"$SOURCEFILE\")" -c main-hook -- "$@"

