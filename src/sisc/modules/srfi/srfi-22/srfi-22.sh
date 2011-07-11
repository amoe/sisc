#!/bin/bash
SCHEMEENV=`basename $0`
SOURCEFILE=$1

sisc -x -e "(srfi-22-prepare '$SCHEMEENV \"$SOURCEFILE\")" -c main-hook -- "$@"

