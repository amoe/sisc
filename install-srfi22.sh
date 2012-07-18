#!/bin/sh -

if [ -z "$1" ]
then
  USERID=`id -u`
  if [ $USERID != 0 ]
  then
    echo This script must be run as root if installing to the default path.
    exit 1
  fi
  INSTALL_DIR=/usr/local/bin
else
  INSTALL_DIR=`cd "$1" && pwd || echo $1` # bad-dir error caught below
fi
if [ -z "$2" ]
then
  SRCDIR="scheme-src/srfi-22"
else
  SRCDIR=`cd "$2" && pwd || echo $2`
fi
if [ -z "$3" ]
then
  SHAREDIR=.
else
  SHAREDIR=$3
fi


echo "This script will install the srfi-22 support scripts in $INSTALL_DIR."
echo "If this is acceptable, press enter now, otherwise press CTL-C."
echo ""
echo "You can call this script with a different install dir as an argument."
read

test -d "$SRCDIR"      || { echo "No source directory $SRCDIR">&2; exit 1; }
test -d "$INSTALL_DIR" || { echo "No install dir $INSTALL_DIR">&2; exit 1; }

cd "$INSTALL_DIR"
ln -sf "$SHAREDIR/srfi-22.sh" scheme-r4rs
ln -sf "$SHAREDIR/srfi-22.sh" scheme-r5rs
ln -sf "$SHAREDIR/srfi-22.sh" scheme-srfi-0
ln -sf "$SHAREDIR/srfi-22.sh" scheme-srfi-7
ln -sf "$SHAREDIR/srfi-22.sh" scheme-srfi-55
ln -sf "$SHAREDIR/srfi-22.sh" scheme-ieee-1178-1900

exit 0
