#!/bin/sh
# Copy the python extension files in the current directory
# into the pylib area in $IGNITION_HOME. These files stub
# out database interactions and simply print the module names.
#
DIR=`pwd`
cp ${DIR}/*.py ${IGNITION_HOME}/user-lib/pylib/ils/extensions

