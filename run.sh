#!/bin/bash

CP=/tmp/batch-release
if [ -d $CP ]; then
	rm -fR $CP
fi
mkdir -p $CP

echo Compile into $CP
rm -f $CP/*

scalac /home/bruno/development/eclipse/batch-release/*.scala -d $CP

scala -cp $CP BatchRelease
