#!/bin/bash
# find . -name *.cs -exec bash replace_license.sh {} \;

SRC_FILE="$1"
DEST_FILE="$1".bak
TEMP_FILE="$1".tmp

usage()
{
    echo "script need one parameter."
    echo "usage: `basename $0` filename"
}

if [ $# -ne 1 ]
then
    usage
    exit 1
fi 
    
combine_continued_empty_line()
{
    #combine continued blank line to one  blank line 
    cat $1 | sed -e '/^[ \t\r\n]*$/{N;/^[ \t\r\n]*$/D}' >$DEST_FILE
    mv $DEST_FILE $1
}

if [ -f $DEST_FILE ]; then
    rm -f $DEST_FILE
fi

# some function comment is not start by *, mostly is start with 8 space
cat $1 | sed -e '/#region License/,/#endregion/ s:.*::' > $DEST_FILE

echo "#region License

// Copyright (c) 2011 - 2014, **** Inc.
// All rights reserved.
// http://www.****.com

#endregion

" > $TEMP_FILE

cat $DEST_FILE >> $TEMP_FILE

combine_continued_empty_line $TEMP_FILE $DEST_FILE

mv $TEMP_FILE $SRC_FILE