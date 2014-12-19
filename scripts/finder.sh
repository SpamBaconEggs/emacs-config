#! /usr/bin/env bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

find "$DIR" \
    -type d -name '.git*' -prune -o \
    -type d -name '.svn*' -prune -o \
    -type d -name 'objects' -prune -o \
    -type d -name 'build' -prune -o \
    -type d -name 'bin' -prune -o \
    -type d -name 'out' -prune -o \
    -type d -name 'lib' -prune -o \
    -type l -prune -o \
    -type f -and -name '*.h' -print -o \
    -type f -and -name '*.hpp' -print -o \
    -type f -and -name '*.c' -print -o \
    -type f -and -name '*.cpp' -print -o \
    -type f -and -name '*.cc' -print -o \
    -type f -and -name '*.py' -print -o \
    -type f -and -name '*.lua' -print -o \
    -type f -and -name '*.xml' -print > ./tmpcscope.files

# CSCope doesn't handle spaces in file names well, so quote
# strings (sure 'find' can do this, but can't be bothered to
# figure out how)
cat ./tmpcscope.files | sed 's/\(.*\)/"\1"/' > cscope.files
rm ./tmpcscope.files

cscope -b -q -k
#Use this to unconditionally rebuild the cross-reference file
#cscope -b -q -k -u

