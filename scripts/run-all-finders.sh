#! /usr/bin/env bash

# Updates cscope database files for all projects with 'finder.sh'
# files under the /workspace/fanner directory.

# use 'read' to handle spaces in filenames (doesn't handle newlines,
# but see http://stackoverflow.com/a/7039579/601626 if you really need
# to do that)
find /workspace/fanner/ -maxdepth 2 -name 'finder.sh' -and -type f -print \
    | while read f
do
    echo "$f"
    # ... loop body
done
