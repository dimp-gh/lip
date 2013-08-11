#!/bin/bash
if [ ! -f $1 ]
then
    echo "File $1 doesn't exist"
else
    swipl -s src/lisp.pl -g true -t "parse_transform(\"$1\")" --quiet
fi
