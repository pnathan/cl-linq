#!/bin/bash

if [ `which sbcl` != "" ]; then
    sbcl --non-interactive --load check-systems.lisp
fi

#if [ `which abcl` != "" ]; then
if [ 1 == 1 ]; then
    java -jar ~/abcl/abcl/abcl.jar --batch --load check-systems.lisp
fi

if [ `which clisp` != "" ]; then
    clisp -i ~/.clisprc check-systems.lisp
fi
