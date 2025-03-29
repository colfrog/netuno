#!/bin/sh
sbcl --non-interactive --load netuno.asd --eval '(ql:quickload :netuno)' --eval '(asdf:make :netuno)' --eval '(quit)'
