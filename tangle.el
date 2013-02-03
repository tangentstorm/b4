#!/bin/sh
# -*- mode: shell-script -*-
#
# tangle files with org-mode
#
DIR=`pwd`
FILES=""

# wrap each argument in the code required to call tangle on it
for i in $@; do
         FILES="$FILES \"$i\""
done

emacs -Q --batch \
--eval "(progn
     (add-to-list 'load-path (expand-file-name \"~/src/org/lisp/\"))
     (add-to-list 'load-path (expand-file-name \"~/src/org/contrib/lisp/\" t))
     (require 'org)(require 'org-exp)(require 'ob)(require 'ob-tangle)
     (mapc (lambda (file)
            (find-file (expand-file-name file \"$DIR\"))
            (org-babel-tangle)
            (kill-buffer)) '($FILES)))" 2>&1
