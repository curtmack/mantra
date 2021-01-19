#!/bin/sh

GUILE=@GUILE@
GUILE_LIBS=@prefix@/share/guile/site/@GUILE_EFFECTIVE_VERSION@/

$GUILE -L $GUILE_LIBS -c '(begin (use-modules (mantra-libs main)) (main (command-line)))' "$@"
