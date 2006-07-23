#!/bin/sh
set -e
set -x
aclocal -I .
autoheader
libtoolize --force --automake --copy
automake --gnu --include-deps --copy --add-missing
autoconf
