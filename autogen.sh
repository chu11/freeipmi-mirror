#!/bin/sh
aclocal
autoheader
libtoolize --force --automake --copy
automake --foreign --include-deps --copy --add-missing
autoconf
