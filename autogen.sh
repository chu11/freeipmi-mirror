#!/bin/sh
aclocal
autoheader
libtoolize --force --automake --copy
automake --gnu --include-deps --copy --add-missing
autoconf
