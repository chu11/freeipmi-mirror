#!/bin/sh
set -e
set -x
aclocal -I config
autoheader
libtoolize --force --automake --copy
automake --gnu --include-deps --copy --add-missing
autoconf
