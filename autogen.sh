#!/bin/sh
cvs2cl
aclocal
autoheader
libtoolize --force --automake
automake --add-missing
autoconf
