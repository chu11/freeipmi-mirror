dnl Process this file with autoconf to produce a configure script.
AC_INIT([freeipmi],[0.1.0],[freeipmi-devel@nongnu.org])
AC_REVISION([$Id: configure.ac,v 1.1 2004-05-05 21:00:18 itz Exp $])
AM_INIT_AUTOMAKE

dnl We require a modern autoconf.
AC_PREREQ([2.59])

dnl Locate the sources.
AC_CONFIG_SRCDIR([fish/fish.scm])

dnl create a config.h file
AC_CONFIG_HEADERS([config.h])

dnl define output files
AC_CONFIG_FILES([
        Makefile doc/Makefile doc/examples/Makefile fish/Makefile fish/src/Makefile
        fish/extensions/Makefile fish/scripts/Makefile fish/scripts/sensors/Makefile
        fish/scripts/sensors/sensors fish/scripts/bmc-config/Makefile
        fish/scripts/bmc-config/bmc-config fish/scripts/sel/Makefile
        fish/scripts/sel/sel fish/scripts/bmc-info/Makefile
        fish/scripts/bmc-info/bmc-info fish/fish.lsm fish/fish.spec 
        libfreeipmi/Makefile libfreeipmi/src/Makefile libfreeipmi/libfreeipmi.lsm 
        libfreeipmi/libfreeipmi.spec])

ISODATE=`date +%Y-%m-%d`
AC_SUBST([ISODATE])

LIBFREEIPMI_CURRENT=0
LIBFREEIPMI_REVISION=0
LIBFREEIPMI_AGE=0

LIBFREEIPMI_VERSION=$LIBFREEIPMI_CURRENT.$LIBFREEIPMI_REVISION.$LIBFREEIPMI_AGE
AC_SUBST([LIBFREEIPMI_VERSION])
LIBFREEIPMI_VERSION_INFO=$LIBFREEIPMI_CURRENT:$LIBFREEIPMI_REVISION:$LIBFREEIPMI_AGE
AC_SUBST([LIBFREEIPMI_VERSION_INFO])

dnl Enable error tracing. Lower level calls will spit error messages
dnl on to the console
AC_ARG_ENABLE([trace],
              AS_HELP_STRING([--enable-trace], [turn on error tracing]))
if test "x${enable_trace}" = "xyes" ; then
        AC_DEFINE([IPMI_TRACE],[1],[Define to turn on error tracing])
fi

AC_ARG_ENABLE([rpm-install],
              AS_HELP_STRING([--enable-rpm-install], 
                             [perform unique installation for building rpms]),
              [RPM_INSTALL=${enableval}],
              [RPM_INSTALL=no])
AM_CONDITIONAL([RPM_INSTALL], [test "$RPM_INSTALL" = "yes"])

AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET
GUILE_PROGS
GUILE_FLAGS

dnl Checks for libraries.
AC_SEARCH_LIBS([tgetent],[ncurses termcap curses])
AC_CHECK_LIB([readline],[readline])

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.
AC_CHECK_FUNCS([ding rl_ding completion_matches rl_completion_matches strchr memcpy memset])

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([unistd.h fcntl.h])

AC_OUTPUT
