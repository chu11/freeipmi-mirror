dnl Process this file with autoconf to produce a configure script.
AC_INIT([freeipmi],[0.1.0],[freeipmi-devel@nongnu.org])
AC_REVISION([$Id: configure.ac,v 1.4 2004-05-07 20:56:42 chu11 Exp $])
AM_INIT_AUTOMAKE

dnl We require a modern autoconf.
AC_PREREQ([2.57])

dnl Locate the sources.
AC_CONFIG_SRCDIR([fish/fish.scm])

dnl create a config.h file
AM_CONFIG_HEADER([config.h])

dnl define output files
AC_CONFIG_FILES([
        Makefile doc/Makefile doc/examples/Makefile fish/Makefile fish/src/Makefile
        fish/extensions/Makefile fish/scripts/Makefile fish/scripts/sensors/Makefile
        fish/scripts/sensors/sensors fish/scripts/bmc-config/Makefile
        fish/scripts/bmc-config/bmc-config fish/scripts/sel/Makefile
        fish/scripts/sel/sel fish/scripts/bmc-info/Makefile
        fish/scripts/bmc-info/bmc-info 
        libfreeipmi/Makefile libfreeipmi/src/Makefile
        bmc-watchdog/Makefile bmc-watchdog/src/Makefile
        doc/bmc-watchdog.8])

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
              AC_HELP_STRING([--enable-trace], [turn on error tracing]))
if test "x${enable_trace}" = "xyes" ; then
        AC_DEFINE([IPMI_TRACE],[1],[Define to turn on error tracing])
fi

AC_ARG_ENABLE([rpm-install],
              AC_HELP_STRING([--enable-rpm-install], 
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

dnl By default AC_PROG_CC sets CFLAGS="-g -O2". Turn off debugging if
dnl not explicitly requested.
AC_ARG_ENABLE(debug,
              AC_HELP_STRING([--enable-debug], [turn on debugging]),
              [
               if test "$GCC" = "yes"; then 
                  CFLAGS="-ggdb -Wall"
               elif test "$ac_cv_prog_cc_g" = "yes"; then
                  CFLAGS="-g"
               fi
               WITH_DEBUG=1
              ],
              [
               AC_DEFINE([NDEBUG], [1], 
                         [Define to 1 for a production release])
               CFLAGS="-O2"
               WITH_DEBUG=0
              ])
AC_SUBST(WITH_DEBUG)

dnl Checks for libraries.
AC_SEARCH_LIBS([tgetent],[ncurses termcap curses])
AC_CHECK_LIB([readline],[readline])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_BIGENDIAN

dnl Checks for library functions.
AC_CHECK_FUNCS([ding rl_ding completion_matches rl_completion_matches strchr memcpy memset getopt_long])

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([unistd.h fcntl.h])

AC_OUTPUT
