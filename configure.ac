dnl Process this file with autoconf to produce a configure script.
AC_INIT([freeipmi],[0.1.1],[freeipmi-devel@gnu.org])
AC_REVISION([$Id: configure.ac,v 1.15 2004-10-20 23:21:22 itz Exp $])
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
        ipmipower/Makefile ipmipower/src/Makefile
        ipmiping/Makefile ipmiping/src/Makefile
        rmcpping/Makefile rmcpping/src/Makefile
        doc/version.texi bmc-watchdog/bmc-watchdog.8 ipmipower/ipmipower.8 ipmipower/ipmipower.conf.5
        ipmiping/ipmiping.8 rmcpping/rmcpping.8
        freeipmi.spec])

ISODATE=`date +%Y-%m-%d`
AC_SUBST([ISODATE])

LIBFREEIPMI_CURRENT=1
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

AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET
GUILE_PROGS
GUILE_FLAGS

dnl Turn off all optimizations when debugging
AC_ARG_ENABLE(debug,
              AC_HELP_STRING([--enable-debug], [turn on debugging]))
if test "x${enable_debug}" = "xyes"; then
   if test "$GCC" = "yes"; then 
      CFLAGS="-ggdb -Wall"
   elif test "$ac_cv_prog_cc_g" = "yes"; then
      CFLAGS="-g"
   fi
   WITH_DEBUG=1
else
   AC_DEFINE([NDEBUG], [1], [Define to 1 for a production release])
   WITH_DEBUG=0
fi
AC_SUBST(WITH_DEBUG)

dnl Checks for libraries.
NOREADLINE_LIBS=${LIBS}
LIBS=${READLINE_LIBS}
AC_SEARCH_LIBS([tgetent],[ncurses termcap curses])
AC_CHECK_LIB([readline],[readline])
AC_CHECK_FUNCS([ding rl_ding completion_matches rl_completion_matches])
READLINE_LIBS=${LIBS}
AC_SUBST([READLINE_LIBS])
LIBS=${NOREADLINE_LIBS}

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_BIGENDIAN

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MMAP
AC_CHECK_FUNCS([strchr memcpy memset getopt_long mlock])

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([unistd.h fcntl.h])

AC_OUTPUT
