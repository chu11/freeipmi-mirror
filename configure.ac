dnl Process this file with autoconf to produce a configure script.
AC_INIT([freeipmi],[0.2.beta1],[freeipmi-devel@gnu.org])
AC_REVISION([$Id: configure.ac,v 1.38.2.1 2006-01-20 01:34:52 chu11 Exp $])
AM_INIT_AUTOMAKE([1.8])

dnl We require a modern autoconf and automake
AC_PREREQ([2.57])

dnl Locate the sources.
AC_CONFIG_SRCDIR([fish/fish.scm])

dnl create a config.h file
AM_CONFIG_HEADER([config.h])

dnl define output files
AC_CONFIG_FILES([
        Makefile doc/Makefile doc/examples/Makefile 
	common/Makefile common/doc/Makefile common/src/Makefile
	fish/Makefile fish/bmc-config.8.pre fish/fish.8.pre fish/ipmi-sel.8.pre
	fish/ipmi-sensors.8.pre fish/bmc-config.conf.5 
	fish/src/Makefile fish/extensions/Makefile fish/scripts/Makefile
	fish/scripts/sensors/Makefile fish/scripts/sensors/ipmi-sensors
	fish/scripts/bmc-config/Makefile fish/scripts/bmc-config/bmc-config 
	fish/scripts/bmc-autoconfig/Makefile
	fish/scripts/sel/Makefile fish/scripts/sel/ipmi-sel 
	fish/scripts/pef/Makefile fish/scripts/pef/ipmi-pef 
        libfreeipmi/Makefile libfreeipmi/src/Makefile
        bmc-watchdog/Makefile bmc-watchdog/src/Makefile
        ipmipower/Makefile ipmipower/src/Makefile
        ipmi-raw/Makefile ipmi-raw/src/Makefile 
	ipmi-raw/doc/Makefile ipmi-raw/doc/ipmi-raw.8.pre
        ipmi-locate/Makefile ipmi-locate/src/Makefile
        bmc-info/Makefile bmc-info/src/Makefile
	bmc-info/doc/Makefile bmc-info/doc/bmc-info.8.pre
        ipmiping/Makefile ipmiping/src/Makefile
        rmcpping/Makefile rmcpping/src/Makefile
	garpd/Makefile garpd/src/Makefile garpd/garpd.8
        bmc-watchdog/bmc-watchdog.8 ipmipower/ipmipower.8 ipmipower/ipmipower.conf.5
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

AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET
AM_CONDITIONAL(WITH_GNU_LD, test "$with_gnu_ld" = "yes")

dnl Enable error tracing. Lower level calls will spit error messages
dnl on to the console
AC_ARG_ENABLE([trace],
              AC_HELP_STRING([--enable-trace], [turn on error tracing]))
if test "x${enable_trace}" = "xyes" ; then
     AC_DEFINE([IPMI_TRACE],[1],[Define to turn on error tracing])
elif test "x${enable_trace}" = "x" ; then
     enable_trace=no
fi

dnl Enable error syslogging. Lower level calls will spit log error messages
dnl to the syslog.
AC_ARG_ENABLE([syslog],
              AC_HELP_STRING([--enable-syslog], [turn on error syslogging]))
if test "x${enable_syslog}" = "xyes" ; then
     AC_DEFINE([IPMI_SYSLOG],[1],[Define to turn on error syslogging])
elif test "x${enable_syslog}" = "x" ; then
     enable_syslog=no
fi

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
   if test "x${enable_debug}" = "x" ; then
      enable_debug=no
   fi
   AC_DEFINE([NDEBUG], [1], [Define to 1 for a production release])
   WITH_DEBUG=0
fi
AC_SUBST(WITH_DEBUG)

dnl See if they want to build fish.
AC_ARG_ENABLE([fish],
              AC_HELP_STRING([--disable-fish], [don't build the fish IPMI shell]))
AM_CONDITIONAL([FISH], [test x"${enable_fish}" != "xno"])
if test "x${enable_fish}" = "x" ; then
   enable_fish=yes
fi

if test "x${enable_fish}" = "xyes" ; then
   GUILE_PROGS
   GUILE_FLAGS

   # Check for minimum required Guile version:
   GUILE_REQUIRED_MINIMUM_VERSION="1.6.4"
   GUILE_VERSION=`guile -c "
   (define (guile-current-version>=? min-required-version)
   \"return TRUE if current-version >= VER\"
   (let
      ((installed-major (car (string-split (version) #\.)))
       (installed-minor (cadr (string-split (version) #\.)))
       (installed-micro (caddr (string-split (version) #\.)))
       (required-major (car (string-split min-required-version #\.)))
       (required-minor (cadr (string-split min-required-version #\.)))
       (required-micro (caddr (string-split min-required-version #\.))))
       (if (and (string-ci>=? installed-major required-major)
                (string-ci>=? installed-minor required-minor)
                (string-ci>=? installed-micro required-micro))
           (display \"true\")
           (display \"false\"))))
   (guile-current-version>=? \"$GUILE_REQUIRED_MINIMUM_VERSION\")"`
   if test "$GUILE_VERSION" != true; then
      AC_MSG_ERROR([Installed version of GNU Guile is too old. You need "$GUILE_REQUIRED_MINIMUM_VERSION" or higher])
   fi

   NOREADLINE_LIBS=${LIBS}
   LIBS=${READLINE_LIBS}
   AC_SEARCH_LIBS([tgetent],[ncurses termcap curses])
   AC_CHECK_LIB([readline],[readline])
   have_ding=no
   AC_CHECK_FUNCS([ding rl_ding],[have_ding=yes])
   have_completion_matches=no
   AC_CHECK_FUNCS([completion_matches rl_completion_matches],[have_completion_matches=yes])
   if test "x${have_ding}" = "xno" -o "x${have_completion_matches}" = "xno" ; then
      AC_MSG_ERROR([fish build requested but no readline functions found])
   fi
   READLINE_LIBS=${LIBS}
   AC_SUBST([READLINE_LIBS])
   LIBS=${NOREADLINE_LIBS}
else
   GUILE_CFLAGS=
   AC_SUBST([GUILE_CFLAGS])
   GUILE_LDFLAGS=
   AC_SUBST([GUILE_LDFLAGS])
   READLINE_LIBS=
   AC_SUBST([READLINE_LIBS])
fi

dnl Checks for libraries.

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

dnl Check for random number paths
AC_CHECK_FILE([/dev/urandom], AC_DEFINE([HAVE_DEVURANDOM], [1], [Define that you found /dev/urandom]))
AC_CHECK_FILE([/dev/random], AC_DEFINE([HAVE_DEVRANDOM], [1], [Define that you found /dev/urandom]))

AC_OUTPUT
