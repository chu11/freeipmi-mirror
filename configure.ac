
dnl Process this file with autoconf to produce a configure script.
AC_INIT([freeipmi],[0.3.beta0],[freeipmi-devel@gnu.org])
AC_REVISION([$Id: configure.ac,v 1.65 2006-08-07 21:14:13 chu11 Exp $])
AM_INIT_AUTOMAKE([1.8])

dnl We require a modern autoconf and automake
AC_PREREQ([2.57])

dnl Locate the sources.
AC_CONFIG_SRCDIR([ChangeLog.0])

dnl create a config.h file
AM_CONFIG_HEADER([config.h])

dnl define output files
AC_CONFIG_FILES([
        Makefile doc/Makefile doc/examples/Makefile 
	common/Makefile common/doc/Makefile common/src/Makefile
        libfreeipmi/Makefile libfreeipmi/include/Makefile 
	libfreeipmi/src/Makefile libfreeipmi/src/udm/Makefile
        bmc-watchdog/Makefile bmc-watchdog/src/Makefile
        ipmipower/Makefile ipmipower/src/Makefile
        ipmi-raw/Makefile ipmi-raw/src/Makefile 
	ipmi-raw/ipmi-raw.8.pre
        ipmi-sel/Makefile ipmi-sel/src/Makefile 
	ipmi-sel/ipmi-sel.8.pre
        ipmi-sensors/Makefile ipmi-sensors/src/Makefile 
	ipmi-sensors/ipmi-sensors.8.pre
        ipmi-locate/Makefile ipmi-locate/src/Makefile
        bmc-info/Makefile bmc-info/src/Makefile
	bmc-info/bmc-info.8.pre
        ipmiping/Makefile ipmiping/src/Makefile
        rmcpping/Makefile rmcpping/src/Makefile
        bmc-watchdog/bmc-watchdog.8 ipmipower/ipmipower.8 ipmipower/ipmipower.conf.5
        ipmiping/ipmiping.8 rmcpping/rmcpping.8
	bmc-config/Makefile bmc-config/src/Makefile
	bmc-config/bmc-config.8.pre bmc-config/bmc-config.conf.5 
	bmc-config/bmc-autoconfig/Makefile
        freeipmi.spec])

ISODATE=`date +%Y-%m-%d`
AC_SUBST([ISODATE])

LIBFREEIPMI_CURRENT=3
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
AM_CONDITIONAL(WITH_GNU_LD, test "$with_gnu_ld" = "yes")
AC_PROG_MAKE_SET

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

dnl Checks for libraries.
AC_CHECK_LIB([gcrypt], [gcry_md_open], [have_gcrypt=yes], [have_gcrypt=no])
if test "x${have_gcrypt}" = "xno"; then
   AC_MSG_ERROR([libgcrypt required to build libfreeipmi])
   AC_MSG_NOTICE([Note: libgpg-error required for libgcrypt])
fi

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_BIGENDIAN

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MMAP
AC_CHECK_FUNCS([strchr memcpy memset getopt_long mlock])
AC_caolan_FUNC_WHICH_GETHOSTBYNAME_R

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([unistd.h fcntl.h getopt.h pthread.h])

dnl Macro definitions

dnl Check for random number paths
AC_CHECK_FILE([/dev/urandom], AC_DEFINE([HAVE_DEVURANDOM], [1], [Define that you found /dev/urandom]))
AC_CHECK_FILE([/dev/random], AC_DEFINE([HAVE_DEVRANDOM], [1], [Define that you found /dev/urandom]))

AC_OUTPUT
