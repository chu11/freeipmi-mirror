dnl Process this file with autoconf to produce a configure script.
AC_INIT([freeipmi],[0.4.beta0],[freeipmi-devel@gnu.org])
AC_REVISION([$Id: configure.ac,v 1.88 2007-06-02 14:34:53 chu11 Exp $])
AM_INIT_AUTOMAKE([1.8])

dnl We require a modern autoconf and automake
AC_PREREQ([2.57])

dnl Locate the sources.
AC_CONFIG_SRCDIR([ChangeLog.0])

dnl create a config.h file
AM_CONFIG_HEADER([config.h])

dnl define output files
AC_CONFIG_FILES([
        Makefile doc/Makefile 
	common/Makefile common/doc/Makefile common/src/Makefile
        libfreeipmi/Makefile libfreeipmi/include/Makefile 
	libfreeipmi/src/Makefile libfreeipmi/src/udm/Makefile
        bmc-watchdog/Makefile bmc-watchdog/src/Makefile
        ipmipower/Makefile ipmipower/src/Makefile
	ipmiconsole/Makefile ipmiconsole/src/Makefile
	ipmiconsole/src/libipmiconsole/Makefile ipmiconsole/src/ipmiconsole/Makefile
	ipmimonitoring/Makefile ipmimonitoring/src/Makefile
	ipmimonitoring/src/libipmisdrcache/Makefile ipmimonitoring/src/libipmimonitoring/Makefile ipmimonitoring/src/ipmimonitoring/Makefile
        ipmidetect/Makefile 
        ipmidetect/src/Makefile
        ipmidetect/src/ipmidetect/Makefile
        ipmidetect/src/ipmidetectd/Makefile
        ipmidetect/src/libipmidetect/Makefile
        ipmi-raw/Makefile ipmi-raw/src/Makefile 
	ipmi-raw/ipmi-raw.8.pre
        ipmi-sel/Makefile ipmi-sel/src/Makefile 
	ipmi-sel/ipmi-sel.8.pre
        pef-config/Makefile pef-config/src/Makefile 
	pef-config/pef-config.8.pre
        ipmi-sensors/Makefile ipmi-sensors/src/Makefile 
	ipmi-sensors/ipmi-sensors.8.pre
        ipmi-locate/Makefile ipmi-locate/src/Makefile
        bmc-info/Makefile bmc-info/src/Makefile
	bmc-info/bmc-info.8.pre
        ipmiping/Makefile ipmiping/src/Makefile
        rmcpping/Makefile rmcpping/src/Makefile
        bmc-watchdog/bmc-watchdog.8 ipmipower/ipmipower.8 ipmipower/ipmipower.conf.5
	ipmiconsole/ipmiconsole.8 ipmiconsole/ipmiconsole.conf.5
        ipmiconsole/libipmiconsole.3
	ipmimonitoring/ipmimonitoring.8
        ipmimonitoring/libipmimonitoring.3
        ipmidetect/ipmidetect.8
        ipmidetect/ipmidetect.conf.5
        ipmidetect/ipmidetectd.8
        ipmidetect/ipmidetectd.conf.5
        ipmidetect/libipmidetect.3
        ipmiping/ipmiping.8 rmcpping/rmcpping.8
	bmc-config/Makefile bmc-config/src/Makefile
	bmc-config/bmc-config.8.pre bmc-config/bmc-config.conf.5 
	bmc-config/bmc-autoconfig/Makefile
	ipmi-chassis/Makefile
	ipmi-chassis/src/Makefile
	ipmi-chassis/ipmi-chassis.8.pre
        freeipmi.spec])

ISODATE=`date +%Y-%m-%d`
AC_SUBST([ISODATE])

# libfreeipmi versioning
#
# If library source code has changed at all increment rev
# (current:rev:age) = (current:rev+1:age)
#
# If any interfaces have been added,removed, or changed increment current
#  and set rev = 0;
#
# If any interfaces have been added increment age
#
# If any interfaces have been removed, set age = 0
#
LIBFREEIPMI_CURRENT=4
LIBFREEIPMI_REVISION=0
LIBFREEIPMI_AGE=0

LIBFREEIPMI_VERSION=$LIBFREEIPMI_CURRENT.$LIBFREEIPMI_REVISION.$LIBFREEIPMI_AGE
AC_SUBST([LIBFREEIPMI_VERSION])
LIBFREEIPMI_VERSION_INFO=$LIBFREEIPMI_CURRENT:$LIBFREEIPMI_REVISION:$LIBFREEIPMI_AGE
AC_SUBST([LIBFREEIPMI_VERSION_INFO])

# libipmiconsole versioning
#
# If library source code has changed at all increment rev
# (current:rev:age) = (current:rev+1:age)
#
# If any interfaces have been added,removed, or changed increment current
#  and set rev = 0;
#
# If any interfaces have been added increment age
#
# If any interfaces have been removed, set age = 0
#
LIBIPMICONSOLE_CURRENT=1
LIBIPMICONSOLE_REVISION=0
LIBIPMICONSOLE_AGE=0
LIBIPMICONSOLE_VERSION_INFO=$LIBIPMICONSOLE_CURRENT:$LIBIPMICONSOLE_REVISION:$LIBIPMICONSOLE_AGE
AC_SUBST([LIBIPMICONSOLE_VERSION_INFO])

# libipmimonitoring versioning
#
# If library source code has changed at all increment rev
# (current:rev:age) = (current:rev+1:age)
#
# If any interfaces have been added,removed, or changed increment current
#  and set rev = 0;
#
# If any interfaces have been added increment age
#
# If any interfaces have been removed, set age = 0
#
LIBIPMIMONITORING_CURRENT=0
LIBIPMIMONITORING_REVISION=0
LIBIPMIMONITORING_AGE=0
LIBIPMIMONITORING_VERSION_INFO=$LIBIPMIMONITORING_CURRENT:$LIBIPMIMONITORING_REVISION:$LIBIPMIMONITORING_AGE
AC_SUBST([LIBIPMIMONITORING_VERSION_INFO])

# libipmidetect versioning
#
# If library source code has changed at all increment rev
# (current:rev:age) = (current:rev+1:age)
#
# If any interfaces have been added,removed, or changed increment current
#  and set rev = 0;
#
# If any interfaces have been added increment age
#
# If any interfaces have been removed, set age = 0
#
LIBIPMIDETECT_CURRENT=0
LIBIPMIDETECT_REVISION=0
LIBIPMIDETECT_AGE=0
LIBIPMIDETECT_VERSION_INFO=$LIBIPMIDETECT_CURRENT:$LIBIPMIDETECT_REVISION:$LIBIPMIDETECT_AGE
AC_SUBST([LIBIPMIDETECT_VERSION_INFO])

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

dnl Option to disable installing init.d scripts
AC_ARG_ENABLE(init-scripts,
	AC_HELP_STRING([--disable-init-scripts],
		[don't install init.d startup scripts]))
INSTALL_INIT_SCRIPTS=install-init-scripts
if test x"$enable_init_scripts" = xno; then
	INSTALL_INIT_SCRIPTS=
fi
AC_SUBST(INSTALL_INIT_SCRIPTS)

dnl Option to disable installing logrotate config
AC_ARG_ENABLE(logrotate-config,
	AC_HELP_STRING([--disable-logrotate-config],
		[don't install logrotate configuration files]))
INSTALL_LOGROTATE_CONFIG=install-logrotate-config
if test x"$enable_logrotate_config" = xno; then
	INSTALL_LOGROTATE_CONFIG=
fi
AC_SUBST(INSTALL_LOGROTATE_CONFIG)

dnl Option to disable creating log directory
AC_ARG_ENABLE([logdir],
	AC_HELP_STRING([--disable-logdir], [don't create log directory]))
INSTALL_LOGDIR=install-logdir
if test x"$enable_logdir" = xno; then
	INSTALL_LOGDIR=
fi
AC_SUBST(INSTALL_LOGDIR)

dnl Option to disable creating ipmi sdr cache directory
AC_ARG_ENABLE([ipmisdrcachedir],
	AC_HELP_STRING([--disable-ipmisdrcachedir], [don't create ipmi sdr cache directory]))
INSTALL_IPMISDRCACHEDIR=install-ipmisdrcachedir
if test x"$enable_ipmisdrcachedir" = xno; then
	INSTALL_IPMISDRCACHEDIR=
fi
AC_SUBST(INSTALL_IPMISDRCACHEDIR)

dnl Checks for libraries.
AC_CHECK_LIB([gcrypt], [gcry_md_open], [have_gcrypt=yes], [have_gcrypt=no])
if test "x${have_gcrypt}" = "xno"; then
   AC_MSG_ERROR([libgcrypt required to build libfreeipmi])
   AC_MSG_NOTICE([Note: libgpg-error required for libgcrypt])
fi

dnl FreeBSD < 5 has getopt_long in a separate gnugetopt library
AC_CHECK_FUNC([getopt_long], [have_getopt_long=yes],
	AC_CHECK_LIB([gnugetopt], [getopt_long], [have_getopt_long=yes],
		AC_MSG_ERROR([gnugetopt library not found. libgnugetopt required.])))

if test "x${have_getopt_long}" = "xyes"; then
   AC_DEFINE(HAVE_GETOPT_LONG, 1, [define if you found getopt_long])
fi

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_BIGENDIAN
AC_C_CONST
AC_C_INLINE

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MMAP
AC_CHECK_FUNCS([strchr memcpy memset mlock])
AC_caolan_FUNC_WHICH_GETHOSTBYNAME_R
dnl There is no error(), strndup() and getline() on FreeBSD
AC_CHECK_FUNCS([error strndup getline strerror_r getprogname getpwuid_r])
dnl FreeBSD has no exp10() nor log2(), FreeBSD < 5 has no exp2()
AC_CHECK_LIB([m], [exp10])
AC_CHECK_LIB([m], [exp2])
AC_CHECK_LIB([m], [log2])

dnl Check for thread-safe gethostbyname()
AC_MSG_CHECKING([for thread-safe gethostbyname()])
have_thread_safe_gethostbyname=no
case "$host_os" in
*freebsd*)
	AC_TRY_CPP([
			#include <sys/param.h>
			#if __FreeBSD_version < 505000
			#error "gethostbyname() is not thread-safe"
			#endif
		], [have_thread_safe_gethostbyname=yes])
	;;
esac
if test "x$have_thread_safe_gethostbyname" = "xyes"; then
	AC_DEFINE([HAVE_THREAD_SAFE_GETHOSTBYNAME], [1],
		[Define if you have thread-safe gethostbyname() implementation])
	AC_MSG_RESULT([yes])
else
	AC_MSG_RESULT([no])
fi

dnl FreeBSD has argp as a separate library
dnl CAVEAT: argp-standalone library exports it's own strndup() function,
dnl not conforming to strndup(3). If we have no strndup() in libc (as
dnl in FreeBSD), and strndup() check is placed after argp check, argp's
dnl strndup() will be detected, which is wrong. So place argp check _after_
dnl strndup() check.
AC_CHECK_FUNC([argp_parse], ,
	AC_CHECK_LIB([argp], [argp_parse], ,
		AC_MSG_ERROR([argp library not found. argp-standalone required.])))

ACX_PTHREAD([], AC_MSG_ERROR([Posix threads required to build libipmiconsole]))

dnl Misc checks and build options
AC_IPMIPOWER_CONFIG_FILE
AC_IPMI_MONITORING_SDR_CACHE_DIR
AC_IPMI_MONITORING_SENSOR_CONFIG_FILE
AC_IPMIDETECT_CONFIG_FILE
AC_IPMIDETECTD_CONFIG_FILE

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([unistd.h fcntl.h getopt.h pthread.h])
AC_CHECK_HEADERS([linux/types.h error.h sys/io.h asm/io.h])
AC_CHECK_HEADERS([linux/ipmi_msgdefs.h])
AC_CHECK_HEADERS([linux/compiler.h])
AC_CHECK_HEADERS([linux/ipmi.h], [], [], 
[#ifdef HAVE_LINUX_IPMI_MSGDEFS_H
 #include <linux/ipmi_msgdefs.h>
 #endif
 #ifdef HAVE_LINUX_COMPILER_H
 #include <linux/compiler.h>
 #endif
])
AC_CHECK_HEADERS([sys/ipmi.h])

dnl Check for variables

dnl program_invocation_short_name is a GNU extension,
dnl we use getprogname() instead on FreeBSD
AC_MSG_CHECKING([for program_invocation_short_name])
AC_TRY_LINK([
		#define _GNU_SOURCE
		#include <errno.h>
	], [
		char *try = program_invocation_short_name;
	], [
		AC_DEFINE([HAVE_PROGRAM_SHORT_NAME], [1],
			[Define if you have program_invocation_shortname variable])
		AC_MSG_RESULT([yes])
	], [AC_MSG_RESULT([no])])

dnl Macro definitions

dnl Check for random number paths
AC_CHECK_FILE([/dev/urandom], AC_DEFINE([HAVE_DEVURANDOM], [1], [Define that you found /dev/urandom]))
AC_CHECK_FILE([/dev/random], AC_DEFINE([HAVE_DEVRANDOM], [1], [Define that you found /dev/urandom]))

AC_OUTPUT
