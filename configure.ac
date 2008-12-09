dnl Process this file with autoconf to produce a configure script.
AC_INIT([freeipmi],[0.7.4.beta1],[freeipmi-devel@gnu.org])
AC_REVISION([$Id: configure.ac,v 1.154.2.13 2008-12-09 21:37:28 chu11 Exp $])

dnl We require a modern autoconf and automake
AC_PREREQ([2.57])

AC_CONFIG_AUX_DIR([config])

dnl Locate the sources.
AC_CONFIG_SRCDIR([ChangeLog.0])

AM_INIT_AUTOMAKE([tar-ustar 1.9])

dnl create a config.h file
AM_CONFIG_HEADER([config/config.h])

dnl define output files
AC_CONFIG_FILES([
        Makefile 
        doc/Makefile 
        etc/Makefile
        man/Makefile
        man/freeipmi.conf.5.pre
        man/freeipmi.7.pre
	common/Makefile 
        common/man/Makefile 
        common/src/Makefile
        common/src/configtool/Makefile
        common/src/debugutil/Makefile
        common/src/miscutil/Makefile
        common/src/pingtool/Makefile
        common/src/portability/Makefile
        common/src/toolcommon/Makefile
        common/src/toolhostrange/Makefile
        contrib/Makefile
        contrib/ganglia/Makefile
        contrib/nagios/Makefile
        libfreeipmi/Makefile 
        libfreeipmi/libfreeipmi.3.pre
        libfreeipmi/include/Makefile 
	libfreeipmi/src/Makefile 
        libfreeipmi/src/api/Makefile
        libfreeipmi/src/cmds/Makefile
        libfreeipmi/src/debug/Makefile
        libfreeipmi/src/driver/Makefile
        libfreeipmi/src/fiid/Makefile
        libfreeipmi/src/interface/Makefile
        libfreeipmi/src/libcommon/Makefile
        libfreeipmi/src/locate/Makefile
        libfreeipmi/src/record-format/Makefile
        libfreeipmi/src/sdr-cache/Makefile
        libfreeipmi/src/spec/Makefile
        libfreeipmi/src/util/Makefile
	bmc-config/Makefile 
 	bmc-config/bmc-config.8.pre 
        bmc-config/bmc-config.conf.5.pre
        bmc-config/src/Makefile
        bmc-device/Makefile 
	bmc-device/bmc-device.8.pre
        bmc-device/src/Makefile
        bmc-info/Makefile 
	bmc-info/bmc-info.8.pre
        bmc-info/src/Makefile
        bmc-watchdog/Makefile 
        bmc-watchdog/bmc-watchdog.8.pre
        bmc-watchdog/src/Makefile
	ipmi-chassis/Makefile
	ipmi-chassis/ipmi-chassis.8.pre
	ipmi-chassis/src/Makefile
        ipmi-chassis-config/Makefile 
	ipmi-chassis-config/ipmi-chassis-config.8.pre
        ipmi-chassis-config/src/Makefile 
        ipmi-fru/Makefile 
	ipmi-fru/ipmi-fru.8.pre
        ipmi-fru/src/Makefile
        ipmi-locate/Makefile 
        ipmi-locate/ipmi-locate.8.pre
        ipmi-locate/src/Makefile
        ipmi-oem/Makefile 
	ipmi-oem/ipmi-oem.8.pre
        ipmi-oem/src/Makefile 
        ipmi-raw/Makefile 
	ipmi-raw/ipmi-raw.8.pre
        ipmi-raw/src/Makefile 
        ipmi-sel/Makefile 
	ipmi-sel/ipmi-sel.8.pre
        ipmi-sel/src/Makefile 
        ipmi-sensors/Makefile 
	ipmi-sensors/ipmi-sensors.8.pre
        ipmi-sensors/src/Makefile 
        ipmi-sensors-config/Makefile 
	ipmi-sensors-config/ipmi-sensors-config.8.pre
        ipmi-sensors-config/src/Makefile 
	ipmiconsole/Makefile 
	ipmiconsole/ipmiconsole.8.pre
        ipmiconsole/ipmiconsole.conf.5.pre
        ipmiconsole/libipmiconsole.3.pre
        ipmiconsole/src/Makefile
	ipmiconsole/src/libipmiconsole/Makefile
        ipmiconsole/src/ipmiconsole/Makefile
        ipmidetect/Makefile
        ipmidetect/ipmidetect.8.pre
        ipmidetect/ipmidetect.conf.5.pre
        ipmidetect/ipmidetectd.8.pre
        ipmidetect/ipmidetectd.conf.5.pre
        ipmidetect/libipmidetect.3.pre
        ipmidetect/src/Makefile
        ipmidetect/src/ipmidetect/Makefile
        ipmidetect/src/ipmidetectd/Makefile
        ipmidetect/src/libipmidetect/Makefile
	ipmimonitoring/Makefile
        ipmimonitoring/src/Makefile
	ipmimonitoring/ipmimonitoring.8.pre
        ipmimonitoring/ipmi_monitoring_sensors.conf.5.pre
        ipmimonitoring/ipmimonitoring_sensors.conf.5.pre
        ipmimonitoring/ipmimonitoring.conf.5.pre
        ipmimonitoring/libipmimonitoring.conf.5.pre
        ipmimonitoring/libipmimonitoring.3.pre
        ipmimonitoring/src/libipmimonitoring/Makefile 
        ipmimonitoring/src/ipmimonitoring/Makefile
        ipmiping/Makefile 
        ipmiping/ipmiping.8.pre
        ipmiping/src/Makefile
        ipmipower/Makefile 
        ipmipower/ipmipower.8.pre
        ipmipower/ipmipower.conf.5.pre
        ipmipower/src/Makefile
        pef-config/Makefile
	pef-config/pef-config.8.pre
        pef-config/src/Makefile 
        rmcpping/Makefile 
        rmcpping/rmcpping.8.pre
        rmcpping/src/Makefile
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
LIBFREEIPMI_CURRENT=7
LIBFREEIPMI_REVISION=1
LIBFREEIPMI_AGE=1

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
LIBIPMICONSOLE_CURRENT=2
LIBIPMICONSOLE_REVISION=2
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
LIBIPMIMONITORING_CURRENT=2
LIBIPMIMONITORING_REVISION=2
LIBIPMIMONITORING_AGE=1
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
AC_PATH_PROG([CPP], [cpp])
if test -z "${CPP}"; then
  AC_MSG_ERROR([cannot find cpp])
fi
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
AC_CHECK_HEADER([gcrypt.h], [have_gcrypte_header=yes], [have_gcrypt_header=no])
if test "x${have_gcrypt_header}" = "xno"; then
   AC_MSG_ERROR([gcrypt.h not found])
fi
AC_CHECK_DECL([GCRYCTL_SET_THREAD_CBS], 
              [have_gcrypt_threads=yes],
              [have_gcrypt_threads=no],
              [#include <gcrypt.h>])
if test "x${have_gcrypt_threads}" = "xno"; then
   AC_MSG_ERROR([libgcrypt with threads support required for libfreeipmi])
fi

dnl FreeBSD < 5 has getopt_long in a separate gnugetopt library
AC_CHECK_FUNC([getopt_long], [have_getopt_long=yes],
	AC_CHECK_LIB([gnugetopt], [getopt_long], [have_getopt_long=yes],
		AC_MSG_ERROR([getopt_long not found.])))

if test "x${have_getopt_long}" = "xyes"; then
   AC_DEFINE(HAVE_GETOPT_LONG, 1, [define if you found getopt_long])
fi

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_BIGENDIAN
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T
LSH_GCC_ATTRIBUTES

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_SYS_WAIT
dnl achu: note "strings.h" plural
AC_CHECK_HEADERS([unistd.h fcntl.h getopt.h pthread.h strings.h])
AC_CHECK_HEADERS([linux/types.h error.h sys/io.h sys/sockio.h asm/io.h sys/ioccom.h])
AC_CHECK_HEADERS([linux/ipmi_msgdefs.h])
AC_CHECK_HEADERS([linux/compiler.h])
AC_CHECK_HEADERS([stropts.h sys/stropts.h])
AC_CHECK_HEADERS([linux/ipmi.h], [], [], 
[#ifdef HAVE_LINUX_IPMI_MSGDEFS_H
 #include <linux/ipmi_msgdefs.h>
 #endif
 #ifdef HAVE_LINUX_COMPILER_H
 #include <linux/compiler.h>
 #endif
])
AC_CHECK_HEADERS([sys/ipmi.h])
AC_CHECK_HEADERS([sys/int_types.h])
AC_CHECK_HEADERS([bmc_intf.h],, AC_MSG_WARN([cannot build sunbmc driver]))

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MMAP
AC_FUNC_VPRINTF

dnl There is no strndup() and getline() on FreeBSD
dnl stristr may not exist at all on *nix libc.  Maybe only in script-lands??
AC_CHECK_FUNCS([strchr strndup strchrnul strsep stristr])
AC_CHECK_FUNCS([memcpy mempcpy memset mlock])
AC_CHECK_FUNCS([getline getprogname])
AC_CHECK_FUNCS([strerror strerror_r])
AC_CHECK_FUNCS([flockfile fputs_unlocked fwrite_unlocked])
AC_CHECK_FUNCS([iopl])
AC_CHECK_FUNCS([asprintf])

# At least on freebsd, putc_unlocked is a macro, so the standard
# AC_CHECK_FUNCS doesn't work well.
AC_TRY_LINK([#include <stdio.h>], 
            [putc_unlocked('x', stdout)],
            [AC_DEFINE_UNQUOTED(HAVE_PUTC_UNLOCKED, [1], [Define to 1 if you have putc_unlocked])])

dnl FreeBSD has no exp10() nor log2(), FreeBSD < 5 has no exp2()
AC_CHECK_LIB([m], [exp10])
AC_CHECK_LIB([m], [exp2])
AC_CHECK_LIB([m], [log2])

dnl Solaris needs -lsocket
AC_CHECK_FUNC([socket], 
              , 
              AC_CHECK_LIB([socket],
                           [socket]))

dnl
dnl achu: Most parts of this gethostbyname_r testing is from the
dnl original done by Caolan McNamara <caolan@skynet.ie>.  That one
dnl had trouble dealing with libnsl and Solaris.  So I had to re-write
dnl it a bit.
dnl
dnl See original: http://www.csn.ul.ie/~caolan/publink/gethostbyname_r
dnl

AC_CHECK_FUNC([gethostbyname_r], 
              [have_gethostbyname_r=yes], 
              AC_CHECK_LIB([nsl],
                           [gethostbyname_r],
                           [LIBS="$LIBS -lnsl" have_gethostbyname_r=yes],
                           [have_gethostbyname_r=no]))             

if test "x${have_gethostbyname_r}" = "xyes"; then
   AC_TRY_LINK(
   [
   #include <netdb.h>
   ],
   [
   char *name;
   struct hostent *he;
   struct hostent_data data;
   (void) gethostbyname_r(name, he, &data);
   ],
   [ac_cv_func_which_gethostbyname_r=three], [
	AC_TRY_LINK(
	[
	#include <netdb.h>
	],
	[
	char *name;
	struct hostent *he, *res;
	char buffer[2048];
	int buflen = 2048;
	int h_errnop;
	(void) gethostbyname_r(name, he, buffer, buflen, &res, &h_errnop);
	],
	[ac_cv_func_which_gethostbyname_r=six], [
	    AC_TRY_LINK(
	    [
	    #include <netdb.h>
	    ],
	    [
	    char *name;
	    struct hostent *he;
	    char buffer[2048];
	    int buflen = 2048;
	    int h_errnop;
	    (void) gethostbyname_r(name, he, buffer, buflen, &h_errnop);
	    ],
	    [ac_cv_func_which_gethostbyname_r=five],
	    [ac_cv_func_which_gethostbyname_r=no])
	])
   ])

dnl  define HAVE_FUNC_GETHOSTBYNAME_R_6 if gethostbyname_r needs 6 arguments (e.g linux)
dnl  define HAVE_FUNC_GETHOSTBYNAME_R_5 if gethostbyname_r needs 5 arguments (e.g. solaris)
dnl  define HAVE_FUNC_GETHOSTBYNAME_R_3 if gethostbyname_r needs 3 arguments (e.g. osf/1)

   if test "x${ac_cv_func_which_gethostbyname_r}" = "xsix"; then
      AC_DEFINE(HAVE_FUNC_GETHOSTBYNAME_R_6, [], [Define gethostbyname_r with 6 args])
   elif test "x${ac_cv_func_which_gethostbyname_r}" = "xfive"; then
      AC_DEFINE(HAVE_FUNC_GETHOSTBYNAME_R_5, [], [Define gethostbyname_r with 5 args])
   elif test "x${ac_cv_func_which_gethostbyname_r}" = "xthree"; then
      AC_DEFINE(HAVE_FUNC_GETHOSTBYNAME_R_3, [], [Define gethostbyname_r with 3 args])
   fi
fi

dnl Check for thread-safe gethostbyname() for internal portable freeipmi_gethostbyname_r
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

AC_CHECK_FUNC([hstrerror], 
              [have_hstrerror=yes], 
              AC_CHECK_LIB([nsl],
                           [hstrerror],
                           [have_hstrerror=yes],
                           [have_hstrerror=no]))

if test "x$have_hstrerror" = "xyes"; then
    AC_DEFINE([HAVE_HSTRERROR], [1], [Define if you have hstrerror])
fi

AC_CHECK_FUNC([getpwuid_r], [have_getpwuid_r=yes], AC_MSG_ERROR([getpwuid_r cannot be found]))

if test "x${have_getpwuid_r}" = "xyes"; then
   AC_TRY_LINK(
   [
   #include <pwd.h>
   #include <sys/types.h>
   ],
   [
   uid_t uid;
   struct passwd pwd;
   struct passwd *result;
   char buffer[1];
   int bufsize;
   (void) getpwuid_r (uid, &pwd, buffer, bufsize, &result);
   ],
   [ac_cv_func_which_getpwuid_r=five], [
	AC_TRY_LINK(
	[
	#include <pwd.h>
        #include <sys/types.h>
	],
	[
        uid_t uid;
        struct passwd pwd;
        char buffer[1];
        int bufsize;
        (void) getpwuid_r (uid, &pwd, buffer, bufsize);
	],
	[ac_cv_func_which_getpwuid_r=four], 
        [ac_cv_func_which_getpwuid_r=no])
   ])

dnl  define HAVE_FUNC_GETPWUID_R_5 if getpwuid_r needs 5 arguments (e.g. linux)
dnl  define HAVE_FUNC_GETPWUID_R_4 if getpwuid_r needs 4 arguments (e.g. solaris)

   if test "x${ac_cv_func_which_getpwuid_r}" = "xfive"; then
      AC_DEFINE(HAVE_FUNC_GETPWUID_R_5, [], [Define getpwuid_r with 5 args])
   elif test "x${ac_cv_func_which_getpwuid_r}" = "xfour"; then
      AC_DEFINE(HAVE_FUNC_GETPWUID_R_4, [], [Define getpwuid_r with 4 args])
   else
      AC_MSG_ERROR([getpwuid_r cannot be compiled])
   fi
fi

dnl FreeBSD has argp as a separate library
dnl CAVEAT: argp-standalone library exports it's own strndup() function,
dnl not conforming to strndup(3). If we have no strndup() in libc (as
dnl in FreeBSD), and strndup() check is placed after argp check, argp's
dnl strndup() will be detected, which is wrong. So place argp check _after_
dnl strndup() check.
dnl
dnl Solaris has no argp library
AC_CHECK_FUNC([argp_parse], 
              [have_argp=yes],
	      AC_CHECK_LIB([argp], 
                           [argp_parse], 
                           [have_argp=yes; LIBS="-largp $LIBS"],
                           [have_argp=no]))

if test "x$have_argp" = "xyes"; then
   AC_CHECK_HEADER([argp.h], 
		   AC_DEFINE([HAVE_ARGP_H], [1], [Define if you have argp.h]),
	           AC_MSG_ERROR([argp header cannot be found]))
else
dnl FreeBSD has the argp-standalone package.  If we're on FreeBSD
dnl make them install the argp-standalone.  On non-FreeBSD, the code
dnl will fall through and compile the internal FreeIPMI-argp
   AC_TRY_COMPILE([#include <sys/param.h>],
                  [
                   #ifdef __FreeBSD_version
                   uncompilable junk syntax %$ # @
                   #endif
                  ],
                  ,
                  AC_MSG_ERROR([argp library not found. argp-standalone required.]))
fi

AM_CONDITIONAL(WITH_FREEIPMI_ARGP, test "$have_argp" = "no")

AC_TRY_COMPILE([#include <stdlib.h>],
               [printf("%s\n", __FUNCTION__);],
               [AC_DEFINE([HAVE_FUNCTION_MACRO], [1], [Define is you have __FUNCTION__])])

ACX_PTHREAD([], AC_MSG_ERROR([Posix threads required to build libipmiconsole]))

dnl Misc checks and build options
AC_FREEIPMI_CONFIG_FILE
AC_BMC_WATCHDOG_LOGFILE_DEFAULT
AC_IPMI_MONITORING_SDR_CACHE_DIR
AC_IPMI_MONITORING_SENSOR_CONFIG_FILE
AC_IPMIDETECT_CONFIG_FILE
AC_IPMIDETECTD_CONFIG_FILE

dnl Checks for things needed in lib argp
AC_CHECK_DECLS([program_invocation_name, program_invocation_short_name],
               [], [], [[#include <errno.h>]])

dnl Check for variables

dnl Macro definitions

dnl Check for random number paths
AC_CHECK_FILE([/dev/urandom], AC_DEFINE([HAVE_DEVURANDOM], [1], [Define that you found /dev/urandom]))
AC_CHECK_FILE([/dev/random], AC_DEFINE([HAVE_DEVRANDOM], [1], [Define that you found /dev/urandom]))

AC_OUTPUT
