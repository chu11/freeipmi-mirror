dnl Process this file with autoconf to produce a configure script.
AC_INIT([freeipmi],[1.4.11],[freeipmi-devel@gnu.org])
AC_REVISION([$Id: configure.ac,v 1.208 2010-08-06 21:02:29 chu11 Exp $])
AC_CONFIG_MACRO_DIR([config])

FREEIPMI_PACKAGE_VERSION_MAJOR=1
FREEIPMI_PACKAGE_VERSION_MINOR=4
FREEIPMI_PACKAGE_VERSION_PATCH=11

AC_SUBST(FREEIPMI_PACKAGE_VERSION_MAJOR)
AC_SUBST(FREEIPMI_PACKAGE_VERSION_MINOR)
AC_SUBST(FREEIPMI_PACKAGE_VERSION_PATCH)

dnl We require a modern autoconf and automake
AC_PREREQ([2.57])

AC_CONFIG_AUX_DIR([config])

dnl Locate the sources.
AC_CONFIG_SRCDIR([ChangeLog.0])

AM_INIT_AUTOMAKE([tar-ustar subdir-objects 1.9])

dnl create a config.h file
AC_CONFIG_HEADERS([config/config.h])

dnl define output files
AC_CONFIG_FILES([
	freeipmi.spec
        Makefile 
        bmc-device/Makefile
        bmc-info/Makefile
        bmc-watchdog/Makefile 
        common/Makefile 
        common/debugutil/Makefile
        common/miscutil/Makefile
        common/parsecommon/Makefile
        common/pingtool/Makefile
        common/portability/Makefile
        common/toolcommon/Makefile
        contrib/Makefile
        doc/Makefile 
        etc/Makefile
        ipmi-chassis/Makefile
	ipmi-config/Makefile
        ipmi-dcmi/Makefile
        ipmi-fru/Makefile
        ipmi-locate/Makefile
        ipmi-oem/Makefile
        ipmi-pet/Makefile
        ipmi-raw/Makefile
        ipmi-sel/Makefile
        ipmi-sensors/Makefile
        ipmiconsole/Makefile
        ipmidetect/Makefile
        ipmidetectd/Makefile
        ipmiping/Makefile
        ipmipower/Makefile 
        ipmiseld/Makefile
        libfreeipmi/Makefile 
        libfreeipmi/libfreeipmi.pc
        libfreeipmi/include/Makefile 
        libfreeipmi/include/freeipmi/freeipmi.h
        libipmiconsole/Makefile
        libipmiconsole/ipmiconsole.h
        libipmiconsole/libipmiconsole.pc
        libipmidetect/Makefile
        libipmidetect/ipmidetect.h
        libipmidetect/libipmidetect.pc
        libipmimonitoring/Makefile
        libipmimonitoring/ipmi_monitoring.h
        libipmimonitoring/libipmimonitoring.pc
        man/Makefile
        man/bmc-device.8.pre
        man/bmc-info.8.pre
        man/bmc-watchdog.8.pre
        man/freeipmi.conf.5.pre
        man/freeipmi.7.pre
        man/ipmi-chassis.8.pre
        man/ipmi-config.8.pre
        man/ipmi-config.conf.5.pre
	man/ipmi-dcmi.8.pre
	man/ipmi-fru.8.pre
	man/ipmi-locate.8.pre
	man/ipmi-oem.8.pre
	man/ipmi-pet.8.pre
	man/ipmi-raw.8.pre
	man/ipmi-sel.8.pre
	man/ipmi-sensors.8.pre
	man/ipmiconsole.8.pre
	man/ipmidetect.8.pre
	man/ipmidetect.conf.5.pre
	man/ipmidetectd.8.pre
	man/ipmidetectd.conf.5.pre
	man/ipmiping.8.pre
	man/ipmipower.8.pre
	man/ipmiseld.8.pre
	man/ipmiseld.conf.5.pre
        man/libfreeipmi.3.pre
        man/freeipmi_interpret_sensor.conf.5.pre
        man/freeipmi_interpret_sel.conf.5.pre
        man/libipmiconsole.3.pre
        man/libipmiconsole.conf.5.pre
        man/libipmidetect.3.pre
        man/libipmimonitoring.3.pre
	man/rmcpping.8.pre
        rmcpping/Makefile])

ISODATE=`date +%Y-%m-%d`
AC_SUBST([ISODATE])

# libfreeipmi libtool versioning
#
# If the library source code has changed at all since the last update,
# then increment c:r: c:r+1:).
#
# If any interfaces have been added, removed, or changed since the
# last update, increment current, and set revision to 0.
#
# If any interfaces have been added since the last public release,
# then increment age.
#
# If any interfaces have been removed or changed since the last public
# release, then set age to 0.
#
LIBFREEIPMI_CURRENT=16
LIBFREEIPMI_REVISION=7
LIBFREEIPMI_AGE=0

LIBFREEIPMI_VERSION=$LIBFREEIPMI_CURRENT.$LIBFREEIPMI_REVISION.$LIBFREEIPMI_AGE
AC_SUBST([LIBFREEIPMI_VERSION])
LIBFREEIPMI_VERSION_INFO=$LIBFREEIPMI_CURRENT:$LIBFREEIPMI_REVISION:$LIBFREEIPMI_AGE
AC_SUBST([LIBFREEIPMI_VERSION_INFO])

# libfreeipmi versioning macros
#
# MAJOR - Incremented when interfaces are changed or removed.
#         Interfaces may be binary incompatible.  When incremented, MINOR and
#         PATCH are zeroed.
# MINOR - Incremented when interfaces are added.  Interfaces are
#         binary compatible with older minor versions.  When incremented,
#         PATCH is zeroed.
# PATCH - Incremented when interfaces are not changed.  Typically
#         incremented due to bug fixes or minor features.  Interfaces are
#         forward and backward compatible to other PATCH versions.

LIBFREEIPMI_VERSION_MAJOR=5
LIBFREEIPMI_VERSION_MINOR=0
LIBFREEIPMI_VERSION_PATCH=6

AC_SUBST(LIBFREEIPMI_VERSION_MAJOR)
AC_SUBST(LIBFREEIPMI_VERSION_MINOR)
AC_SUBST(LIBFREEIPMI_VERSION_PATCH)

# libipmiconsole libtool versioning
#
# If the library source code has changed at all since the last update,
# then increment c:r: c:r+1:).
#
# If any interfaces have been added, removed, or changed since the
# last update, increment current, and set revision to 0.
#
# If any interfaces have been added since the last public release,
# then increment age.
#
# If any interfaces have been removed or changed since the last public
# release, then set age to 0.
#
LIBIPMICONSOLE_CURRENT=5
LIBIPMICONSOLE_REVISION=3
LIBIPMICONSOLE_AGE=3
LIBIPMICONSOLE_VERSION_INFO=$LIBIPMICONSOLE_CURRENT:$LIBIPMICONSOLE_REVISION:$LIBIPMICONSOLE_AGE
AC_SUBST([LIBIPMICONSOLE_VERSION_INFO])

# libipmiconsole versioning macros
#
# MAJOR - Incremented when interfaces are changed or removed.
#         Interfaces may be binary incompatible.  When incremented, MINOR and
#         PATCH are zeroed.
# MINOR - Incremented when interfaces are added.  Interfaces are
#         binary compatible with older minor versions.  When incremented,
#         PATCH is zeroed.
# PATCH - Incremented when interfaces are not changed.  Typically
#         incremented due to bug fixes or minor features.  Interfaces are
#         forward and backward compatible to other PATCH versions.
#

LIBIPMICONSOLE_VERSION_MAJOR=1
LIBIPMICONSOLE_VERSION_MINOR=2
LIBIPMICONSOLE_VERSION_PATCH=3

AC_SUBST(LIBIPMICONSOLE_VERSION_MAJOR)
AC_SUBST(LIBIPMICONSOLE_VERSION_MINOR)
AC_SUBST(LIBIPMICONSOLE_VERSION_PATCH)

# libipmimonitoring libtool versioning
#
# If the library source code has changed at all since the last update,
# then increment c:r: c:r+1:).
#
# If any interfaces have been added, removed, or changed since the
# last update, increment current, and set revision to 0.
#
# If any interfaces have been added since the last public release,
# then increment age.
#
# If any interfaces have been removed or changed since the last public
# release, then set age to 0.
#
LIBIPMIMONITORING_CURRENT=5
LIBIPMIMONITORING_REVISION=6
LIBIPMIMONITORING_AGE=0
LIBIPMIMONITORING_VERSION_INFO=$LIBIPMIMONITORING_CURRENT:$LIBIPMIMONITORING_REVISION:$LIBIPMIMONITORING_AGE
AC_SUBST([LIBIPMIMONITORING_VERSION_INFO])

# libipmimonitoring versioning macros
#
# MAJOR - Incremented when interfaces are changed or removed.
#         Interfaces may be binary incompatible.  When incremented, MINOR and
#         PATCH are zeroed.
# MINOR - Incremented when interfaces are added.  Interfaces are
#         binary compatible with older minor versions.  When incremented,
#         PATCH is zeroed.
# PATCH - Incremented when interfaces are not changed.  Typically
#         incremented due to bug fixes or minor features.  Interfaces are
#         forward and backward compatible to other PATCH versions.
#

LIBIPMIMONITORING_VERSION_MAJOR=1
LIBIPMIMONITORING_VERSION_MINOR=0
LIBIPMIMONITORING_VERSION_PATCH=6

AC_SUBST(LIBIPMIMONITORING_VERSION_MAJOR)
AC_SUBST(LIBIPMIMONITORING_VERSION_MINOR)
AC_SUBST(LIBIPMIMONITORING_VERSION_PATCH)

# libipmidetect libtool versioning
#
# If the library source code has changed at all since the last update,
# then increment c:r: c:r+1:).
#
# If any interfaces have been added, removed, or changed since the
# last update, increment current, and set revision to 0.
#
# If any interfaces have been added since the last public release,
# then increment age.
#
# If any interfaces have been removed or changed since the last public
# release, then set age to 0.
#
LIBIPMIDETECT_CURRENT=0
LIBIPMIDETECT_REVISION=0
LIBIPMIDETECT_AGE=0
LIBIPMIDETECT_VERSION_INFO=$LIBIPMIDETECT_CURRENT:$LIBIPMIDETECT_REVISION:$LIBIPMIDETECT_AGE
AC_SUBST([LIBIPMIDETECT_VERSION_INFO])

# libipmidetect versioning macros
#
# MAJOR - Incremented when interfaces are changed or removed.
#         Interfaces may be binary incompatible.  When incremented, MINOR and
#         PATCH are zeroed.
# MINOR - Incremented when interfaces are added.  Interfaces are
#         binary compatible with older minor versions.  When incremented,
#         PATCH is zeroed.
# PATCH - Incremented when interfaces are not changed.  Typically
#         incremented due to bug fixes or minor features.  Interfaces are
#         forward and backward compatible to other PATCH versions.
#

LIBIPMIDETECT_VERSION_MAJOR=1
LIBIPMIDETECT_VERSION_MINOR=0
LIBIPMIDETECT_VERSION_PATCH=0

AC_SUBST(LIBIPMIDETECT_VERSION_MAJOR)
AC_SUBST(LIBIPMIDETECT_VERSION_MINOR)
AC_SUBST(LIBIPMIDETECT_VERSION_PATCH)

AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_CC
dnl Needed for per-target flags
AM_PROG_CC_C_O
AC_PROG_LIBTOOL
AC_PATH_PROG([CPP], [cpp])
if test -z "${CPP}"; then
  AC_MSG_ERROR([cannot find cpp])
fi
AM_CONDITIONAL(WITH_GNU_LD, test "$with_gnu_ld" = "yes")
AC_PROG_MAKE_SET
AC_PROG_LN_S

dnl Enable error tracing. Lower level calls will spit error messages
dnl on to the console
AC_ARG_ENABLE([trace],
              AC_HELP_STRING([--enable-trace], [turn on error tracing]))
if test "x${enable_trace}" = "xyes" ; then
     AC_DEFINE([IPMI_TRACE],[1],[Define to turn on error tracing])
elif test "x${enable_trace}" = "x" ; then
     enable_trace=no
fi

AC_ARG_ENABLE([profiling],
              AC_HELP_STRING([--enable-profiling], [turn on profiling]))
if test "x${enable_profiling}" = "x" ; then
     enable_profiling=no
fi

dnl Turn off all optimizations when debugging
AC_ARG_ENABLE(debug,
              AC_HELP_STRING([--enable-debug], [turn on debugging]))
if test "x${enable_debug}" = "xyes"; then
   if test "$GCC" = "yes" && test "x${enable_profiling}" = "xyes"; then 
      CFLAGS="$CFLAGS -ggdb -Wall -pg"
   elif test "$GCC" = "yes" && test "x${enable_profiling}" = "xno"; then 
      CFLAGS="$CFLAGS -ggdb -Wall"
   elif test "$ac_cv_prog_cc_g" = "yes"; then
      CFLAGS="$CFLAGS -g"
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

dnl Allow advanced developers to compile with raw dumping
AC_ARG_ENABLE(rawdumps,
   AC_HELP_STRING([--enable-rawdumps], [output raw packet dumps when debugging]))
if test "x${enable_rawdumps}" = "xyes"; then
   AC_DEFINE([WITH_RAWDUMPS], [1], [Define if you want raw dumps])
fi

dnl Option to disable installing init.d scripts
AC_ARG_ENABLE(init-scripts,
   AC_HELP_STRING([--disable-init-scripts],
      [don\'t install init.d startup scripts]))
INSTALL_INIT_SCRIPTS=install-init-scripts
if test x"$enable_init_scripts" = xno; then
   INSTALL_INIT_SCRIPTS=
fi
AC_SUBST(INSTALL_INIT_SCRIPTS)

dnl Option to disable creating ipmi sdr cache directory
AC_ARG_ENABLE([ipmisdrcachedir],
   AC_HELP_STRING([--disable-ipmisdrcachedir], [don\'t create ipmi sdr cache directory]))
INSTALL_IPMISDRCACHEDIR=install-ipmisdrcachedir
if test x"$enable_ipmisdrcachedir" = xno; then
   INSTALL_IPMISDRCACHEDIR=
fi
AC_SUBST(INSTALL_IPMISDRCACHEDIR)

dnl Option to disable encryption in FreeIPMI
dnl
dnl May be useful/needed for embedded systems w/ IPMI in band only and
dnl don\'t want to link to additional encryption libraries not on the
dnl system.  If turned off and users use encryption features (i.e. in IPMI 2.0),
dnl errors will be returned from the underlying subsystems.
dnl
dnl Default to it being on
AC_MSG_CHECKING([for encryption support])
AC_ARG_WITH([encryption],
           AC_HELP_STRING([--without-encryption], [don\'t support encryption]),
           [ case "$withval" in
             no)  ac_with_encryption=no ;;
             yes) ac_with_encryption=yes ;;
             *)   ac_with_encryption=yes ;;
           esac
           ])
AC_MSG_RESULT([${ac_with_encryption=yes}])

if test x"${ac_with_encryption}" = xyes; then
   AC_DEFINE([WITH_ENCRYPTION], [1], [Define if you want encryption])
fi

dnl Checks for libraries.
dnl don\'t include lib if LAN not desired
if test x"${ac_with_encryption}" = xyes; then
  AC_CHECK_LIB([gcrypt], [gcry_md_open], [have_gcrypt=yes], [have_gcrypt=no])
  if test "x${have_gcrypt}" = "xno"; then
     AC_MSG_ERROR([libgcrypt required to build libfreeipmi])
     AC_MSG_NOTICE([Note: libgpg-error required for libgcrypt])
  fi
  AC_CHECK_HEADERS([gcrypt.h], [have_gcrypt_header=yes], [have_gcrypt_header=no])
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
  GCRYPT_LIBS=-lgcrypt
fi
AC_SUBST(GCRYPT_LIBS)

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
AC_CHECK_HEADERS([linux/types.h error.h netdb.h sys/io.h sys/sockio.h asm/io.h sys/ioccom.h])
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
AC_CHECK_HEADERS([bmc_intf.h])
AC_CHECK_HEADERS([signal.h])

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
AC_CHECK_FUNCS([cbrt])

dnl sighandler_t apparently not defined in Apple/OS X
AC_CHECK_TYPES([sighandler_t], [], [], [[#include <signal.h>]])

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
dnl FreeBSD and Apple has the argp-standalone package.  If we're on
dnl FreeBSD or Apple make them install the argp-standalone.  Otherwise
dnl the code will fall through and compile the internal FreeIPMI-argp
   AC_TRY_COMPILE([#include <sys/param.h>],
                  [
                   #if defined(__FreeBSD__) || defined(__APPLE__)
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

FREEIPMI_SYSCONFDIR=${sysconfdir}/freeipmi/
AC_SUBST(FREEIPMI_SYSCONFDIR)

AC_FREEIPMI_CONFIG_FILE
AC_IPMI_MONITORING_SDR_CACHE_DIR
dnl legacy
AC_IPMI_MONITORING_SENSOR_CONFIG_FILE
dnl legacy
AC_IPMIPOWER_CONFIG_FILE
dnl legacy
AC_IPMICONSOLE_CONFIG_FILE
AC_INTERPRET_SENSOR_CONFIG_FILE
AC_INTERPRET_SEL_CONFIG_FILE
AC_IPMIDETECT_CONFIG_FILE
AC_IPMIDETECTD_CONFIG_FILE
AC_IPMISELD_CONFIG_FILE
AC_IPMISELD_CACHE_DIRECTORY
AC_LIBIPMICONSOLE_CONFIG_FILE
AC_DONT_CHECK_FOR_ROOT

dnl Checks for things needed in lib argp
AC_CHECK_DECLS([program_invocation_name, program_invocation_short_name],
               [], [], [[#include <errno.h>]])

dnl Check for variables

dnl Macro definitions

dnl Check for random number paths

dnl Option to disable checking for /dev/urandom and /dev/random
dnl
dnl May be useful for cross compile distros where checking for
dnl specific files is illegal.
dnl
AC_MSG_CHECKING([for random device support])
AC_ARG_WITH([random-device],
           AC_HELP_STRING([--without-random-device], [don't support random devices like /dev/urandom]),
           [ case "$withval" in
             no)  ac_with_random_device=no ;;
             yes) ac_with_random_device=yes ;;
             *)   ac_with_random_device=yes ;;
           esac
           ])
AC_MSG_RESULT([${ac_with_random_device=yes}])

if test x"${ac_with_random_device}" = xyes; then
   AC_CHECK_FILE([/dev/urandom], AC_DEFINE([HAVE_DEVURANDOM], [1], [Define that you found /dev/urandom]))
   AC_CHECK_FILE([/dev/random], AC_DEFINE([HAVE_DEVRANDOM], [1], [Define that you found /dev/urandom]))
fi


dnl Option to install pkg-config support files

pkgconfigdir='${libdir}/pkgconfig'
AC_MSG_CHECKING(whether to install pkg-config *.pc files)
AC_ARG_WITH(pkgconfig-dir,
	AS_HELP_STRING([--with-pkgconfig-dir=PATH], [where to install pkg-config *.pc files (EPREFIX/lib/pkgconfig)]),
[
	case "${withval}" in
	yes|auto)
		;;
	no)
		pkgconfigdir=""
		;;
	*)
		pkgconfigdir="${withval}"
		;;
	esac
], [])
if test -n "${pkgconfigdir}"; then
	AC_MSG_RESULT(using ${pkgconfigdir})
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(WITH_PKG_CONFIG, test -n "${pkgconfigdir}")
AC_SUBST(pkgconfigdir)

AC_OUTPUT
