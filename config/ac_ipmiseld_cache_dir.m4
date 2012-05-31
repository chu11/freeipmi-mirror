##*****************************************************************************
## $Id: ac_ipmi_monitoring_cache_directory.m4,v 1.2 2007-12-14 19:16:19 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMISELD_CACHE_DIRECTORY],
[
# Must expand nested unquoting
  IPMISELD_CACHE_DIRECTORY_TMP1="`eval echo ${localstatedir}/cache/ipmiseld/`"
  IPMISELD_CACHE_DIRECTORY_TMP2="`echo $IPMISELD_CACHE_DIRECTORY_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMISELD_CACHE_DIRECTORY="`eval echo $IPMISELD_CACHE_DIRECTORY_TMP2`"

  AC_MSG_CHECKING([for ipmiseld cache dir default path])
  AC_ARG_WITH([ipmiseld-cache-dir],
    AC_HELP_STRING([--with-ipmiseld-cache-dir=PATH], 
                   [Specify default ipmiseld cache dir path]),
    [ case "$withval" in
        no)  ;;
        yes) ;;
        *)   IPMISELD_CACHE_DIRECTORY=$withval 
      esac
    ]
  )
  AC_MSG_RESULT($IPMISELD_CACHE_DIRECTORY)

  AC_DEFINE_UNQUOTED([IPMISELD_CACHE_DIRECTORY], 
                     ["$IPMISELD_CACHE_DIRECTORY"], 
                     [Define default ipmiseld cache dir.])
  AC_SUBST(IPMISELD_CACHE_DIRECTORY)
])
