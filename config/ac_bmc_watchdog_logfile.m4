##*****************************************************************************
## $Id: ac_bmc_watchdog_logfile.m4,v 1.1 2008-05-29 05:50:19 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_BMC_WATCHDOG_LOGFILE_DEFAULT],
[
# Must expand nested unquoting
  BMC_WATCHDOG_LOGFILE_DEFAULT_TMP1="`eval echo ${localstatedir}/log/freeipmi/bmc-watchdog.log`"
  BMC_WATCHDOG_LOGFILE_DEFAULT_TMP2="`echo $BMC_WATCHDOG_LOGFILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  BMC_WATCHDOG_LOGFILE_DEFAULT="`eval echo $BMC_WATCHDOG_LOGFILE_DEFAULT_TMP2`"

  AC_MSG_CHECKING([for bmc-watchdog logfile default path])
  AC_ARG_WITH([bmc-watchdog-logfile],
    AC_HELP_STRING([--with-bmc-watchdog-logfile=PATH], 
                   [Specify default bmc-watchdog logfile path]),
    [ case "$withval" in
        no)  ;;
        yes) ;;
        *)   BMC_WATCHDOG_LOGFILE_DEFAULT=$withval 
      esac
    ]
  )
  AC_MSG_RESULT($BMC_WATCHDOG_LOGFILE_DEFAULT)

  AC_DEFINE_UNQUOTED([BMC_WATCHDOG_LOGFILE_DEFAULT], 
                     ["$BMC_WATCHDOG_LOGFILE_DEFAULT"], 
                     [Define default bmc-watchdog logfile.])
  AC_SUBST(BMC_WATCHDOG_LOGFILE_DEFAULT)
])
