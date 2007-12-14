##*****************************************************************************
## $Id: ac_ipmi_monitoring_sdr_cache_dir.m4,v 1.2 2007-12-14 19:16:19 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMI_MONITORING_SDR_CACHE_DIR],
[
# Must expand nested unquoting
  IPMI_MONITORING_SDR_CACHE_DIR_TMP1="`eval echo ${localstatedir}/cache/ipmimonitoringsdrcache`"
  IPMI_MONITORING_SDR_CACHE_DIR_TMP2="`echo $IPMI_MONITORING_SDR_CACHE_DIR_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMI_MONITORING_SDR_CACHE_DIR="`eval echo $IPMI_MONITORING_SDR_CACHE_DIR_TMP2`"

  AC_MSG_CHECKING([for ipmi_monitoring sdr cache dir default path])
  AC_ARG_WITH([ipmi-monitoring-sdr-cache-dir],
    AC_HELP_STRING([--with-ipmi-monitoring-sdr-cache-dir=PATH], 
                   [Specify default ipmi_monitoring sdr cache dir path]),
    [ case "$withval" in
        no)  ;;
        yes) ;;
        *)   IPMI_MONITORING_SDR_CACHE_DIR=$withval 
      esac
    ]
  )
  AC_MSG_RESULT($IPMI_MONITORING_SDR_CACHE_DIR)

  AC_DEFINE_UNQUOTED([IPMI_MONITORING_SDR_CACHE_DIR], 
                     ["$IPMI_MONITORING_SDR_CACHE_DIR"], 
                     [Define default ipmi_monitoring sdr_cache_dir.])
  AC_SUBST(IPMI_MONITORING_SDR_CACHE_DIR)
])
