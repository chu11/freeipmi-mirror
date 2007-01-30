##*****************************************************************************
## $Id: ac_ipmi_monitoring_sdr_cache_dir.m4,v 1.1 2007-01-30 21:52:57 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMI_MONITORING_SDR_CACHE_DIR],
[
  IPMI_MONITORING_SDR_CACHE_DIR=/var/cache/ipmimonitoringsdrcache

  AC_MSG_CHECKING([for ipmi_monitoring sdr cache dir default path])
  AC_ARG_WITH([ipmi-monitoring-sensor-sdr-cache-dir],
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
