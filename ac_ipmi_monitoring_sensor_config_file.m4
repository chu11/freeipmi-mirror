##*****************************************************************************
## $Id: ac_ipmi_monitoring_sensor_config_file.m4,v 1.1 2007-01-30 21:52:57 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMI_MONITORING_SENSOR_CONFIG_FILE],
[
  IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT=/etc/ipmi_monitoring_sensor.conf

  AC_MSG_CHECKING([for ipmi_monitoring_sensor config file default path])
  AC_ARG_WITH([ipmi-monitoring-sensor-config-file],
    AC_HELP_STRING([--with-ipmi-monitoring-sensor-config-file=PATH], 
                   [Specify default ipmi_monitoring_sensor config file path]),
    [ case "$withval" in
        no)  ;;
        yes) ;;
        *)   IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT=$withval 
      esac
    ]
  )
  AC_MSG_RESULT($IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT)

  AC_DEFINE_UNQUOTED([IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT], 
                     ["$IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT"], 
                     [Define default ipmi_monitoring_sensor config_file.])
  AC_SUBST(IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT)
])
