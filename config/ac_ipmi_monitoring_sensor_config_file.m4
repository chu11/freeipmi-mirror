##*****************************************************************************
## $Id: ac_ipmi_monitoring_sensor_config_file.m4,v 1.1.2.1 2007-12-14 18:21:13 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMI_MONITORING_SENSOR_CONFIG_FILE],
[
# Must expand nested unquoting
  IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT_TMP1="`eval echo ${sysconfdir}/ipmi_monitoring_sensors.conf`"
  IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT_TMP2="`echo $IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT="`eval echo $IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT], 
                     ["$IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT"], 
                     [Define default ipmi_monitoring_sensor config_file.])
  AC_SUBST(IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT)
])
