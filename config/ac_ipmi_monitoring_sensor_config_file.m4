##*****************************************************************************
## $Id: ac_ipmi_monitoring_sensor_config_file.m4,v 1.2 2007-12-14 19:16:19 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMI_MONITORING_SENSOR_CONFIG_FILE],
[
# Must expand nested unquoting
# Legacy - don't store in /etc/freeipmi/
  IPMI_MONITORING_SENSOR_CONFIG_FILE_LEGACY_TMP1="`eval echo ${sysconfdir}/ipmi_monitoring_sensors.conf`"
  IPMI_MONITORING_SENSOR_CONFIG_FILE_LEGACY_TMP2="`echo $IPMI_MONITORING_SENSOR_CONFIG_FILE_LEGACY_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMI_MONITORING_SENSOR_CONFIG_FILE_LEGACY="`eval echo $IPMI_MONITORING_SENSOR_CONFIG_FILE_LEGACY_TMP2`"

  AC_DEFINE_UNQUOTED([IPMI_MONITORING_SENSOR_CONFIG_FILE_LEGACY], 
                     ["$IPMI_MONITORING_SENSOR_CONFIG_FILE_LEGACY"], 
                     [Define legacy ipmi_monitoring_sensor config file.])
  AC_SUBST(IPMI_MONITORING_SENSOR_CONFIG_FILE_LEGACY)
])
