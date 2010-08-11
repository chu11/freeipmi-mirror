##*****************************************************************************
## $Id: ac_ipmi_monitoring_sensor_config_file.m4,v 1.2 2007/12/14 19:16:19 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMICONSOLE_CONFIG_FILE],
[
# Must expand nested unquoting
# Legacy - don't store in /etc/freeipmi/
  IPMICONSOLE_CONFIG_FILE_LEGACY_TMP1="`eval echo ${sysconfdir}/ipmiconsole.conf`"
  IPMICONSOLE_CONFIG_FILE_LEGACY_TMP2="`echo $IPMICONSOLE_CONFIG_FILE_LEGACY_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMICONSOLE_CONFIG_FILE_LEGACY="`eval echo $IPMICONSOLE_CONFIG_FILE_LEGACY_TMP2`"

  AC_DEFINE_UNQUOTED([IPMICONSOLE_CONFIG_FILE_LEGACY], 
                     ["$IPMICONSOLE_CONFIG_FILE_LEGACY"], 
                     [Define legacy ipmiconsole config file.])
  AC_SUBST(IPMICONSOLE_CONFIG_FILE_LEGACY)
])
