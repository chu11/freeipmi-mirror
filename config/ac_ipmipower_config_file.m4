##*****************************************************************************
## $Id: ac_ipmi_monitoring_sensor_config_file.m4,v 1.2 2007/12/14 19:16:19 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMIPOWER_CONFIG_FILE],
[
# Must expand nested unquoting
# Legacy - don't store in /etc/freeipmi/
  IPMIPOWER_CONFIG_FILE_LEGACY_TMP1="`eval echo ${sysconfdir}/ipmipower.conf`"
  IPMIPOWER_CONFIG_FILE_LEGACY_TMP2="`echo $IPMIPOWER_CONFIG_FILE_LEGACY_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMIPOWER_CONFIG_FILE_LEGACY="`eval echo $IPMIPOWER_CONFIG_FILE_LEGACY_TMP2`"

  AC_DEFINE_UNQUOTED([IPMIPOWER_CONFIG_FILE_LEGACY], 
                     ["$IPMIPOWER_CONFIG_FILE_LEGACY"], 
                     [Define legacy ipmipower config file.])
  AC_SUBST(IPMIPOWER_CONFIG_FILE_LEGACY)
])
