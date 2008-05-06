##*****************************************************************************
## $Id: ac_freeipmi_hostmap.m4,v 1.1.2.1 2008-05-06 20:41:26 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_FREEIPMI_HOSTMAP],
[
  # Must expand nested unquoting
  FREEIPMI_HOSTMAP_DEFAULT_TMP1="`eval echo ${sysconfdir}/freeipmi.hostmap`"
  FREEIPMI_HOSTMAP_DEFAULT_TMP2="`echo $FREEIPMI_HOSTMAP_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  FREEIPMI_HOSTMAP_DEFAULT="`eval echo $FREEIPMI_HOSTMAP_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([FREEIPMI_HOSTMAP_DEFAULT], 
                     ["$FREEIPMI_HOSTMAP_DEFAULT"], 
                     [Define default ipmi_monitoring_sensor config_file.])
  AC_SUBST(FREEIPMI_HOSTMAP_DEFAULT)
])
