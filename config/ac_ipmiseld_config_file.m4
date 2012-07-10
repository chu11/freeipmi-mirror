##*****************************************************************************
## $Id: ac_ipmiseld_config_file.m4,v 1.2 2007-12-14 19:16:19 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMISELD_CONFIG_FILE],
[
# Must expand nested unquoting
  IPMISELD_CONFIG_FILE_DEFAULT_TMP1="`eval echo $FREEIPMI_SYSCONFDIR/ipmiseld.conf`"
  IPMISELD_CONFIG_FILE_DEFAULT_TMP2="`echo $IPMISELD_CONFIG_FILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMISELD_CONFIG_FILE_DEFAULT="`eval echo $IPMISELD_CONFIG_FILE_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([IPMISELD_CONFIG_FILE_DEFAULT], 
                     ["$IPMISELD_CONFIG_FILE_DEFAULT"], 
                     [Define default ipmiseld config file.])
  AC_SUBST(IPMISELD_CONFIG_FILE_DEFAULT)
])
