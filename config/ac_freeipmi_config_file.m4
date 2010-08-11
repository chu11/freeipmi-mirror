##*****************************************************************************
## $Id: ac_freeipmi_config_file.m4,v 1.1 2008-05-20 22:49:04 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_FREEIPMI_CONFIG_FILE],
[
  # Must expand nested unquoting
  FREEIPMI_CONFIG_FILE_DEFAULT_TMP1="`eval echo $FREEIPMI_SYSCONFDIR/freeipmi.conf`"
  FREEIPMI_CONFIG_FILE_DEFAULT_TMP2="`echo $FREEIPMI_CONFIG_FILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  FREEIPMI_CONFIG_FILE_DEFAULT="`eval echo $FREEIPMI_CONFIG_FILE_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([FREEIPMI_CONFIG_FILE_DEFAULT],
                     ["$FREEIPMI_CONFIG_FILE_DEFAULT"],
                     [Define default freeipmi config file.])
  AC_SUBST(FREEIPMI_CONFIG_FILE_DEFAULT)

# Must expand nested unquoting
# Legacy - don't store in /etc/freeipmi/
  FREEIPMI_CONFIG_FILE_LEGACY_TMP1="`eval echo ${sysconfdir}/freeipmi.conf`"
  FREEIPMI_CONFIG_FILE_LEGACY_TMP2="`echo $FREEIPMI_CONFIG_FILE_LEGACY_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  FREEIPMI_CONFIG_FILE_LEGACY="`eval echo $FREEIPMI_CONFIG_FILE_LEGACY_TMP2`"

  AC_DEFINE_UNQUOTED([FREEIPMI_CONFIG_FILE_LEGACY],
                     ["$FREEIPMI_CONFIG_FILE_LEGACY"],
                     [Define legacy freeipmi config file.])
])
