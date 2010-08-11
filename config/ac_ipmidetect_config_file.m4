##*****************************************************************************
## $Id: ac_ipmidetect_config_file.m4,v 1.2 2007-12-14 19:16:19 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMIDETECT_CONFIG_FILE],
[
# Must expand nested unquoting
  IPMIDETECT_CONFIG_FILE_DEFAULT_TMP1="`eval echo $FREEIPMI_SYSCONFDIR/ipmidetect.conf`"
  IPMIDETECT_CONFIG_FILE_DEFAULT_TMP2="`echo $IPMIDETECT_CONFIG_FILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMIDETECT_CONFIG_FILE_DEFAULT="`eval echo $IPMIDETECT_CONFIG_FILE_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([IPMIDETECT_CONFIG_FILE_DEFAULT], 
                     ["$IPMIDETECT_CONFIG_FILE_DEFAULT"], 
                     [Define default ipmidetect config file.])
  AC_SUBST(IPMIDETECT_CONFIG_FILE_DEFAULT)

# Must expand nested unquoting
# Legacy - don't store in /etc/freeipmi/
  IPMIDETECT_CONFIG_FILE_LEGACY_TMP1="`eval echo ${sysconfdir}/ipmidetect.conf`"
  IPMIDETECT_CONFIG_FILE_LEGACY_TMP2="`echo $IPMIDETECT_CONFIG_FILE_LEGACY_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMIDETECT_CONFIG_FILE_LEGACY="`eval echo $IPMIDETECT_CONFIG_FILE_LEGACY_TMP2`"

  AC_DEFINE_UNQUOTED([IPMIDETECT_CONFIG_FILE_LEGACY], 
                     ["$IPMIDETECT_CONFIG_FILE_LEGACY"], 
                     [Define legacy ipmidetect config file.])
])
