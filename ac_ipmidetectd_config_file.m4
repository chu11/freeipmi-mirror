##*****************************************************************************
## $Id: ac_ipmidetectd_config_file.m4,v 1.1.10.1 2007-08-07 03:18:46 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMIDETECTD_CONFIG_FILE],
[
# Must expand nested unquoting
  IPMIDETECTD_CONFIG_FILE_DEFAULT_TMP1="`eval echo ${sysconfdir}/ipmidetectd.conf`"
  IPMIDETECTD_CONFIG_FILE_DEFAULT_TMP2="`echo $IPMIDETECTD_CONFIG_FILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMIDETECTD_CONFIG_FILE_DEFAULT="`eval echo $IPMIDETECTD_CONFIG_FILE_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([IPMIDETECTD_CONFIG_FILE_DEFAULT], 
                     ["$IPMIDETECTD_CONFIG_FILE_DEFAULT"], 
                     [Define default ipmidetectd config_file.])
  AC_SUBST(IPMIDETECTD_CONFIG_FILE_DEFAULT)
])
