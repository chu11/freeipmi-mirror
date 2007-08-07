##*****************************************************************************
## $Id: ac_ipmipower_config_file.m4,v 1.1.10.1 2007-08-07 03:18:47 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMIPOWER_CONFIG_FILE],
[
# Must expand nested unquoting
  IPMIPOWER_CONFIG_FILE_DEFAULT_TMP1="`eval echo ${sysconfdir}/ipmipower.conf`"
  IPMIPOWER_CONFIG_FILE_DEFAULT_TMP2="`echo $IPMIPOWER_CONFIG_FILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  IPMIPOWER_CONFIG_FILE_DEFAULT="`eval echo $IPMIPOWER_CONFIG_FILE_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([IPMIPOWER_CONFIG_FILE_DEFAULT], 
                     ["$IPMIPOWER_CONFIG_FILE_DEFAULT"], 
                     [Define default ipmipower config_file.])
  AC_SUBST(IPMIPOWER_CONFIG_FILE_DEFAULT)
])
