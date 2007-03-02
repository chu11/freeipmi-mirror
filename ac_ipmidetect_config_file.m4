##*****************************************************************************
## $Id: ac_ipmidetect_config_file.m4,v 1.1 2007-03-02 00:56:26 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMIDETECT_CONFIG_FILE],
[
  IPMIDETECT_CONF_FILE=/etc/ipmidetect.conf

  AC_MSG_CHECKING([for ipmidetect config file default path])
  AC_ARG_WITH([ipmidetect-config-file],
    AC_HELP_STRING([--with-ipmidetect-config-file=PATH], 
                   [Specify default ipmidetect config file path]),
    [ case "$withval" in
        no)  ;;
        yes) ;;
        *)   IPMIDETECT_CONF_FILE=$withval 
      esac
    ]
  )
  AC_MSG_RESULT($IPMIDETECT_CONF_FILE)

  AC_DEFINE_UNQUOTED([IPMIDETECT_CONF_FILE], 
                     ["$IPMIDETECT_CONF_FILE"], 
                     [Define default ipmidetect config_file.])
  AC_SUBST(IPMIDETECT_CONF_FILE)
])
