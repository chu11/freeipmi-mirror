##*****************************************************************************
## $Id: ac_ipmidetectd_config_file.m4,v 1.1 2007-03-02 00:56:26 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMIDETECTD_CONFIG_FILE],
[
  IPMIDETECTD_CONFIG_FILE_DEFAULT=/etc/ipmidetectd.conf

  AC_MSG_CHECKING([for ipmidetectd config file default path])
  AC_ARG_WITH([ipmidetectd-config-file],
    AC_HELP_STRING([--with-ipmidetectd-config-file=PATH], 
                   [Specify default ipmidetectd config file path]),
    [ case "$withval" in
        no)  ;;
        yes) ;;
        *)   IPMIDETECTD_CONFIG_FILE_DEFAULT=$withval 
      esac
    ]
  )
  AC_MSG_RESULT($IPMIDETECTD_CONFIG_FILE_DEFAULT)

  AC_DEFINE_UNQUOTED([IPMIDETECTD_CONFIG_FILE_DEFAULT], 
                     ["$IPMIDETECTD_CONFIG_FILE_DEFAULT"], 
                     [Define default ipmidetectd config_file.])
  AC_SUBST(IPMIDETECTD_CONFIG_FILE_DEFAULT)
])
