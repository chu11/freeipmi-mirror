##*****************************************************************************
## $Id: ac_ipmidetect_config_file.m4,v 1.2 2007-08-06 23:00:12 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMIDETECT_CONFIG_FILE],
[
  IPMIDETECT_CONFIG_FILE_DEFAULT=/etc/ipmidetect.conf

  AC_MSG_CHECKING([for ipmidetect config file default path])
  AC_ARG_WITH([ipmidetect-config-file],
    AC_HELP_STRING([--with-ipmidetect-config-file=PATH], 
                   [Specify default ipmidetect config file path]),
    [ case "$withval" in
        no)  ;;
        yes) ;;
        *)   IPMIDETECT_CONFIG_FILE_DEFAULT=$withval 
      esac
    ]
  )
  AC_MSG_RESULT($IPMIDETECT_CONFIG_FILE_DEFAULT)

  AC_DEFINE_UNQUOTED([IPMIDETECT_CONFIG_FILE_DEFAULT], 
                     ["$IPMIDETECT_CONFIG_FILE_DEFAULT"], 
                     [Define default ipmidetect config_file.])
  AC_SUBST(IPMIDETECT_CONFIG_FILE_DEFAULT)
])
