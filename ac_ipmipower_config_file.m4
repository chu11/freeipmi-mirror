##*****************************************************************************
## $Id: ac_ipmipower_config_file.m4,v 1.1 2007-02-28 04:20:38 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_IPMIPOWER_CONFIG_FILE],
[
  IPMIPOWER_CONFIG_FILE_DEFAULT=/etc/ipmipower.conf

  AC_MSG_CHECKING([for ipmipower config file default path])
  AC_ARG_WITH([ipmipower-config-file],
    AC_HELP_STRING([--with-ipmipower-config-file=PATH], 
                   [Specify default ipmipower config file path]),
    [ case "$withval" in
        no)  ;;
        yes) ;;
        *)   IPMIPOWER_CONFIG_FILE_DEFAULT=$withval 
      esac
    ]
  )
  AC_MSG_RESULT($IPMIPOWER_CONFIG_FILE_DEFAULT)

  AC_DEFINE_UNQUOTED([IPMIPOWER_CONFIG_FILE_DEFAULT], 
                     ["$IPMIPOWER_CONFIG_FILE_DEFAULT"], 
                     [Define default ipmipower config_file.])
  AC_SUBST(IPMIPOWER_CONFIG_FILE_DEFAULT)
])
