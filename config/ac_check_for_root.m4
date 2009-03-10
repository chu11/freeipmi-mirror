##*****************************************************************************
## $Id: ac_check_for_root.m4,v 1.1 2009-03-10 17:54:08 chu11 Exp $
##*****************************************************************************
## This configure option will allow packagers to disable the check for root
## in FreeIPMI tools before executing in-band communication.
##
## This may be desired in some environments where permission checks for
## IPMI are defined more by device files (i.e. /dev/ipmi).
##*****************************************************************************
AC_DEFUN([AC_DONT_CHECK_FOR_ROOT],
[
  AC_MSG_CHECKING([whether tools should check for root before executing in-band
                   IPMI communication])
  AC_ARG_WITH([check-for-root],
    AC_HELP_STRING([--check-for-root], [check for root before executing in-band IPMI communication]),
    [ case "$withval" in
        yes) ac_dont_check_for_root=yes ;;
        no)  ac_dont_check_for_root=no ;;
        *)   ac_dont_check_for_root=yes ;; 
      esac
    ]
  )
  AC_MSG_RESULT([${ac_dont_check_for_root=no}])

  if test "$ac_dont_check_for_root" = "yes"; then
    AC_DEFINE([IPMI_DONT_CHECK_FOR_ROOT], [1], [Define to 1 to not check for root])
  fi
])
