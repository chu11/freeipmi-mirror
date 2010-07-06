##*****************************************************************************
## $Id: ac_dont_check_for_random.m4,v 1.1 2010-07-06 21:41:24 chu11 Exp $
##*****************************************************************************
## This configure option will allow packagers to disable the check for
## /dev/urandom and /dev/random in FreeIPMI.
##
## This may be desired in some cross compile environments where this
## may not exist or may not be desireable.
##*****************************************************************************
AC_DEFUN([AC_DONT_CHECK_FOR_RANDOM],
[
  AC_MSG_CHECKING([whether tools should check for /dev/urandom or /dev/random])
  AC_ARG_WITH([dont-check-for-random],
    AC_HELP_STRING([--with-dont-check-for-random], [don't check for /dev/urandom or /dev/random]),
    [ case "$withval" in
        yes) ac_dont_check_for_random=yes ;;
        no)  ac_dont_check_for_random=no ;;
        *)   ac_dont_check_for_random=yes ;; 
      esac
    ]
  )
  AC_MSG_RESULT([${ac_dont_check_for_random=no}])

  if test "$ac_dont_check_for_random" = "yes"; then
    AC_DEFINE([IPMI_DONT_CHECK_FOR_RANDOM], [1], [Define to 1 to not check for /dev/urandom or /dev/random])
  fi
])
