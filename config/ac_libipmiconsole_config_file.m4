##*****************************************************************************
## $Id: ac_freeipmi_interpret_sel_config_file.m4,v 1.2 2010-03-10 19:36:31 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_LIBIPMICONSOLE_CONFIG_FILE],
[
# Must expand nested unquoting
  LIBIPMICONSOLE_CONFIG_FILE_DEFAULT_TMP1="`eval echo $FREEIPMI_SYSCONFDIR/libipmiconsole.conf`"
  LIBIPMICONSOLE_CONFIG_FILE_DEFAULT_TMP2="`echo $LIBIPMICONSOLE_CONFIG_FILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  LIBIPMICONSOLE_CONFIG_FILE_DEFAULT="`eval echo $LIBIPMICONSOLE_CONFIG_FILE_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([LIBIPMICONSOLE_CONFIG_FILE_DEFAULT], 
                     ["$LIBIPMICONSOLE_CONFIG_FILE_DEFAULT"], 
                     [Define default libipmiconsole config file.])
  AC_SUBST(LIBIPMICONSOLE_CONFIG_FILE_DEFAULT)
])
