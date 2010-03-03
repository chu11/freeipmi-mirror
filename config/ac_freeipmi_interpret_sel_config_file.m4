##*****************************************************************************
## $Id: ac_freeipmi_interpret_sel_config_file.m4,v 1.1.2.1 2010-03-03 23:35:01 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_INTERPRET_SEL_CONFIG_FILE],
[
# Must expand nested unquoting
  INTERPRET_SEL_CONFIG_FILE_DEFAULT_TMP1="`eval echo ${sysconfdir}/freeipmi_interpret_sel.conf`"
  INTERPRET_SEL_CONFIG_FILE_DEFAULT_TMP2="`echo $INTERPRET_SEL_CONFIG_FILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  INTERPRET_SEL_CONFIG_FILE_DEFAULT="`eval echo $INTERPRET_SEL_CONFIG_FILE_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([INTERPRET_SEL_CONFIG_FILE_DEFAULT], 
                     ["$INTERPRET_SEL_CONFIG_FILE_DEFAULT"], 
                     [Define default interpret sel config file.])
  AC_SUBST(INTERPRET_SEL_CONFIG_FILE_DEFAULT)
])
