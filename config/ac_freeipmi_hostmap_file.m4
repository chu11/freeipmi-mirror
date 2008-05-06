##*****************************************************************************
## $Id: ac_freeipmi_hostmap_file.m4,v 1.1.2.1 2008-05-06 22:15:17 chu11 Exp $
##*****************************************************************************

AC_DEFUN([AC_FREEIPMI_HOSTMAP_FILE],
[
  # Must expand nested unquoting
  FREEIPMI_HOSTMAP_FILE_DEFAULT_TMP1="`eval echo ${sysconfdir}/freeipmi.hostmap`"
  FREEIPMI_HOSTMAP_FILE_DEFAULT_TMP2="`echo $FREEIPMI_HOSTMAP_FILE_DEFAULT_TMP1 | sed 's/^NONE/$ac_default_prefix/'`"
  FREEIPMI_HOSTMAP_FILE_DEFAULT="`eval echo $FREEIPMI_HOSTMAP_FILE_DEFAULT_TMP2`"

  AC_DEFINE_UNQUOTED([FREEIPMI_HOSTMAP_FILE_DEFAULT], 
                     ["$FREEIPMI_HOSTMAP_FILE_DEFAULT"], 
                     [Define default freeipmi hostmap file.])
  AC_SUBST(FREEIPMI_HOSTMAP_FILE_DEFAULT)
])
