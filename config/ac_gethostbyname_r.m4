##*****************************************************************************
## $Id: ac_gethostbyname_r.m4,v 1.4 2009-07-09 20:50:14 chu11 Exp $
##*****************************************************************************
##
## achu: Most parts of this gethostbyname_r testing is from the
## original done by Caolan McNamara <caolan@skynet.ie>.  That one
## had trouble dealing with libnsl and Solaris.  So I had to re-write
## it a bit.
##
## See original: http://www.csn.ul.ie/~caolan/publink/gethostbyname_r
##
##*****************************************************************************
AC_DEFUN([AC_GETHOSTBYNAME_R],
[
  AC_CHECK_FUNC([gethostbyname_r],
                [have_gethostbyname_r=yes],
                AC_CHECK_LIB([nsl],
                             [gethostbyname_r],
                             [LIBS="$LIBS -lnsl" have_gethostbyname_r=yes],
                             [have_gethostbyname_r=no]))

  if test "x${have_gethostbyname_r}" = "xyes"; then
     AC_TRY_LINK(
     [
     #include <netdb.h>
     ],
     [
     char *name;
     struct hostent *he;
     struct hostent_data data;
     (void) gethostbyname_r(name, he, &data);
     ],
     [ac_cv_func_which_gethostbyname_r=three], [
          AC_TRY_LINK(
          [
          #include <netdb.h>
          ],
          [
          char *name;
          struct hostent *he, *res;
          char buffer[2048];
          int buflen = 2048;
          int h_errnop;
          (void) gethostbyname_r(name, he, buffer, buflen, &res, &h_errnop);
          ],
          [ac_cv_func_which_gethostbyname_r=six], [
              AC_TRY_LINK(
              [
              #include <netdb.h>
              ],
              [
              char *name;
              struct hostent *he;
              char buffer[2048];
              int buflen = 2048;
              int h_errnop;
              (void) gethostbyname_r(name, he, buffer, buflen, &h_errnop);
              ],
              [ac_cv_func_which_gethostbyname_r=five],
              [ac_cv_func_which_gethostbyname_r=no])
          ])
     ])

##  define HAVE_FUNC_GETHOSTBYNAME_R_6 if gethostbyname_r needs 6 arguments (e.g linux)
##  define HAVE_FUNC_GETHOSTBYNAME_R_5 if gethostbyname_r needs 5 arguments (e.g. solaris)
##  define HAVE_FUNC_GETHOSTBYNAME_R_3 if gethostbyname_r needs 3 arguments (e.g. osf/1)

     if test "x${ac_cv_func_which_gethostbyname_r}" = "xsix"; then
        AC_DEFINE(HAVE_FUNC_GETHOSTBYNAME_R_6, [], [Define gethostbyname_r with 6 args])
     elif test "x${ac_cv_func_which_gethostbyname_r}" = "xfive"; then
        AC_DEFINE(HAVE_FUNC_GETHOSTBYNAME_R_5, [], [Define gethostbyname_r with 5 args])
     elif test "x${ac_cv_func_which_gethostbyname_r}" = "xthree"; then
        AC_DEFINE(HAVE_FUNC_GETHOSTBYNAME_R_3, [], [Define gethostbyname_r with 3 args])
     fi
  fi

])
