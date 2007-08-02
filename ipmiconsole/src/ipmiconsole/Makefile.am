##*****************************************************************************
## $Id: Makefile.am,v 1.8 2007-08-02 20:50:13 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmiconsole_config.h \
	error.h

sbin_PROGRAMS = ipmiconsole

ipmiconsole_SOURCES = \
	ipmiconsole.c \
	ipmiconsole_config.c \
	error.c

ipmiconsole_CPPFLAGS = -I$(srcdir)/../libipmiconsole \
		       -I$(srcdir)/../../../common/src \
		       -I$(srcdir)/../../../libfreeipmi/include	

ipmiconsole_LDADD = ../../../common/src/libcmdlineparsecommon.la \
		    ../../../common/src/libllnlcommon.la \
                    ../../../common/src/libtoolcommon.la \
		    ../libipmiconsole/libipmiconsole.la

../../../common/src/libcmdlineparsecommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmiconsole/libipmiconsole.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
