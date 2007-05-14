##*****************************************************************************
## $Id: Makefile.am,v 1.3.2.1 2007-05-14 02:41:13 chu11 Exp $
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

ipmiconsole_LDADD = ../../../common/src/libllnlcommon.la \
                    ../../../common/src/libipmicommon.la \
		    ../libipmiconsole/libipmiconsole.la

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmiconsole/libipmiconsole.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
