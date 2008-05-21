##*****************************************************************************
## $Id: Makefile.am,v 1.12 2008-05-21 16:02:33 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmiconsole_.h \
	ipmiconsole-argp.h

sbin_PROGRAMS = ipmiconsole

ipmiconsole_SOURCES = \
	ipmiconsole.c \
	ipmiconsole-argp.c

ipmiconsole_CPPFLAGS = -I$(srcdir)/../libipmiconsole \
		       -I$(srcdir)/../../../common/src \
		       -I$(srcdir)/../../../libfreeipmi/include	

ipmiconsole_LDADD = ../../../common/src/libtoolcommon.la \
		    ../../../common/src/libmiscutils.la \
		    ../libipmiconsole/libipmiconsole.la

../../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmiconsole/libipmiconsole.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
