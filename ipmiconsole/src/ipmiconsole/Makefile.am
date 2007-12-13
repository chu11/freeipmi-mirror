##*****************************************************************************
## $Id: Makefile.am,v 1.8.4.2 2007-12-13 01:21:59 chu11 Exp $
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

ipmiconsole_LDADD = ../../../common/src/libmiscutils.la \
                    ../../../common/src/libtoolcommon.la \
		    ../libipmiconsole/libipmiconsole.la

../../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmiconsole/libipmiconsole.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
