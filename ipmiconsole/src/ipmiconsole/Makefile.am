##*****************************************************************************
## $Id: Makefile.am,v 1.1 2006-11-06 00:13:12 chu11 Exp $
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

ipmiconsole_CFLAGS = -I$(srcdir)/../libipmiconsole \
		     -I$(srcdir)/../../../common/src \
		     -I$(srcdir)/../../../libfreeipmi/include

ipmiconsole_LDADD = ../../../common/src/libllnlcommon.la \
		    ../libipmiconsole/libipmiconsole.la

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmiconsole/libipmiconsole.la: force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
