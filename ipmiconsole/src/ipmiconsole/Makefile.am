##*****************************************************************************
## $Id: Makefile.am,v 1.19 2008-06-21 05:46:33 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmiconsole_.h \
	ipmiconsole-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/ipmiconsole/src/libipmiconsole \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmiconsole

ipmiconsole_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(top_builddir)/ipmiconsole/src/libipmiconsole/libipmiconsole.la

ipmiconsole_SOURCES = \
	ipmiconsole.c \
	ipmiconsole-argp.c

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/ipmiconsole/src/libipmiconsole/libipmiconsole.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
