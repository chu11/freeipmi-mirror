##*****************************************************************************
## $Id: Makefile.am,v 1.15 2008-06-19 21:24:22 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmiconsole_.h \
	ipmiconsole-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/ipmiconsole/src/libipmiconsole \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmiconsole

ipmiconsole_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la \
	$(top_builddir)/ipmiconsole/src/libipmiconsole/libipmiconsole.la

ipmiconsole_SOURCES = \
	ipmiconsole.c \
	ipmiconsole-argp.c

$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/ipmiconsole/src/libipmiconsole/libipmiconsole.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
