##*****************************************************************************
## $Id: Makefile.am,v 1.12.2.2 2008-06-06 16:21:10 chu11 Exp $
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
	$(top_builddir)/common/src/libmiscutils.la \
	$(top_builddir)/common/src/libfreeipmiportability.la \
	$(top_builddir)/ipmiconsole/src/libipmiconsole/libipmiconsole.la

ipmiconsole_SOURCES = \
	ipmiconsole.c \
	ipmiconsole-argp.c

