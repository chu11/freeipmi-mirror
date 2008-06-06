##*****************************************************************************
## $Id: Makefile.am,v 1.4.6.2 2008-06-06 16:21:11 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	debug.h \
	ipmidetectd.h \
	ipmidetectd_config.h \
	ipmidetectd_loop.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmidetectd

ipmidetectd_LDADD = \
	$(top_builddir)/common/src/libmiscutils.la \
	$(top_builddir)/common/src/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

ipmidetectd_SOURCES = \
	debug.c \
	ipmidetectd.c \
	ipmidetectd_config.c \
	ipmidetectd_loop.c

