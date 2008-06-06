##*****************************************************************************
## $Id: Makefile.am,v 1.4.6.2 2008-06-06 16:21:10 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS = ipmidetect

AM_CPPFLAGS = \
	-I $(top_builddir)/common/src \
	-I $(top_builddir)/ipmidetect/src/libipmidetect \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmidetect_LDADD = \
	$(top_builddir)/common/src/libmiscutils.la \
	$(top_builddir)/common/src/libfreeipmiportability.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la

ipmidetect_SOURCES = ipmidetect.c

