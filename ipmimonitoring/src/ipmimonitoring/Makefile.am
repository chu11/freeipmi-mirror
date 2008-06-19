##*****************************************************************************
## $Id: Makefile.am,v 1.12 2008-06-19 21:24:22 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmimonitoring.h \
	ipmimonitoring-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/ipmimonitoring/src/libipmimonitoring \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmimonitoring

ipmimonitoring_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la

ipmimonitoring_SOURCES = \
	ipmimonitoring.c \
	ipmimonitoring-argp.c

$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
