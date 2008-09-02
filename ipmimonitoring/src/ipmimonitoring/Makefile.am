##*****************************************************************************
## $Id: Makefile.am,v 1.20 2008-09-02 16:12:14 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmimonitoring.h \
	ipmimonitoring-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/ipmimonitoring/src/libipmimonitoring \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmimonitoring

ipmimonitoring_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la

ipmimonitoring_SOURCES = \
	ipmimonitoring.c \
	ipmimonitoring-argp.c

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
