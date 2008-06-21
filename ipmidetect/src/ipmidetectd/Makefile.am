##*****************************************************************************
## $Id: Makefile.am,v 1.8 2008-06-21 05:46:33 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	debug.h \
	ipmidetectd.h \
	ipmidetectd_config.h \
	ipmidetectd_loop.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmidetectd

ipmidetectd_LDADD = \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

ipmidetectd_SOURCES = \
	debug.c \
	ipmidetectd.c \
	ipmidetectd_config.c \
	ipmidetectd_loop.c

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:

