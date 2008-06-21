##*****************************************************************************
## $Id: Makefile.am,v 1.7 2008-06-21 04:50:12 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS = ipmidetect

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/util \
	-I$(top_builddir)/ipmidetect/src/libipmidetect \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmidetect_LDADD = \
	$(top_builddir)/common/src/util/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la

ipmidetect_SOURCES = ipmidetect.c

$(top_builddir)/common/src/util/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
