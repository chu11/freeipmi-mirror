##*****************************************************************************
## $Id: Makefile.am,v 1.6 2008-06-21 04:50:12 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/ipmidetect.map
OTHER_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

include_HEADERS = ipmidetect.h

lib_LTLIBRARIES = libipmidetect.la 

libipmidetect_la_CPPFLAGS = \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/util

libipmidetect_la_LDFLAGS = \
	-version-info @LIBIPMIDETECT_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmidetect_la_LIBADD = \
	$(top_builddir)/common/src/util/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la

libipmidetect_la_SOURCES = \
	ipmidetect.c

$(top_builddir)/common/src/util/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:

EXTRA_DIST = ipmidetect.map

