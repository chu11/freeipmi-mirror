##*****************************************************************************
## $Id: Makefile.am,v 1.10 2008-09-17 22:56:04 chu11 Exp $
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
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability

libipmidetect_la_LDFLAGS = \
	-version-info @LIBIPMIDETECT_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmidetect_la_LIBADD = \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la

libipmidetect_la_SOURCES = \
	ipmidetect.c

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

EXTRA_DIST = ipmidetect.map

