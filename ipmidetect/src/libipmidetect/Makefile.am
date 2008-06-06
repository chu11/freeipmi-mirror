##*****************************************************************************
## $Id: Makefile.am,v 1.3.14.2 2008-06-06 16:21:11 chu11 Exp $
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
	-I$(top_builddir)/common/src

libipmidetect_la_LDFLAGS = \
	-version-info @LIBIPMIDETECT_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmidetect_la_LIBADD = \
	$(top_builddir)/common/src/libmiscutils.la \
	$(top_builddir)/common/src/libfreeipmiportability.la

libipmidetect_la_SOURCES = \
	ipmidetect.c

EXTRA_DIST = ipmidetect.map

