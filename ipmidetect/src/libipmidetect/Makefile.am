##*****************************************************************************
## $Id: Makefile.am,v 1.1.10.1 2007-10-15 16:07:10 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/ipmidetect.map
OTHER_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

include_HEADERS = ipmidetect.h

lib_LTLIBRARIES = libipmidetect.la 

libipmidetect_la_SOURCES = \
	ipmidetect.c

libipmidetect_la_CPPFLAGS = \
	-I$(srcdir)/../../../common/src

libipmidetect_la_LIBADD = \
	../../../common/src/libllnlcommon.la

libipmidetect_la_LDFLAGS = -version-info @LIBIPMIDETECT_VERSION_INFO@ $(OTHER_FLAGS)

EXTRA_DIST = ipmidetect.map

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
