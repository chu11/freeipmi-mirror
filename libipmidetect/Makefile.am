if WITH_PKG_CONFIG
pkgconfig_DATA = libipmidetect.pc
endif

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/ipmidetect.map
OTHER_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

nodist_include_HEADERS = ipmidetect.h

lib_LTLIBRARIES = libipmidetect.la 

libipmidetect_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability

libipmidetect_la_LDFLAGS = \
	-version-info @LIBIPMIDETECT_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmidetect_la_LIBADD = \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/portability/libportability.la

libipmidetect_la_SOURCES = \
	ipmidetect.c

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

EXTRA_DIST = ipmidetect.map

