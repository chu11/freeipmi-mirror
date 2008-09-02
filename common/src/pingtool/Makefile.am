noinst_HEADERS = \
	ping-tool-common.h

noinst_LTLIBRARIES = \
	libpingtoolcommon.la

libpingtoolcommon_la_LIBADD = \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

libpingtoolcommon_la_CPPFLAGS = \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/libfreeipmi/include

libpingtoolcommon_la_SOURCES = \
	ping-tool-common.c

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
