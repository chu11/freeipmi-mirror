noinst_LTLIBRARIES = libutil.la

libutil_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/src \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/util \
	-D_GNU_SOURCE \
	-D_REENTRANT

libutil_la_SOURCES = \
	ipmi-cipher-suite-util.c \
	ipmi-error-util.c \
	ipmi-lan-util.c \
	ipmi-sensor-and-event-code-tables-util.c \
	ipmi-sensor-util.c \
	ipmi-rmcpplus-util.c \
	ipmi-util.c \
	rmcp-util.c
