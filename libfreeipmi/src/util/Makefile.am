noinst_LTLIBRARIES = libutil.la

libutil_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libutil_la_SOURCES = \
	ipmi-cipher-suite-util.c \
	ipmi-error-util.c \
	ipmi-ipmb-util.c \
	ipmi-lan-util.c \
	ipmi-sensor-and-event-code-tables-util.c \
	ipmi-sensor-util.c \
	ipmi-rmcpplus-util.c \
	ipmi-util.c \
	rmcp-util.c
