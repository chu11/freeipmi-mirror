noinst_LTLIBRARIES = libutil.la

libutil_la_SOURCES = \
	ipmi-cipher-suite-util.c \
	ipmi-error-util.c \
	ipmi-lan-util.c \
	ipmi-sensor-util.c \
	ipmi-rmcpplus-util.c \
	ipmi-util.c \
	rmcp-util.c

libutil_la_CFLAGS = -Wall
libutil_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT
