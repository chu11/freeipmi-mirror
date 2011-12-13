noinst_HEADERS = \
	ipmi-debug-common.h

noinst_LTLIBRARIES = libdebug.la

libdebug_la_CPPFLAGS = \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libdebug_la_SOURCES = \
	ipmi-debug.c \
	ipmi-debug-common.c \
	ipmi-debug-inband.c \
	ipmi-debug-lan.c \
	ipmi-debug-rmcp.c \
	ipmi-debug-rmcpplus.c \
	ipmi-debug-sdr.c
