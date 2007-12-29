noinst_LTLIBRARIES = libdebug.la

libdebug_la_SOURCES = \
	ipmi-debug.c \
	ipmi-debug-common.c \
	ipmi-debug-lan.c \
	ipmi-debug-rmcp.c \
	ipmi-debug-rmcpplus.c \
	ipmi-debug-sdr.c

noinst_HEADERS = \
	ipmi-debug-common.h

libdebug_la_CFLAGS = -Wall
libdebug_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT
