noinst_HEADERS = \
	tool-common.h \
	tool-config-file-common.h \
	tool-cmdline-common.h \
	tool-fiid-wrappers.h \
	tool-sdr-cache-common.h \
	tool-sensor-common.h

noinst_LTLIBRARIES = \
	libtoolcommon.la

libtoolcommon_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include 

libtoolcommon_la_SOURCES = \
	tool-common.c \
	tool-config-file-common.c \
	tool-cmdline-common.c \
	tool-sdr-cache-common.c \
	tool-sensor-common.c
