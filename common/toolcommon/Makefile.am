noinst_LTLIBRARIES = \
	libtoolcommon.la

libtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/common/toolsdr \
	-I$(top_srcdir)/common/toolsensor \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libtoolcommon_la_SOURCES = \
	tool-common.c \
	tool-common.h \
	tool-config-file-common.c \
	tool-config-file-common.h \
	tool-cmdline-common.c \
	tool-cmdline-common.h
