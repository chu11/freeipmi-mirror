noinst_LTLIBRARIES = \
	libpingtool.la

libpingtool_la_CPPFLAGS = \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libpingtool_la_SOURCES = \
	ping-tool-common.c \
	ping-tool-common.h
