noinst_LTLIBRARIES = \
	libtoolsdr.la

libtoolsdr_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libtoolsdr_la_SOURCES = \
	tool-sdr-cache-common.c \
	tool-sdr-cache-common.h
