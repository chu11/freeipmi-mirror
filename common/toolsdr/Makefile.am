noinst_HEADERS = \
	tool-sdr-cache-common.h

noinst_LTLIBRARIES = \
	libtoolsdr.la

libtoolsdr_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include 

libtoolsdr_la_SOURCES = \
	tool-sdr-cache-common.c
