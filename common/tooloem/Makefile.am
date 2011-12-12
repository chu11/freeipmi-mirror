noinst_HEADERS = \
	tool-oem-common.h

noinst_LTLIBRARIES = \
	libtooloem.la

libtooloem_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include 

libtooloem_la_SOURCES = \
	tool-oem-common.c
