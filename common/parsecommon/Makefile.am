noinst_HEADERS = \
	parse-common.h

noinst_LTLIBRARIES = \
	libparsecommon.la

libparsecommon_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include 

libparsecommon_la_SOURCES = \
	parse-common.c
