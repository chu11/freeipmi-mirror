noinst_LTLIBRARIES = \
	libparsecommon.la

libparsecommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libparsecommon_la_SOURCES = \
	parse-common.c \
	parse-common.h
