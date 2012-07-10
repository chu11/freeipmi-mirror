noinst_LTLIBRARIES = \
	libdebugutil.la

libdebugutil_la_CPPFLAGS = \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libdebugutil_la_SOURCES = \
	debug-util.c \
	debug-util.h
