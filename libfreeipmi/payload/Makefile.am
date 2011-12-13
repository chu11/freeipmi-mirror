noinst_LTLIBRARIES = libpayload.la

libpayload_la_CPPFLAGS = \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libpayload_la_SOURCES = \
	ipmi-sol-payload.c
