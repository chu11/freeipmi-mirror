noinst_HEADERS = \
	tool-event-common.h

noinst_LTLIBRARIES = \
	libtoolevent.la

libtoolevent_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include 

libtoolevent_la_SOURCES = \
	tool-event-common.c
