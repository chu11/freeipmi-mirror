noinst_LTLIBRARIES = \
	libtoolevent.la

libtoolevent_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/common/tooloem \
	-I$(top_srcdir)/common/toolsdr \
	-I$(top_srcdir)/common/toolsensor \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libtoolevent_la_SOURCES = \
	tool-event-common.c \
	tool-event-common.h
