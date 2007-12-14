noinst_LTLIBRARIES = libinterface.la

libinterface_la_SOURCES = \
	ipmi-kcs-interface.c \
	ipmi-lan-interface.c \
	ipmi-rmcpplus-interface.c \
	rmcp-interface.c

libinterface_la_CFLAGS = -Wall
libinterface_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT
