noinst_LTLIBRARIES = libinterface.la

libinterface_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libinterface_la_SOURCES = \
	ipmi-ipmb-interface.c \
	ipmi-kcs-interface.c \
	ipmi-lan-interface.c \
	ipmi-rmcpplus-interface.c \
	rmcp-interface.c
