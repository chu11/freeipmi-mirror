noinst_LTLIBRARIES = libinterface.la

libinterface_la_CPPFLAGS = \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libinterface_la_SOURCES = \
	ipmi-ipmb-interface.c \
	ipmi-kcs-interface.c \
	ipmi-lan-interface.c \
	ipmi-rmcpplus-interface.c \
	rmcp-interface.c
