noinst_LTLIBRARIES = liboem.la

liboem_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

liboem_la_SOURCES = \
	ipmi-oem-intel-node-manager.c
