noinst_LTLIBRARIES = \
	libtoolsensor.la

libtoolsensor_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/common/toolsdr \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libtoolsensor_la_SOURCES = \
	tool-sensor-common.c \
	tool-sensor-common.h
