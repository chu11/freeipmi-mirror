##*****************************************************************************
## $Id: Makefile.am,v 1.3 2009-02-23 22:29:22 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sensor-read-defs.h \
	ipmi-sensor-read-trace.h \
	ipmi-sensor-read-util.h

noinst_LTLIBRARIES = libsensorread.la 

libsensorread_la_CPPFLAGS = \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libsensorread_la_SOURCES = \
	ipmi-sensor-read.c \
	ipmi-sensor-read-util.c
