##*****************************************************************************
## $Id: Makefile.am,v 1.2.4.2 2009-02-07 21:21:07 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sensor-read-defs.h \
	ipmi-sensor-read-trace.h

noinst_LTLIBRARIES = libsensorread.la 

libsensorread_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libsensorread_la_SOURCES = \
	ipmi-sensor-read.c
