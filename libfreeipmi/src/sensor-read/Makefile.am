##*****************************************************************************
## $Id: Makefile.am,v 1.2 2009-01-30 18:04:14 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sensor-read-defs.h

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
