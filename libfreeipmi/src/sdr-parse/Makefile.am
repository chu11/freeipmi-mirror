##*****************************************************************************
## $Id: Makefile.am,v 1.2.2.3 2009-02-11 18:22:20 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sdr-parse-defs.h \
	ipmi-sdr-parse-trace.h \
	ipmi-sdr-parse-util.h

noinst_LTLIBRARIES = libsdrparse.la 

libsdrparse_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libsdrparse_la_SOURCES = \
	ipmi-sdr-parse.c \
	ipmi-sdr-parse-util.c
