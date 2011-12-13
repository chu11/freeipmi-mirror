##*****************************************************************************
## $Id: Makefile.am,v 1.3 2009-02-23 22:29:21 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sdr-parse-defs.h \
	ipmi-sdr-parse-trace.h \
	ipmi-sdr-parse-util.h

noinst_LTLIBRARIES = libsdrparse.la 

libsdrparse_la_CPPFLAGS = \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libsdrparse_la_SOURCES = \
	ipmi-sdr-parse.c \
	ipmi-sdr-parse-util.c
