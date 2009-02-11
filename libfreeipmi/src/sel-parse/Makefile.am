##*****************************************************************************
## $Id: Makefile.am,v 1.3.10.3 2009-02-11 17:37:39 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sel-parse-defs.h \
	ipmi-sel-parse-common.h \
	ipmi-sel-parse-string.h \
	ipmi-sel-parse-trace.h \
	ipmi-sel-parse-util.h

noinst_LTLIBRARIES = libselparse.la 

libselparse_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libselparse_la_SOURCES = \
	ipmi-sel-parse.c \
	ipmi-sel-parse-common.c \
	ipmi-sel-parse-string.c \
	ipmi-sel-parse-util.c
