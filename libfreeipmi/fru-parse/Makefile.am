##*****************************************************************************
## $Id: Makefile.am,v 1.2 2009-04-17 23:50:34 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-fru-parse-common.h \
	ipmi-fru-parse-defs.h \
	ipmi-fru-parse-trace.h \
	ipmi-fru-parse-util.h

noinst_LTLIBRARIES = libfruparse.la 

libfruparse_la_CPPFLAGS = \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libfruparse_la_SOURCES = \
	ipmi-fru-parse.c \
	ipmi-fru-parse-common.c \
	ipmi-fru-parse-data.c \
	ipmi-fru-parse-util.c
