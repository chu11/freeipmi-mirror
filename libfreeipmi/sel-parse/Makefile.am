##*****************************************************************************
## $Id: Makefile.am,v 1.7 2010-05-11 23:34:48 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sel-parse-defs.h \
	ipmi-sel-parse-common.h \
	ipmi-sel-parse-string.h \
	ipmi-sel-parse-string-dell.h \
	ipmi-sel-parse-string-fujitsu.h \
	ipmi-sel-parse-string-intel.h \
	ipmi-sel-parse-string-intel-node-manager.h \
	ipmi-sel-parse-string-inventec.h \
	ipmi-sel-parse-string-quanta.h \
	ipmi-sel-parse-string-sun.h \
	ipmi-sel-parse-trace.h \
	ipmi-sel-parse-util.h

noinst_LTLIBRARIES = libselparse.la 

libselparse_la_CPPFLAGS = \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libselparse_la_SOURCES = \
	ipmi-sel-parse.c \
	ipmi-sel-parse-common.c \
	ipmi-sel-parse-string.c \
	ipmi-sel-parse-string-dell.c \
	ipmi-sel-parse-string-fujitsu.c \
	ipmi-sel-parse-string-intel.c \
	ipmi-sel-parse-string-intel-node-manager.c \
	ipmi-sel-parse-string-inventec.c \
	ipmi-sel-parse-string-quanta.c \
	ipmi-sel-parse-string-sun.c \
	ipmi-sel-parse-util.c
