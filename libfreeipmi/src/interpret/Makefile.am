##*****************************************************************************
## $Id: Makefile.am,v 1.3 2010-03-10 19:36:31 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-interpret-defs.h \
	ipmi-interpret-config.h \
	ipmi-interpret-trace.h \
	ipmi-interpret-util.h

noinst_LTLIBRARIES = libinterpret.la 

libinterpret_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libinterpret_la_SOURCES = \
	ipmi-interpret.c \
	ipmi-interpret-config.c \
	ipmi-interpret-util.c
