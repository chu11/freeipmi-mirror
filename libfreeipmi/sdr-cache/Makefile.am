##*****************************************************************************
## $Id: Makefile.am,v 1.11 2009-02-23 22:29:20 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sdr-cache-common.h \
	ipmi-sdr-cache-defs.h \
	ipmi-sdr-cache-trace.h \
	ipmi-sdr-cache-util.h

noinst_LTLIBRARIES = libsdrcache.la 

libsdrcache_la_CPPFLAGS = \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libsdrcache_la_SOURCES = \
	ipmi-sdr-cache.c \
	ipmi-sdr-cache-common.c \
	ipmi-sdr-cache-create.c \
	ipmi-sdr-cache-delete.c \
	ipmi-sdr-cache-read.c \
	ipmi-sdr-cache-util.c
