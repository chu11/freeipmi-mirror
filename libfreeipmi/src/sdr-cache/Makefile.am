##*****************************************************************************
## $Id: Makefile.am,v 1.1.2.3 2007-12-23 02:03:11 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sdr-cache-common.h \
	ipmi-sdr-cache-defs.h

noinst_LTLIBRARIES = libsdrcache.la 

libsdrcache_la_SOURCES = \
	ipmi-sdr-cache.c \
	ipmi-sdr-cache-common.c \
	ipmi-sdr-cache-create.c \
	ipmi-sdr-cache-delete.c \
	ipmi-sdr-cache-read.c

libsdrcache_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT


