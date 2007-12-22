##*****************************************************************************
## $Id: Makefile.am,v 1.1.2.1 2007-12-22 19:41:15 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi_sdr_cache_common.h \
	ipmi_sdr_cache_defs.h

noinst_LTLIBRARIES = libsdrcache.la 

libsdrcache_la_SOURCES = \
	ipmi_sdr_cache.c \
	ipmi_sdr_cache_common.c \
	ipmi_sdr_cache_create.c \
	ipmi_sdr_cache_delete.c \
	ipmi_sdr_cache_read.c

libsdrcache_la_CPPFLAGS = -D_REENTRANT \
			  -I$(srcdir)/../../../common/src \
			  -I$(srcdir)/../../../libfreeipmi/include

force-dependency-check:

