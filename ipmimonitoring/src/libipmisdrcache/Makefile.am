##*****************************************************************************
## $Id: Makefile.am,v 1.4.6.1 2007-12-20 22:59:42 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi_sdr_cache.h \
	ipmi_sdr_cache_common.h \
	ipmi_sdr_cache_defs.h

noinst_LTLIBRARIES = libipmisdrcache.la 

libipmisdrcache_la_SOURCES = \
	ipmi_sdr_cache.c \
	ipmi_sdr_cache_common.c \
	ipmi_sdr_cache_create.c \
	ipmi_sdr_cache_delete.c \
	ipmi_sdr_cache_read.c

libipmisdrcache_la_CPPFLAGS = -D_REENTRANT \
				-I$(srcdir)/../../../common/src \
			    -I$(srcdir)/../../../libfreeipmi/include

force-dependency-check:

