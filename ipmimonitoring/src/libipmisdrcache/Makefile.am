##*****************************************************************************
## $Id: Makefile.am,v 1.2 2007-02-06 17:25:33 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = ipmi_sdr_cache.h

noinst_LTLIBRARIES = libipmisdrcache.la 

libipmisdrcache_la_SOURCES = ipmi_sdr_cache.c

libipmisdrcache_la_CFLAGS = -I$(srcdir)/../../../common/src \
			    -I$(srcdir)/../../../libfreeipmi/include

force-dependency-check:

