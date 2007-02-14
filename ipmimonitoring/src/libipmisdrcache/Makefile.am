##*****************************************************************************
## $Id: Makefile.am,v 1.3 2007-02-14 19:36:39 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = ipmi_sdr_cache.h

noinst_LTLIBRARIES = libipmisdrcache.la 

libipmisdrcache_la_SOURCES = ipmi_sdr_cache.c

libipmisdrcache_la_CFLAGS = -I$(srcdir)/../../../common/src \
			    -I$(srcdir)/../../../libfreeipmi/include

libipmisdrcache_la_CPPFLAGS = \
	-D_REENTRANT

force-dependency-check:

