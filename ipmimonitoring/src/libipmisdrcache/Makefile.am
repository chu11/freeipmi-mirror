##*****************************************************************************
## $Id: Makefile.am,v 1.4 2007-10-15 16:07:00 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = ipmi_sdr_cache.h

noinst_LTLIBRARIES = libipmisdrcache.la 

libipmisdrcache_la_SOURCES = ipmi_sdr_cache.c

libipmisdrcache_la_CPPFLAGS = -D_REENTRANT \
				-I$(srcdir)/../../../common/src \
			    -I$(srcdir)/../../../libfreeipmi/include

force-dependency-check:

