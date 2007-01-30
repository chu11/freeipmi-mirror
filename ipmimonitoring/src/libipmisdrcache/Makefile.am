##*****************************************************************************
## $Id: Makefile.am,v 1.1 2007-01-30 21:52:57 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

include_HEADERS = ipmi_sdr_cache.h

lib_LTLIBRARIES = libipmisdrcache.la 

libipmisdrcache_la_SOURCES = ipmi_sdr_cache.c

libipmisdrcache_la_CFLAGS = -I$(srcdir)/../../../common/src \
			    -I$(srcdir)/../../../libfreeipmi/include

libipmisdrcache_la_LIBADD = ../../../common/src/libllnlcommon.la \
			    ../../../libfreeipmi/src/libfreeipmi.la \
			    ../../../common/src/libfreeipmiportability.la
			    
../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:

